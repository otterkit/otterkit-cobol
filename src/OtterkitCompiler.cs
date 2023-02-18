using System.Diagnostics;

namespace Otterkit;

public record Options
{
    public required string Name;
    public required string Type;
    public required BuildType BuildMode;
    public required int ColumnLength;
    public required string EntryPoint;
    public required string SourceFormat;
    public required List<string> FileNames;
}

public static class OtterkitCompiler
{
    internal static Options Options = new()
    {
        Name = "OtterkitExport",
        Type = "app",
        BuildMode = BuildType.BuildOnly,
        EntryPoint = "main.cob",
        SourceFormat = "fixed",
        ColumnLength = 80,
        FileNames = new()
    };

    public static void Main(string[] args)
    {
        if (args.Length <= 1 || args[0].Equals("-h") || args[0].Equals("--help"))
        {
            DisplayHelpMessage();
            return;
        }

        if (args[0].Equals("new"))
        {
            CommandLineArguments(args);
            return;
        }

        if (args[0].Equals("build"))
        {
            CommandLineArguments(args);
            return;
        }

        CommandLineArguments(args);
    }

    private static void CommandLineArguments(string[] args)
    {
        // Index should always be +1 the true index:
        // This makes it easier to get the next item after
        // the command line argument, like the entry point file.
        if (args[0].Equals("new"))
        {
            var index = 0;
            foreach (string argument in args)
            {
                index++;
                switch (argument)
                {
                    case "app":
                        Options.Type = "app";
                        break;

                    case "module":
                        Options.Type = "mod";
                        break;
                    
                    case "-n":
                    case "--name":
                        Options.Name = args[index];
                        break;
                }
            }

            CallDotnetCompiler(args[0]);
        }

        if (args[0].Equals("build"))
        {
            var index = 0;
            foreach (string argument in args)
            {
                index++;
                switch (argument)
                {
                    case "-h":
                    case "--help":
                        DisplayHelpMessage();
                        Environment.Exit(0);
                        break;

                    case "-e":
                    case "--entry":
                        Options.EntryPoint = args[index];
                        break;

                    case "-cl":
                    case "--columns":
                        Options.ColumnLength = int.Parse(args[index]);
                        break;

                    case "-p":
                    case "--parse":
                        Options.BuildMode = BuildType.ParseOnly;
                        break;

                    case "-p:tokens":
                    case "--parse:tokens":
                        Options.BuildMode = BuildType.PrintTokens;
                        break;

                    case "-p:symbols":
                    case "--parse:symbols":
                        Options.BuildMode = BuildType.PrintSymbols;
                        break;

                    case "-r":
                    case "--run":
                        Options.BuildMode = BuildType.BuildAndRun;
                        break;

                    // --Fixed meaning Fixed Format
                    case "--fixed":
                        Options.SourceFormat = "fixed";
                        break;
                    // --Free meaning Free Format
                    case "--free":
                        Options.SourceFormat = "free";
                        break;
                }
            }

            List<string> preprocessedLines = Preprocessor.Preprocess(ReadSourceFile());
            List<Token> tokens = Lexer.Tokenize(preprocessedLines);
            List<Token> classified = Token.FromValue(tokens);
            List<Token> analized = Analyzer.Analyze(classified, Options.EntryPoint);

            if (Options.BuildMode is BuildType.ParseOnly)
            {
                if (!ErrorHandler.Error) ErrorHandler.SuccessfulParsing();
            }

            if (Options.BuildMode is BuildType.PrintTokens)
            {
                bool colorToggle = true;

                foreach (var token in analized)
                {
                    if (colorToggle)
                    {
                        Console.ForegroundColor = ConsoleColor.Gray;
                    }
                    else
                    {
                        Console.ForegroundColor = ConsoleColor.DarkGray;
                    }

                    colorToggle = !colorToggle;
                    Console.WriteLine(token);
                }

                if (!ErrorHandler.Error) ErrorHandler.SuccessfulParsing();
            }

            if (Options.BuildMode is BuildType.PrintSymbols)
            {
                bool colorToggle = true;

                foreach (var symbol in SymbolTable.Symbols)
                {
                    if (colorToggle)
                    {
                        Console.ForegroundColor = ConsoleColor.Gray;
                    }
                    else
                    {
                        Console.ForegroundColor = ConsoleColor.DarkGray;
                    }

                    colorToggle = !colorToggle;
                    Console.WriteLine($"{symbol.Key}  =>  {symbol.Value.SymbolType}");
                }

                if (!ErrorHandler.Error) ErrorHandler.SuccessfulParsing();
            }

            if (Options.BuildMode is BuildType.BuildOnly)
            {
                Codegen.Generate(analized, Options.EntryPoint);

                Directory.CreateDirectory(".otterkit/Build");
                CallDotnetCompiler("build");
            }
            
            if (Options.BuildMode is BuildType.BuildAndRun)
            {
                Codegen.Generate(analized, Options.EntryPoint);

                Directory.CreateDirectory(".otterkit/Build");
                CallDotnetCompiler("run");
            }
        }
    }

    private static void CallDotnetCompiler(string operation, bool output = true)
    {
        var arguments = string.Empty;
        if (operation.Equals("new"))
        {
            var type = Options.Type switch
            {
                "app" => "otterkit-export",
                "mod" => "otterkit-module-export",
                _ => "otterkit-export"
            };

            var otterkitConfig = $$"""
            {
                "$schema": "https://raw.githubusercontent.com/otterkit/otterkit/unfinished-codegen/src/schema.json",
                "author": "Project Author",
                "name": "{{Options.Name}}",
                "id": "MyCompany.{{Options.Name}}",
                "description": "Description of the project's purpose",
                "tags": ["COBOL"],
                "metadata": {
                    "entryPoint": "main.cob#main",
                    "type": "{{(Options.Type.Equals("app")?"application":"module")}}"
                },
                "license": "License URL"
            }
            """;

            Directory.CreateDirectory(".otterkit");

            File.WriteAllText(".otterkit/OtterkitConfig.json", otterkitConfig);
            arguments = $"new {type} -n OtterkitExport -o .otterkit --force";
        }

        if (operation.Equals("run"))
        {
            CallDotnetCompiler("build", false);

            var binaryName = ".otterkit/Build/OtterkitExport";

            if (OperatingSystem.IsWindows())
            {
                binaryName = @".otterkit\Build\OtterkitExport.exe";
            }

            using Process otterkitExport = new();
            otterkitExport.StartInfo.FileName = binaryName;
            otterkitExport.StartInfo.UseShellExecute = false;
            otterkitExport.Start();

            otterkitExport.WaitForExit();

            return;
        }

        if (operation.Equals("build"))
        {
            string runtimeIdentifier = string.Empty;
            if (OperatingSystem.IsLinux())
            {
                runtimeIdentifier = "linux-x64";
            }

            if (OperatingSystem.IsMacOS())
            {
                runtimeIdentifier = "osx-x64";
            }

            if (OperatingSystem.IsWindows())
            {
                runtimeIdentifier = "win-x64";
            }

            arguments = $"""
            publish .otterkit/OtterkitExport/OtterkitExport.csproj -r {runtimeIdentifier} -c Release -o .otterkit/Build
            """;
        }

        using (Process dotnet = new())
        {
            dotnet.StartInfo.FileName = "dotnet";
            dotnet.StartInfo.Arguments = arguments;
            dotnet.StartInfo.UseShellExecute = false;
            dotnet.StartInfo.RedirectStandardOutput = true;
            dotnet.Start();

            if (output)
            {
                Console.Write(dotnet.StandardOutput.ReadToEnd());
            }

            dotnet.WaitForExit();
        }

        if (Directory.Exists(".otterkit/Build/OtterkitMath"))
        {
            Directory.Delete(".otterkit/Build/OtterkitMath", true);
        }
    }

    private static List<string> ReadSourceFile()
    {
        const string eofMarker = "       >>IMP-EOF";
        if (!File.Exists(Options.EntryPoint))
        {
            ErrorHandler.Compiler.Report("Otterkit compiler error: File Not Found");
            ErrorHandler.Compiler.Report($"The compiler was not able not find the file: {Options.EntryPoint}");
            Environment.Exit(1);
        }

        var sourceLines = new List<string>();

        foreach (var line in File.ReadLines(Options.EntryPoint))
        {
            sourceLines.Add(line);
        }

        sourceLines.Add(eofMarker);

        var allSourceFiles = Directory.EnumerateFiles(Directory.GetCurrentDirectory(), "*.cob", SearchOption.AllDirectories);
        var excludeEntryPoint = Directory.EnumerateFiles(Directory.GetCurrentDirectory(), Options.EntryPoint);
        var sourcesWithoutEntryPoint = allSourceFiles.Except(excludeEntryPoint);

        foreach (var file in sourcesWithoutEntryPoint)
        {
            foreach (var line in File.ReadLines(file))
            {
                sourceLines.Add(line);
            }
            
            sourceLines.Add(eofMarker);
            Options.FileNames.Add(file);
        }

        return sourceLines;
    }

    private static void DisplayHelpMessage()
    {
        const string helpMessage = """

        Otterkit COBOL Compiler
        Copyright Otterkit Project 2023
        Licensed under Apache 2.0

        Command line options:
            -h --help           : Displays this help message

        New command usage:
            otterkit new <options>

        New command options:
            app                 : Create a new executable COBOL application
            module              : Create a new COBOL module library

        Build command usage:
            otterkit build <options>

        Build command options:
            -e --entry          : Specify the entry point for the project
            -cl --columns       : Specify the max column length (default is 80)
            -p --parse          : Use the "Parse Only" build mode
            -r --run            : Use the "Build & Run" build mode
            --fixed             : Use fixed source format
            --free              : Use free source format 
                                    (-cl has no effect on free format)

        """;

        Console.WriteLine(helpMessage);
    }
}