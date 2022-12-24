using System.Diagnostics;
using System.Reflection;
using System.Text.Json;

namespace Otterkit;

public struct Options
{
    public string Name;
    public string Type;
    public string BuildMode;
    public string EntryPoint;
    public string SourceFormat;
    public int ColumnLength;
}

public static class OtterkitCompiler
{
    public static Options Options;
    public static JsonElement ParsingInfo;

    public static void Main(string[] args)
    {
        if (args.Length <= 1 || (args[0].Equals("-h") || args[0].Equals("--help")))
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
            Options.Type = "app";
            Options.Name = "OtterkitExport";

            int index = 0;
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
                        Options.Name = argument;
                        break;
                }
            }

            CallDotnetCompiler(args[0]);
        }

        if (args[0].Equals("build"))
        {
            Options.BuildMode = "BuildOnly";
            Options.EntryPoint = "main.cob";
            Options.SourceFormat = "fixed";
            Options.ColumnLength = 80;

            int index = 0;
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
                        Options.BuildMode = "ParseOnly";
                        break;

                    case "-r":
                    case "--run":
                        Options.BuildMode = "Build&Run";
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

            if (Options.BuildMode.Equals("ParseOnly"))
            {
                List<string> sourceLines = ReadSourceFile();
                List<string> preprocessedLines = Preprocessor.Preprocess(sourceLines);
                List<Token> tokens = Lexer.Tokenize(preprocessedLines);
                List<Token> classified = Token.fromValue(tokens);
                List<Token> analized = Analyzer.Analyze(classified, Options.EntryPoint);

                string format = $$"""
                {0,-5} {1,-3} {2,-35} {3,-16} {4,-10}
                """;
                foreach (Token token in analized)
                {
                    Console.WriteLine(format, token.line, token.column, token.value, token.type, token.context);
                }
            }

            if (Options.BuildMode.Equals("BuildOnly"))
            {
                List<string> sourceLines = ReadSourceFile();
                List<string> preprocessedLines = Preprocessor.Preprocess(sourceLines);
                List<Token> tokens = Lexer.Tokenize(preprocessedLines);
                List<Token> classified = Token.fromValue(tokens);
                List<Token> analized = Analyzer.Analyze(classified, Options.EntryPoint);
                Codegen.Generate(analized, Options.EntryPoint);

                Directory.CreateDirectory(".otterkit/Build");
                CallDotnetCompiler("build");
            }
            
            if (Options.BuildMode.Equals("Build&Run"))
            {
                List<string> sourceLines = ReadSourceFile();
                List<string> preprocessedLines = Preprocessor.Preprocess(sourceLines);
                List<Token> tokens = Lexer.Tokenize(preprocessedLines);
                List<Token> classified = Token.fromValue(tokens);
                List<Token> analized = Analyzer.Analyze(classified, Options.EntryPoint);
                Codegen.Generate(analized, Options.EntryPoint);

                Directory.CreateDirectory(".otterkit/Build");
                CallDotnetCompiler("run");
            }
        }
    }

    private static void CallDotnetCompiler(string operation, bool output = true)
    {
        string arguments = string.Empty;
        if (operation.Equals("new"))
        {
            string type = Options.Type switch
            {
                "app" => "otterkit-export",
                "mod" => "otterkit-module-export",
                _ => "app"
            };

            string OtterkitConfig = $$"""
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

            File.WriteAllText(".otterkit/OtterkitConfig.json", OtterkitConfig);
            arguments = $"new {type} -n OtterkitExport -o .otterkit --force";
        }

        if (operation.Equals("run"))
        {
            CallDotnetCompiler("build", false);

            using (Process otterkitExport = new Process())
            {
                otterkitExport.StartInfo.FileName = ".otterkit/Build/OtterkitExport";
                otterkitExport.StartInfo.UseShellExecute = true;
                otterkitExport.Start();

                otterkitExport.WaitForExit();
            }

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

        using (Process dotnet = new Process())
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
        if (!File.Exists(Options.EntryPoint))
        {
            ErrorHandler.Compiler.Report("Otterkit compiler error: File Not Found");
            ErrorHandler.Compiler.Report($"The compiler was not able not find the file: {Options.EntryPoint}");
            Environment.Exit(1);
        }

        List<string> sourceLines = File.ReadAllLines(Options.EntryPoint).ToList();

        // Get COBOL reserved keyword information for parsinginfo.json
        Assembly assembly = Assembly.GetCallingAssembly();
        Stream? stream = assembly.GetManifestResourceStream("Otterkit.parsinginfo.json");
        StreamReader reader = new System.IO.StreamReader(stream == null ? throw new ArgumentNullException() : stream);
        ParsingInfo = JsonSerializer.Deserialize<JsonElement>(reader.ReadToEnd());

        return sourceLines;
    }

    private static void DisplayHelpMessage()
    {
        string helpMessage = """

        Otterkit COBOL Compiler
        Copyright Otterkit Project 2022
        Licensed under Apache 2.0

        Command line options:
            -h --help           : Displays this help message

        New command usage:
            otterkit new <options>

        New command options:
            app                 : Create a new executable COBOL application
            module              : Create a new COBOL module library
            -n --name           : Specify the name for the project 
                                    (default is OtterkitExport)

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