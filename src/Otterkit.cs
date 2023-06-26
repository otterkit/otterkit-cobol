using System.Diagnostics;
using Otterkit.Analyzers;
using Otterkit.CodeGenerators;
using Otterkit.Tokenizers;
using Otterkit.Workspaces;
using Otterkit.Types;

namespace Otterkit;

public static class Otterkit
{
    public static void Main(string[] args)
    {
        if (args.Length < 1 || args[0].Equals("-h") || args[0].Equals("--help"))
        {
            DisplayHelpMessage();
            return;
        }

        if (args[0] is "new" or "build" or "tools")
        {
            TryLoadProject();

            CommandLineArguments(args);
            return;
        }

        DisplayHelpMessage();
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
                    case "application":
                        CompilerOptions.Output = OutputType.Application;
                        break;

                    case "lib":
                    case "library":
                        CompilerOptions.Output = OutputType.Library;
                        break;

                    case "-n":
                    case "--name":
                        CompilerOptions.Name = args[index];
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
                        CompilerOptions.Main = args[index];
                        break;

                    case "-cl":
                    case "--columns":
                        CompilerOptions.Columns = int.Parse(args[index]);
                        break;

                    case "-t":
                    case "--tokenize":
                        CompilerOptions.Mode = BuildType.LexOnly;
                        break;

                    case "-p":
                    case "--parse":
                        CompilerOptions.Mode = BuildType.ParseOnly;
                        break;

                    case "-p:tokens":
                    case "--parse:tokens":
                        CompilerOptions.Mode = BuildType.PrintTokens;
                        break;

                    case "-g":
                    case "--generate":
                        CompilerOptions.Mode = BuildType.GenerateOnly;
                        break;

                    case "-r":
                    case "--run":
                        CompilerOptions.Mode = BuildType.BuildAndRun;
                        break;

                    // --Fixed meaning Fixed Format
                    case "--fixed":
                        CompilerOptions.Format = SourceFormat.Fixed;
                        break;
                    // --Free meaning Free Format
                    case "--free":
                        CompilerOptions.Format = SourceFormat.Free;
                        break;

                    case "--enable-extensions":
                        CompilerContext.ExtensionsEnabled = true;
                        break;

                    case "--crash-on-error":
                        ErrorHandler.CrashOnError = true;
                        break;
                }
            }

            OtterkitMain();
        }

        if (args[0].Equals("tools"))
        {
            HandleToolsCommands(args);
        }
    }

    private static void HandleToolsCommands(string[] args)
    {
        for (var i = 1; i < args.Length; i++)
        {
            var argument = args[i];

            var index = i + 1;
            switch (argument)
            {
                case "--generate-exception-index":
                    Tools.GenerateExceptionIndex();
                    break;

                case "--generate-unicode-data":

                    Console.WriteLine("Generating Unicode Data...");

                    if (index > args.Length - 1 || args[index].StartsWith("--"))
                    {
                        Console.WriteLine("No path specified.");
                        goto default;
                    }

                    Console.WriteLine($"Tools path: {args[index]}");

                    var path = args[index];

                    i++;

                    Tools.GenerateBasicMultilingualPlane(path);
                    break;

                default:
                    Console.WriteLine($"Invalid command: {argument}");
                    return;
            }
        }
    }

    private static void TryLoadProject()
    {
        if (!File.Exists("package.otterproj")) return;

        var otterproj = ProjectLoader.ReadOtterproj("package.otterproj").AwaitResult();

        if (!ProjectLoader.TryLoadProject(otterproj))
        {
            ErrorHandler.StopCompilation("project");
        }
    }

    private static void OtterkitMain()
    {
        var tokenizedLines = Tokenizer.Tokenize(CompilerOptions.Main);

        var classified = Token.ClassifyTokens(tokenizedLines);

        if (CompilerOptions.Mode is BuildType.LexOnly)
        {
            PrintTokens();
            return;
        }

        var analized = Analyzer.Analyze(classified);

        if (CompilerOptions.Mode is BuildType.ParseOnly)
        {
            if (!ErrorHandler.HasOccurred) ErrorHandler.SuccessfulParse();
        }

        if (CompilerOptions.Mode is BuildType.PrintTokens)
        {
            PrintTokens();
        }

        TokenHandling.Index = 0;

        if (CompilerOptions.Mode is BuildType.GenerateOnly)
        {
            CodeGenerator.Generate(analized, CompilerOptions.Main);
        }

        if (CompilerOptions.Mode is BuildType.BuildOnly)
        {
            CodeGenerator.Generate(CompilerContext.SourceTokens, CompilerOptions.Main);

            Directory.CreateDirectory(".otterkit/Build");
            CallDotnetCompiler("build");
        }

        if (CompilerOptions.Mode is BuildType.BuildAndRun)
        {
            CodeGenerator.Generate(CompilerContext.SourceTokens, CompilerOptions.Main);

            Directory.CreateDirectory(".otterkit/Build");
            CallDotnetCompiler("run");
        }
    }

    private static void CallDotnetCompiler(string operation, bool output = true)
    {
        var arguments = string.Empty;
        if (operation.Equals("new"))
        {
            var templateType = CompilerOptions.Output switch
            {
                OutputType.Application => "otterkit-app",
                OutputType.Library => "otterkit-lib",
                _ => "otterkit-app"
            };

            arguments = $"new {templateType} --force";

            if (Directory.Exists(".otterkit") && File.Exists("package.otterproj"))
            {
                Console.WriteLine("A project already exists in this directory.");
                Console.Write("Delete project and create a new one? ['y' or 'n'] ");

                var character = Console.ReadKey().KeyChar;

                Console.WriteLine();

                if (character is not 'y') return;

                Directory.Delete(".otterkit", true);
                File.Delete("package.otterproj");
            }
        }

        if (operation.Equals("run"))
        {
            CallDotnetCompiler("build", false);

            var binaryName = ".otterkit/Build/OtterkitExport";

            if (OperatingSystem.IsWindows())
            {
                binaryName = @".otterkit\Build\OtterkitExport.exe";
            }

            using Process binary = new();
            binary.StartInfo.FileName = binaryName;
            binary.StartInfo.UseShellExecute = false;
            binary.Start();

            binary.WaitForExit();

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
            publish .otterkit/Artifacts/OtterkitExport.csproj -r {runtimeIdentifier} -c Release -o .otterkit/Build
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
    }

    private static void PrintTokens()
    {
        var colorToggle = true;

        foreach (var token in CompilerContext.SourceTokens)
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

        Console.ResetColor();

        if (!ErrorHandler.HasOccurred) ErrorHandler.SuccessfulParse();
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
            
            Notes:
                --columns has no effect on free format

        """;

        Console.WriteLine(helpMessage);
    }
}
