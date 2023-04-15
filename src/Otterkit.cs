using System.Diagnostics;
using Otterkit.SourceAnalyzer;

namespace Otterkit;

public static class Otterkit
{
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
                        CompilerOptions.Output = OutputType.Application;
                        break;

                    case "module":
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
                        CompilerOptions.EntryPoint = args[index];
                        break;

                    case "-cl":
                    case "--columns":
                        CompilerOptions.ColumnLength = int.Parse(args[index]);
                        break;

                    case "-p":
                    case "--parse":
                        CompilerOptions.BuildMode = BuildType.ParseOnly;
                        break;

                    case "-p:tokens":
                    case "--parse:tokens":
                        CompilerOptions.BuildMode = BuildType.PrintTokens;
                        break;

                    case "-r":
                    case "--run":
                        CompilerOptions.BuildMode = BuildType.BuildAndRun;
                        break;

                    // --Fixed meaning Fixed Format
                    case "--fixed":
                        CompilerOptions.SourceFormat = SourceFormat.Fixed;
                        break;
                    // --Free meaning Free Format
                    case "--free":
                        CompilerOptions.SourceFormat = SourceFormat.Free;
                        break;
                }
            }

            var preprocessedLines = Preprocessor.Preprocess(CompilerOptions.EntryPoint);

            var classified = Token.ClassifyFromValue(preprocessedLines);

            var analized = Analyzer.Analyze(classified);

            if (CompilerOptions.BuildMode is BuildType.ParseOnly)
            {
                if (!Error.HasOccurred) Error.SuccessfulParse();
            }

            if (CompilerOptions.BuildMode is BuildType.PrintTokens)
            {
                var colorToggle = true;

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

                if (!Error.HasOccurred) Error.SuccessfulParse();
            }

            if (CompilerOptions.BuildMode is BuildType.BuildOnly)
            {
                Codegen.Generate(CompilerContext.SourceTokens, CompilerOptions.EntryPoint);

                Directory.CreateDirectory(".otterkit/Build");
                CallDotnetCompiler("build");
            }
            
            if (CompilerOptions.BuildMode is BuildType.BuildAndRun)
            {
                Codegen.Generate(CompilerContext.SourceTokens, CompilerOptions.EntryPoint);

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
            var templateType = CompilerOptions.Output switch
            {
                OutputType.Application => "otterkit-export",
                OutputType.Library => "otterkit-module-export",
                _ => "otterkit-export"
            };

            Directory.CreateDirectory(".otterkit");

            arguments = $"new {templateType} -n OtterkitExport -o .otterkit --force";
        }

        if (operation.Equals("run"))
        {
            CallDotnetCompiler("build", false);

            var binaryName = ".otterkit/Build/OtterkitExport";

            if (OperatingSystem.IsWindows())
            {
                binaryName = @".otterkit\Build\OtterkitExport.exe";
            }

            using Process outputBinary = new();
            outputBinary.StartInfo.FileName = binaryName;
            outputBinary.StartInfo.UseShellExecute = false;
            outputBinary.Start();

            outputBinary.WaitForExit();

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