using System.Diagnostics;
using System.Reflection;
using System.Text.Json;

namespace Otterkit;

public struct Options
{
    public string Name;
    public string Type;
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
                    case "application":
                        Options.Type = "app";
                        break;

                    case "mod":
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
                    case "--Help":
                        DisplayHelpMessage();
                        Environment.Exit(0);
                        break;

                    case "-e":
                    case "--Entry":
                        Options.EntryPoint = args[index];
                        break;

                    case "-cl":
                    case "--Columns":
                        Options.ColumnLength = int.Parse(args[index]);
                        break;
                    // --Fixed meaning Fixed Format
                    case "--Fixed":
                        Options.SourceFormat = "fixed";
                        break;
                    // --Free meaning Free Format
                    case "--Free":
                        Options.SourceFormat = "free";
                        break;
                }
            }

            List<string> sourceLines = ReadSourceFile();
            List<string> preprocessedLines = Preprocessor.Preprocess(sourceLines);
            List<Token> tokens = Lexer.Tokenize(preprocessedLines);
            List<Token> classified = Token.fromValue(tokens);
            List<Token> analized = Analyzer.Analyze(classified, Options.EntryPoint);
            Codegen.Generate(analized, Options.EntryPoint);
        }
    }

    private static void CallDotnetCompiler(string operation)
    {
        string arguments = "";
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

        using (Process dotnet = new Process())
        {
            dotnet.StartInfo.FileName = "dotnet";
            dotnet.StartInfo.Arguments = arguments;
            dotnet.StartInfo.UseShellExecute = false;
            dotnet.StartInfo.RedirectStandardOutput = true;
            dotnet.Start();

            Console.Write(dotnet.StandardOutput.ReadToEnd());

            dotnet.WaitForExit();
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
            -h --Help           : Displays this help message

        New command usage:
            otterkit new <options>

        New command options:
            app application     : Create a new executable COBOL application
            mod module          : Create a new COBOL module library
            -n --Name           : Specify the name for the project 
                                    (default is OtterkitExport)

        Build command usage:
            otterkit build <options>

        Build command options:
            -e --Entry          : Specify the entry point for the project
            -cl --Columns       : Specify the max column length (default is 80)
            --Fixed             : Use fixed source format
            --Free              : Use free source format 
                                    (-cl has no effect on free format)

        """;

        Console.WriteLine(helpMessage);
    }
}