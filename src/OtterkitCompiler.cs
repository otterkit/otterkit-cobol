using System.Diagnostics;
using System.Text;
using OtterkitLibrary;

namespace Otterkit;
public static class OtterkitCompiler
{
    static Encoding encoding = Encoding.UTF8;
    static Memory<byte> _EXT_TEST = External.Resolver("EXT-TEST", 10);
    static Memory<byte> _EXT_TEST_2 = External.Resolver("EXT-TEST", 10);
    static Memory<byte> _EXT_TEST_NEW = External.Resolver("EXT-TEST-NEW", 20);
    static Memory<byte> _EXT_TEST_NEW_2 = External.Resolver("EXT-TEST-NEW", 20);

    public static void Main(string[] args)
    {
        "TEST1"u8.CopyTo(_EXT_TEST.Span);
        Console.WriteLine(encoding.GetString(_EXT_TEST_2.Span));


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
            int index = 0;
            string type = "app";
            string name = "OtterkitExport";
            foreach (string argument in args)
            {
                index++;
                switch (argument)
                {
                    case "app":
                    case "application":
                        type = "app";
                        break;

                    case "mod":
                    case "module":
                        type = "mod";
                        break;
                    
                    case "-n":
                    case "--name":
                        name = argument;
                        break;
                }
            }

            CallDotNetCompiler(args[0], type, name);
        }

        if (args[0].Equals("build"))
        {
            int index = 0;
            string entryPoint = "main.cob";
            string sourceFormat = "fixed";
            int columnLength = 80;

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
                        entryPoint = args[index];
                        break;

                    case "-cl":
                    case "--Columns":
                        columnLength = int.Parse(args[index]);
                        break;
                    // --Fixed meaning Fixed Format
                    case "--Fixed":
                        sourceFormat = "fixed";
                        break;
                    // --Free meaning Free Format
                    case "--Free":
                        sourceFormat = "free";
                        break;
                }
            }

            List<string> sourceLines = ReadAndProcessFile(entryPoint, sourceFormat, columnLength);
            List<Token> tokens = OtterkitLexer.Tokenize(sourceLines);
            List<Token> classified = Token.fromValue(tokens);
            List<Token> analized = OtterkitAnalyzer.Analyze(classified, entryPoint);
            OtterkitCodegen.Generate(analized, entryPoint);
        }
    }

    private static void CallDotNetCompiler(string operation, params string[] options)
    {
        string arguments = "";
        if (operation.Equals("new"))
        {
            string type = options[0] switch
            {
                "app" => "otterkit-export",
                "mod" => "otterkit-module-export",
                _ => "app"
            };

            string OtterkitConfig = $$"""
            {
                "$schema": "https://raw.githubusercontent.com/otterkit/otterkit/unfinished-codegen/src/schema.json",
                "author": "Project Author",
                "name": "{{options[1]}}",
                "id": "MyCompany.{{options[1]}}",
                "description": "Description of the project's purpose",
                "tags": ["COBOL"],
                "metadata": {
                    "entryPoint": "main.cob#main",
                    "type": "{{(options[0].Equals("app")?"application":"module")}}"
                },
                "license": "License URL"
            }
            """;

            Directory.CreateDirectory(".otterkit");
            Directory.SetCurrentDirectory(".otterkit");

            File.WriteAllText("OtterkitConfig.json", OtterkitConfig);
            arguments = $"new {type} -n {options[1]} --force";
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

        Directory.SetCurrentDirectory("..");
    }

    private static List<string> ReadAndProcessFile(string fileName, string sourceFormat, int columnLength)
    {
        if (!File.Exists(fileName))
        {
            ErrorHandler.Compiler.Report("Otterkit compiler error: File Not Found");
            ErrorHandler.Compiler.Report($"The compiler was not able not find the file: {fileName}");
            Environment.Exit(1);
        }

        string[] readLines = File.ReadAllLines(fileName);
        List<string> processedLines = new();
        foreach (string line in readLines)
        {
            if (sourceFormat == "fixed")
            {
                string currentLine = line;
                if (currentLine.Length >= columnLength)
                {
                    // Removes everything after the max column length
                    currentLine = currentLine.Substring(0, columnLength);
                }

                // Removes the sequence number area
                currentLine = currentLine.PadRight(6).Substring(6);

                if (currentLine.StartsWith("*"))
                {
                    // Removes all fixed format comment lines
                    currentLine = "";
                }

                processedLines.Add(currentLine);
            }
            if (sourceFormat == "free")
            {
                string currentLine = line;
                int commentIndex = currentLine.IndexOf("*>");
                if (commentIndex > -1)
                {
                    // Removes all free format comments
                    currentLine = currentLine.Substring(0, commentIndex);
                }

                processedLines.Add(currentLine);
            }
        }
        return processedLines;
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