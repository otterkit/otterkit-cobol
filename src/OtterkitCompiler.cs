using System.Diagnostics;

namespace Otterkit;

public static class OtterkitCompiler
{
    private static string entryPoint = "main.cob";
    private static string sourceFormat = "fixed";
    private static int maxColumnLength = 80;
    public static void Main(string[] args)
    {
        if (args.Length <= 1)
        {
            DisplayHelpMessage();
            return;
        }

        if (args[0].Equals("new"))
        {
            CommandLineArguments(args);
            return;
        }

        CommandLineArguments(args);
        List<string> sourceLines = ReadAndProcessFile(entryPoint, sourceFormat);
        List<Token> tokens = OtterkitLexer.Tokenize(sourceLines);
        List<Token> classified = Token.fromValue(tokens);
        List<Token> analized = OtterkitAnalyzer.Analyze(classified, entryPoint);
        OtterkitCodegen.Generate(analized, entryPoint);
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
            string directory = ".otterkit";
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

                    case "-d":
                    case "--directory":
                        directory = argument;
                        break;
                    
                    case "-n":
                    case "--name":
                        name = argument;
                        break;
                }
            }

            CallDotNetCompiler(args[0], type, name, directory);
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

            arguments = $"new {type} -n {options[1]} -o {options[2]}";
        }

        using (Process dotnet = new Process())
        {
            dotnet.StartInfo.FileName = "dotnet";
            dotnet.StartInfo.Arguments = arguments;
            dotnet.StartInfo.UseShellExecute = false;
            dotnet.StartInfo.RedirectStandardOutput = true;
            dotnet.Start();

            Console.WriteLine(dotnet.StandardOutput.ReadToEnd());

            dotnet.WaitForExit();
        }
    }

    // private static void ProcessArguments(string[] args)
    // {
    //     // Index is always +1 the true index:
    //     // This makes it easier to get the next item,
    //     // after the command line argument, like the filename.
    //     var index = 0;
    //     foreach (string argument in args)
    //     {
    //         index += 1;
    //         switch (argument)
    //         {
    //             // -H meaning Help
    //             case "-h":
    //             case "--Help":
    //                 DisplayHelpMessage();
    //                 Environment.Exit(0);
    //                 break;

    //             case "-f":
    //             case "--File":
    //                 fileName = args[index];
    //                 break;

    //             case "-cl":
    //             case "--Columns":
    //                 maxColumnLength = int.Parse(args[index]);
    //                 break;
    //             // --Fixed meaning Fixed Format
    //             case "--Fixed":
    //                 sourceFormat = "fixed";
    //                 break;
    //             // --Free meaning Free Format
    //             case "--Free":
    //                 sourceFormat = "free";
    //                 break;
    //             default:
    //                 break;
    //         }

    //     }
    //     return;
    // }

    private static List<string> ReadAndProcessFile(string fileName, string sourceFormat)
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
                if (currentLine.Length >= maxColumnLength)
                {
                    // Removes everything after the max column length
                    currentLine = currentLine.Substring(0, maxColumnLength);
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
        Console.WriteLine("\nOtterkit COBOL Compiler           ");
        Console.WriteLine("<Copyright 2022 Otterkit Project>   ");
        Console.WriteLine("<Apache 2.0 license>              \n");
        Console.WriteLine("Command line options:             \n");
        
        Console.WriteLine("  -h --Help      : Displays this message      ");
        Console.WriteLine("  -f --File      : Compile specified file     ");
        Console.WriteLine("  -cl --Columns  : Specify max column length  ");
        Console.WriteLine("  --Fixed        : Use fixed source format    ");
        Console.WriteLine("  --Free         : Use free source format     ");

        Console.WriteLine("\n");
    }
}