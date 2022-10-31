namespace Otterkit;

public static class OtterkitCompiler
{
    private static string fileName = "main.cob";
    private static string sourceFormat = "fixed";
    private static int maxColumnLength = 80;
    public static void Main(string[] args)
    {
        if (args.Length <= 1)
        {
            DisplayHelpMessage();
            Environment.Exit(0);
        }

        ProcessArguments(args);
        List<string> sourceLines = ReadAndProcessFile(fileName, sourceFormat);
        List<Token> tokens = OtterkitLexer.Tokenize(sourceLines);
        List<Token> classified = Token.fromValue(tokens);
        foreach (var item in classified)
        {
            Console.WriteLine("{0,4} {1,4} {2,16} {3} {4}", item.line, item.column, item.value, item.type, item.scope);
        }
    }

    private static void ProcessArguments(string[] args)
    {
        // Index is always +1 the true index:
        // This makes it easier to get the next item,
        // after the command line argument, like the filename.
        var index = 0;
        foreach (string argument in args)
        {
            index += 1;
            switch (argument)
            {
                // -H meaning Help
                case "-H":
                    DisplayHelpMessage();
                    Environment.Exit(0);
                    break;
                // -F meaning File
                case "-F":
                    fileName = args[index];
                    break;
                // -CL meaning Column Length
                case "-CL":
                    maxColumnLength = int.Parse(args[index]);
                    break;
                // --Fixed meaning Fixed Format
                case "--Fixed":
                    sourceFormat = "fixed";
                    break;
                // --Free meaning Free Format
                case "--Free":
                    sourceFormat = "free";
                    break;
                default:
                    break;
            }

        }
        return;
    }
    private static List<string> ReadAndProcessFile(string fileName, string sourceFormat)
    {
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
        Console.WriteLine("\nOtterkit COBOL Compiler                   ");
        Console.WriteLine("<Copyright 2022 Otterkit Project>           ");
        Console.WriteLine("<Apache 2.0 license>                      \n");
        Console.WriteLine("Command line options:                     \n");
        Console.WriteLine("  -H           : Displays this message      ");
        Console.WriteLine("  -F filename  : Compile specified file     ");
        Console.WriteLine("  -CL integer  : Specify max column length  ");
        Console.WriteLine("  --Fixed      : Use fixed source format    ");
        Console.WriteLine("  --Free       : Use free source format     ");
        Console.WriteLine("\n");
    }
}