namespace Otterkit;

public static class OtterkitCompiler
{
    private static string fileName = "main.cob";
    private static string sourceFormat = "fixed";
    public static void Main(string[] args)
    {
        if (args.Length <= 1)
        {
            DisplayHelpMessage();
            Environment.Exit(0);
        }

        ProcessArguments(args);
        List<string> sourceLines = ReadAndProcessFile(fileName, sourceFormat);
        OtterkitLexer.Tokenize(sourceLines);
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
                // --Fixed meaning Fixed Format
                case "--Fixed":
                    sourceFormat = "fixed";
                    break;
                // --Free meaning Free Format
                case "--Free":
                    sourceFormat = "free";
                    break;
                default: break;
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
            if (line.Length >= 6 && sourceFormat == "fixed")
            {
                processedLines.Add(line.Substring(6));
            }
            else
            {
                processedLines.Add(line);
            }
        }
        return processedLines;
    }

    private static void DisplayHelpMessage()
    {
        Console.WriteLine("\nOtterkit COBOL Compiler            ");
        Console.WriteLine("<Copyright 2022 - Otterkit Project>\n");
        Console.WriteLine("Command line options:                ");
        Console.WriteLine("  help - Displays this help message  ");
        Console.WriteLine("  file <filename> - Compile file     ");
        Console.WriteLine("\n");
    }
}