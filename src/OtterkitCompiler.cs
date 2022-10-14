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
            if (sourceFormat == "fixed")
            {   
                string currentLine = line.PadRight(7);
                if (currentLine.Length >= maxColumnLength)
                {
                    // Removes everything after the max column length
                    currentLine = currentLine.Substring(0, maxColumnLength);
                }

                if (currentLine[6].ToString() == "*")
                {   
                    //Removes all comment lines
                    currentLine = "";
                }

                // Removes the sequence number area
                currentLine = currentLine.PadRight(6).Substring(6);
                processedLines.Add(currentLine);
            }
            if (sourceFormat == "free")
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