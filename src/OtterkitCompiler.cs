namespace Otterkit;

public static class OtterkitCompiler
{
    private static string fileName = "main.cob";
    public static void Main(string[] args)
    {
        if (args.Length <= 1)
        {
            DisplayHelpMessage();
            Environment.Exit(0);
        }
        ProcessArguments(args);
        Console.WriteLine(fileName);
    }

    private static void ProcessArguments(string[] args)
    {
        // Index is always +1 the true index:
        // This makes it easier to get the next item
        // after the command line argument, like the filename.
        var index = 0;
        foreach (string argument in args)
        {
            index += 1;
            if (argument == "help")
            {
                DisplayHelpMessage();
                Environment.Exit(0);
            }
            if (argument == "file")
            {
                fileName = args[index];
            }
        }
        return;
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