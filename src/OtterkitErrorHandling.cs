namespace Otterkit;

public static class ErrorHandler
{
    internal static bool HasError = false;

    public static class Compiler
    {
        public static void Report(string error)
        {
            Console.ForegroundColor = ConsoleColor.Red;
            Console.WriteLine(error);
            Console.ResetColor();
        }
    }

    public static void SuccessfulParsing()
    {
        var filesCount = CompilerContext.FileNames.Count;
        var isPlural = filesCount > 1 ? "s" : "";

        Console.ForegroundColor = ConsoleColor.Blue;
        Console.WriteLine($"Analyzed {filesCount} file{isPlural}, no errors found! \n");
        Console.ResetColor();
    }

    public static void Terminate(string errorType)
    {
        Console.ForegroundColor = ConsoleColor.Red;
        Console.WriteLine($"Compilation process cancelled due to a {errorType} error");
        Console.ResetColor();
        Environment.Exit(1);
    }
}