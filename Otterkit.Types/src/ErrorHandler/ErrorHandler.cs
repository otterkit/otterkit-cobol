namespace Otterkit.Types;

public readonly ref partial struct ErrorHandler
{
    public static ErrorType SuppressedError { private get; set; }
    public static bool HasOccurred = false;
    public static bool CrashOnError = false;

    private readonly ErrorType ErrorType;
    private readonly ConsoleColor ConsoleColor;


    public ErrorHandler(ErrorType errorType, ConsoleColor consoleColor)
    {
        ErrorType = errorType;
        ConsoleColor = consoleColor;
    }
}
