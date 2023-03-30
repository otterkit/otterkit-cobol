namespace Otterkit;

public readonly ref partial struct Error
{
    public static ErrorType SuppressedError { private get; set; }

    private readonly ErrorType ErrorType;
    private readonly ConsoleColor ConsoleColor;


    public Error(ErrorType errorType, ConsoleColor consoleColor)
    {
        ErrorType = errorType;
        ConsoleColor = consoleColor;
    }
}
