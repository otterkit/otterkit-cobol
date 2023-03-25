namespace Otterkit;

public readonly ref partial struct Error
{
    private readonly static ConsoleColor Green = ConsoleColor.Green;
    private readonly static ConsoleColor DarkGray = ConsoleColor.DarkGray;

    private readonly ErrorType ErrorType;
    private readonly ConsoleColor ConsoleColor;

    public Error(ErrorType errorType, ConsoleColor consoleColor)
    {
        ErrorType = errorType;
        ConsoleColor = consoleColor;
    }
}
