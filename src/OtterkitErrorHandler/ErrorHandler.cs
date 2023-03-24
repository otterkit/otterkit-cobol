namespace Otterkit;

public ref struct Error
{
    private static ConsoleColor Green = ConsoleColor.Green;
    private static ConsoleColor DarkGray = ConsoleColor.DarkGray;

    private ErrorType ErrorType;
    private ConsoleColor ConsoleColor;

    public Error(ErrorType errorType, ConsoleColor consoleColor)
    {
        ErrorType = errorType;
        ConsoleColor = consoleColor;
    }

    private static void ColoredWrite(ConsoleColor consoleColor, string text)
    {
        Console.ForegroundColor = consoleColor;
        Console.Write(text);

        Console.ResetColor();
    }

    private static void ColoredWriteLine(ConsoleColor consoleColor, string text)
    {
        Console.ForegroundColor = consoleColor;
        Console.WriteLine(text);

        Console.ResetColor();
    }

    public static Error Build(ErrorType errorType, ConsoleColor consoleColor, int errorCode, string errorMessage)
    {
        Console.OutputEncoding = System.Text.Encoding.UTF8;

        ColoredWrite(consoleColor, $" {errorType} Error [COB{errorCode:D4}]");

        Console.WriteLine($": {errorMessage}");

        return new(errorType, consoleColor);
    }

    public Error WithSourceLine(Token token, string fileName, string? errorHelp = null)
    {
        string line = File.ReadLines(fileName).Skip(token.line - 1).Take(token.line).First();
        string error = new(' ', line.Length - token.value.Length);

        int count = line.TakeWhile(char.IsWhiteSpace).Count();
        error = error.Insert(token.column - count, new string('~', token.value.Length));
        
        ColoredWrite(DarkGray, "     ╭─/> [");

        Console.Write($"{fileName}:{token.line}:{token.column}");

        ColoredWriteLine(DarkGray, "]");

        ColoredWriteLine(DarkGray, "     │");

        ColoredWrite(DarkGray, $"{token.line,4} │  ");

        Console.WriteLine($"{line.TrimStart()}");

        ColoredWrite(DarkGray, "     │");

        ColoredWrite(ConsoleColor, $"  {error.TrimEnd()}");

        if (errorHelp is not null)
        {
            ColoredWrite(ConsoleColor, "/> ");

            Console.WriteLine(errorHelp);
        }

        if (errorHelp is null)
        {
            Console.WriteLine();
        }

        ColoredWriteLine(DarkGray, "     │");

        return this;
    }

    public Error WithNote(string noteMessage)
    {
        ColoredWrite(DarkGray, "     │  ");

        ColoredWrite(Green, "Note");

        Console.WriteLine($": {noteMessage}");

        ColoredWriteLine(DarkGray, "     │");

        return this;
    }

    public void CloseError()
    {
        ColoredWriteLine(DarkGray, " ────╯\n");
    }
}
