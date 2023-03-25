namespace Otterkit;

public readonly ref partial struct Error
{
    private static void ColoredWrite(ConsoleColor consoleColor, string text)
    {
        Console.ForegroundColor = consoleColor;
        Console.Write(text);
    }

    private static ConsoleColor ResetColor()
    {
        Console.ResetColor();

        return Console.ForegroundColor;
    }

    private static void Separator()
    {
        ColoredWrite(DarkGray, "     │\n");
    }

    private static void ShowFileInformation(char joiningChar, Token token, string fileName)
    {
        // ╭
        ColoredWrite(DarkGray     , $"     {joiningChar}─/> [");
        ColoredWrite(ResetColor() , $"{fileName}:{token.line}:{token.column}");
        ColoredWrite(DarkGray     , "]\n");
    }

    private static void ShowSourceLine(int lineNumber, string sourceLine)
    {
        ColoredWrite(DarkGray     , $"{lineNumber,4} │  ");
        ColoredWrite(ResetColor() , $"{sourceLine.TrimStart()}\n");
    }

    private static void ShowErrorPosition(ConsoleColor errorColor, string errorLine)
    {
        ColoredWrite(DarkGray     , "     │");
        ColoredWrite(errorColor   ,$"  {errorLine.TrimEnd()}");
    }

    private static void ShowNotePosition(ConsoleColor errorColor, string errorLine)
    {
        ColoredWrite(DarkGray     , "     │");
        ColoredWrite(errorColor   ,$"  {errorLine.TrimEnd()}\n");
    }

    private static void ShowErrorHelp(ConsoleColor errorColor, string errorHelp)
    {
        ColoredWrite(errorColor, "/> ");
        Console.WriteLine(errorHelp);
    }

    private static void ShowNote(ConsoleColor noteColor, string noteMessage)
    {
        ColoredWrite(DarkGray     ,  "     │  ");
        ColoredWrite(noteColor    , "Note");
        ColoredWrite(ResetColor() , $": {noteMessage}\n");
    }

    private static (string, string) FetchSourceLine(Token token, string fileName)
    {
        string line = File.ReadLines(fileName).Skip(token.line - 1).Take(token.line).First();
        int whiteSpace = line.TakeWhile(char.IsWhiteSpace).Count() + 1;

        string error = new string(' ', line.Length - token.value.Length)
            .Insert(token.column - whiteSpace, new string('~', token.value.Length));

        return (line, error);
    }
}