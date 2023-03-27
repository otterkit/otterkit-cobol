namespace Otterkit;

public readonly ref partial struct Error
{
    public static Error Build(ErrorType errorType, ConsoleColor consoleColor, int errorCode, string errorMessage)
    {
        Console.OutputEncoding = System.Text.Encoding.UTF8;

        ColoredWrite(consoleColor, $"\n {errorType} Error [COB{errorCode:D4}]");

        ColoredWrite(ResetColor(), $": {errorMessage}\n");

        ErrorHandler.HasError = true;
        return new(errorType, consoleColor);
    }

    public Error WithSourceLine(Token token, string? errorHelp = null)
    {
        var (line, error) = FetchSourceLine(token);
        
        //   ╭─/> [file.cob:5:25]
        ShowFileInformation('╭', token);

        //   │ 
        Separator();

        // 7 │  END PROGRAM HELLO.  
        ShowSourceLine(token.Line, line);

        //   │              ~~~~~
        ShowErrorPosition(ConsoleColor, error);

        if (errorHelp is not null)
        {
            // ~~/> Expected the following identifier: HELLO-WORLD.
            ShowErrorHelp(ConsoleColor, errorHelp);
        }

        if (errorHelp is null)
        {
            // Move to next line if no error help message was provided
            Console.WriteLine();
        }

        //   │
        Separator();

        return this;
    }

    public Error WithSourceNote(Token token)
    {
        var (line, note) = FetchSourceLine(token);
        
        //   ├─/> [file.cob:5:25]
        ShowFileInformation('├', token);

        //   │ 
        Separator();

        // 7 │  END PROGRAM HELLO.  
        ShowSourceLine(token.Line, line);

        //   │              ~~~~~
        ShowNotePosition(Green, note);

        //   │
        Separator();

        return this;
    }

    public Error WithNote(string noteMessage)
    {
        //   │  Note: {noteMessage} 
        ShowNote(Green, noteMessage);

        //   │
        Separator();

        return this;
    }

    public void CloseError()
    {
        // ────╯
        ColoredWrite(DarkGray, " ────╯\n");
    }
}
