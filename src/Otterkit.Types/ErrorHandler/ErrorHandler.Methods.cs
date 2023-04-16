namespace Otterkit.Types;

public readonly ref partial struct ErrorHandler
{
    public static void SuccessfulParse()
    {
        var filesCount = CompilerContext.FileNames.Count;
        var isPlural = filesCount > 1 ? "s" : "";

        Console.ForegroundColor = ConsoleColor.Blue;
        Console.WriteLine($"Analyzed {filesCount} file{isPlural}, no errors found! \n");
        Console.ResetColor();
    }

    public static void StopCompilation(string errorType)
    {
        Console.ForegroundColor = ConsoleColor.Red;
        Console.WriteLine($"Compilation process cancelled due to {errorType} error");
        Console.ResetColor();
        Environment.Exit(1);
    }

    public static ErrorHandler Build(ErrorType errorType, ConsoleColor consoleColor, int errorCode, string errorMessage)
    {
        // Return early, don't display the initial message:
        if (SuppressedError == errorType) return new (errorType, consoleColor);

        Console.OutputEncoding = System.Text.Encoding.UTF8;

        ColoredWrite(consoleColor, $"\n {errorType} Error [COB{errorCode:D4}]");

        ColoredWrite(ResetColor(), $": {errorMessage}\n");

        ErrorHandler.HasOccurred = true;
        return new(errorType, consoleColor);
    }

    public ErrorHandler WithSourceLine(Token token, string? errorHelp = null)
    {
        // Return early, don't display any further messages:
        if (SuppressedError == ErrorType) return this;

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

    public ErrorHandler WithSourceNote(Token token)
    {
        // Return early, don't display any further messages:
        if (SuppressedError == ErrorType) return this;

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

    public ErrorHandler WithNote(string noteMessage)
    {
        // Return early, don't display any further messages:
        if (SuppressedError == ErrorType) return this;

        //   │  Note: {noteMessage} 
        ShowNote(Green, noteMessage);

        //   │
        Separator();

        return this;
    }

    public ErrorHandler WithStartingError(string errorNote)
    {
        // Return early, don't display any further messages:
        if (SuppressedError == ErrorType) return this;

        //   ╭─/> 
        StartingSeparator('╭', '\n');

        //   │
        Separator();

        //   │  Note: {noteMessage} 
        ShowNote(ConsoleColor.Red, errorNote);

        //   │
        Separator();

        return this;
    }

    public void CloseError()
    {
        // Return early, don't display the closing message:
        if (SuppressedError == ErrorType) return;

        // ────╯
        ColoredWrite(DarkGray, " ────╯\n");
        Console.ResetColor();
    }
}
