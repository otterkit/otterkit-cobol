namespace Otterkit.Types;

public static class TokenHandling
{
    /// <summary>
    /// Used for keeping track of the current token list index.
    /// </summary>  
    public static int Index;
    public static List<Token> Source => CompilerContext.SourceTokens;
    
    // Analyzer token handling methods.
    // These are the main methods used to interact with and iterate through the List of Tokens.
    // (And other helpers that it easier to iterate through the List)
    // All other methods inside of the analyzer depend on these to parse through the tokens.
    public static void AnchorPoint(ReadOnlySpan<char> anchors)
    {
        while (!CurrentEquals(TokenType.EOF))
        {
            if (CurrentEquals("."))
            {
                Continue();
                return;
            }

            if (CurrentEquals(anchors, true))
            {
                return;
            }

            Continue();
        }
    }

    public static void AnchorPoint(TokenContext anchor)
    {
        while (!CurrentEquals(TokenType.EOF))
        {
            if (CurrentEquals(".", ". "))
            {
                Continue();
                return;
            }

            if (CurrentEquals(anchor))
            {
                return;
            }

            Continue();
        }
    }

    public static void AnchorPoint(TokenContext anchor, ReadOnlySpan<char> anchors)
    {
        while (!CurrentEquals(TokenType.EOF))
        {
            if (CurrentEquals("."))
            {
                Continue();
                return;
            }

            if (CurrentEquals(anchors, true) || CurrentEquals(anchor))
            {
                return;
            }

            Continue();
        }
    }

    public static Token Lookahead(int amount)
    {
        if (Index + amount >= Source.Count) return Source[^1];

        return Index + amount < 0 ? Source[0] : Source[Index + amount];
    }

    public static bool LookaheadEquals(int lookahead, ReadOnlySpan<char> keyword, bool multipleKeywords = false)
    {
        if (multipleKeywords)
        {
            return Optimization.SpaceSeparatedSearch(keyword, Current().Value);
        }
        
        return keyword.Equals(Lookahead(lookahead).Value, StringComparison.OrdinalIgnoreCase);
    }

    public static bool LookaheadEquals(int lookahead, ReadOnlySpan<char> keyword, ReadOnlySpan<char> otherKeyword)
    {
        return LookaheadEquals(lookahead, keyword) || LookaheadEquals(lookahead, otherKeyword);
    }

    public static bool LookaheadEquals(int lookahead, TokenType type)
    {
        return Lookahead(lookahead).Type == type;
    }

    public static Token Current()
    {
        return Source[Index];
    }

    public static bool CurrentEquals(ReadOnlySpan<char> keyword, bool multipleKeywords = false)
    {
        if (multipleKeywords)
        {
            return Optimization.SpaceSeparatedSearch(keyword, Current().Value);
        }

        return keyword.Equals(Current().Value, StringComparison.OrdinalIgnoreCase);
    }

    public static bool CurrentEquals(ReadOnlySpan<char> keyword, ReadOnlySpan<char> otherKeyword)
    {
        return CurrentEquals(keyword) || CurrentEquals(otherKeyword);
    }

    public static bool CurrentEquals(TokenType tokenType)
    {
        return Current().Type == tokenType;
    }

    public static bool CurrentEquals(TokenType type, TokenType otherType)
    {
        return CurrentEquals(type) || CurrentEquals(otherType);
    }

    public static bool CurrentEquals(params TokenType[] tokenTypesToCompare)
    {
        foreach (var type in tokenTypesToCompare)
        {
            if (Current().Type == type) return true;
        }

        return false;
    }

    public static bool CurrentEquals(TokenContext tokenContext)
    {
        return Current().Context == tokenContext;
    }

    public static void Continue(int amount = 1)
    {
        if (CurrentEquals(TokenType.EOF) && Index >= Source.Count - 1) return;

        Index += amount;
    }

    public static void Choice(ReadOnlySpan<char> firstKeyword, ReadOnlySpan<char> otherKeyword)
    {
        if (CurrentEquals(firstKeyword) || CurrentEquals(otherKeyword))
        {
            Continue();

            return;
        }
        
        ErrorHandler
        .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, """
            Unexpected token.
            """)
        .WithSourceLine(Current(), $"""
            Expected one of the following: {firstKeyword}, {otherKeyword}.
            """)
        .CloseError();

        Continue();
    }

    public static void Choice(ReadOnlySpan<char> choices)
    {
        if (Optimization.SpaceSeparatedSearch(choices, Current().Value))
        {
            Continue();

            return;
        }

        ErrorHandler
        .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, """
            Unexpected token.
            """)
        .WithSourceLine(Current(), $"""
            Expected one of the following: {string.Concat(", ", choices)},
            """)
        .CloseError();

        Continue();
    }

    public static void Optional(ReadOnlySpan<char> optional)
    {
        if (CurrentEquals(optional)) Continue();
    }

    public static void OptionalChoice(ReadOnlySpan<char> choices)
    {
        if (Optimization.SpaceSeparatedSearch(choices, Current().Value))
        {
            Continue();
        }
    }

    public static bool Expected(ReadOnlySpan<char> expected, bool useDefaultError = true)
    {
        if (CurrentEquals(TokenType.EOF))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 0, """
                Unexpected end of file.
                """)
            .WithSourceLine(Lookahead(-1), $"""
                Expected {expected} after this token.
                """)
            .CloseError();

            // Error has already been handled above
            return true;
        }

        var isExpectedToken = CurrentEquals(expected);

        if (!isExpectedToken && useDefaultError)
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, """
                Unexpected token.
                """)
            .WithSourceLine(Current(), $"""
                $"Expected '{expected}' here, found '{Current().Value}' instead"
                """)
            .CloseError();

            // Error has already been handled above
            // Handled using a default error message
            return true;
        }

        if (!isExpectedToken && !useDefaultError)
        {
            // Error has not been handled
            // Expected to be handled by the consumer
            // of this method using an if statement
            return false;
        }
        
        Continue();

        return true;
    }
}
