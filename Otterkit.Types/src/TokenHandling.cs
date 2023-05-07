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
    public static void AnchorPoint(params string[] anchors)
    {
        while (!CurrentEquals(TokenType.EOF))
        {
            if (CurrentEquals("."))
            {
                Continue();
                return;
            }

            if (CurrentEquals(anchors))
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

    public static void AnchorPoint(TokenContext anchor, params string[] anchors)
    {
        while (!CurrentEquals(TokenType.EOF))
        {
            if (CurrentEquals("."))
            {
                Continue();
                return;
            }

            if (CurrentEquals(anchors) || CurrentEquals(anchor))
            {
                return;
            }

            Continue();
        }
    }

    /// <summary>
    /// Token <c>Lookahead</c>: This method returns a Token from an index of Current Index + the amount parameter
    /// <para>When passed a positive amount it will act as a lookahead method, and when passed a negative amount it will act as a lookbehind method</para>
    /// <para>Technically this method allows for infinite lookahead and lookbehind, as long as the Index + amount is not bigger than the
    /// number of items on the list of Tokens or smaller than 0.</para>
    /// </summary>
    public static Token Lookahead(int amount)
    {
        if (Index + amount >= Source.Count) return Source[^1];

        return Index + amount < 0 ? Source[0] : Source[Index + amount];
    }

    /// <summary>
    /// <c>LookaheadEquals</c>: This method returns true or false depending on if the Token value from an index of Current Index + the first parameter is equal to the values of the second paramenter
    /// <para>When passed a positive amount it will act as a lookahead comparison method, and when passed a negative amount it will act as a lookbehind comparison method</para>
    /// <para>Technically this method allows for infinite lookahead and lookbehind comparisons, as long as the Index + amount is not bigger than the
    /// number of items on the list of Tokens or smaller than 0.</para>
    /// </summary>
    public static bool LookaheadEquals(int lookahead, string value)
    {
        return Lookahead(lookahead).Value.Equals(value, StringComparison.OrdinalIgnoreCase);
    }

    public static bool LookaheadEquals(int lookahead, params string[] valuesToCompare)
    {
        foreach (var value in valuesToCompare)
        {
            if (Lookahead(lookahead).Value.Equals(value, StringComparison.OrdinalIgnoreCase)) return true;
        }

        return false;
    }

    /// <summary>
    /// Boolean <c>LookaheadEquals</c> TokenType[]: This method returns true or false depending on if the Token type from an index of Current Index + the first parameter is equal to the types of the second paramenter
    /// <para>When passed a positive amount it will act as a lookahead comparison method, and when passed a negative amount it will act as a lookbehind comparison method</para>
    /// <para>Technically this method allows for infinite lookahead and lookbehind comparisons, as long as the Index + amount is not bigger than the
    /// number of items on the list of Tokens or smaller than 0.</para>
    /// </summary>
    public static bool LookaheadEquals(int lookahead, TokenType tokenType)
    {
        return Lookahead(lookahead).Type == tokenType;
    }

    /// <summary>
    /// Token <c>Current</c>: This method returns the current token from the current Index.
    /// <para>The returned token in encapsulated inside of the Token type, which holds not only the value of the token but also some additional context
    /// that might be required by the parser in certain situations</para>
    /// </summary>
    public static Token Current()
    {
        return Source[Index];
    }

    public static bool CurrentEquals(string value)
    {
        return Current().Value.Equals(value, StringComparison.OrdinalIgnoreCase);
    }

    /// <summary>
    /// Boolean <c>CurrentEquals</c> string[]: This method returns true or false depending on if the current token from the current Index has the same value as the parameter.
    /// <para>This helper method is an alternative to the <c>"Current().value.Equals()"</c> syntax, which could become verbose and harder to read when it's used frequently</para>
    /// </summary>
    public static bool CurrentEquals(params string[] valuesToCompare)
    {
        foreach (var value in valuesToCompare)
        {
            if (Current().Value.Equals(value, StringComparison.OrdinalIgnoreCase)) return true;
        }

        return false;
    }

    public static bool CurrentEquals(TokenType tokenType)
    {
        return Current().Type == tokenType;
    }

    /// <summary>
    /// Boolean <c>CurrentEquals</c> TokenType[]: This method returns true or false depending on if the current token from the current Index has the same type as the parameter.
    /// <para>This helper method is an alternative to the <c>"Current().value.Equals()"</c> syntax, which could become verbose and harder to read when it's used frequently</para>
    /// </summary>
    public static bool CurrentEquals(params TokenType[] tokenTypesToCompare)
    {
        foreach (var type in tokenTypesToCompare)
        {
            if (Current().Type == type) return true;
        }

        return false;
    }

    /// <summary>
    /// Boolean <c>CurrentEquals</c> TokenContext[]: This method returns true or false depending on if the current token from the current Index has the same context as the parameter.
    /// <para>This helper method is an alternative to the <c>"Current().value.Equals()"</c> syntax, which could become verbose and harder to read when it's used frequently</para>
    /// </summary>
    public static bool CurrentEquals(TokenContext tokenContext)
    {
        return Current().Context == tokenContext;
    }

    /// <summary>
    /// Void <c>Continue</c>: This method adds +1 to the current index, moving the index to the next token.
    /// <para>This method will be called in the Expected(), Optional(), Choice(), Identifier(), Number() and String() methods,
    /// so there's no need to call Continue after calling those methods.</para>
    /// </summary>
    public static void Continue(int amount = 1)
    {
        if (CurrentEquals(TokenType.EOF) && Index >= Source.Count - 1) return;

        Index += amount;
    }

    /// <summary>
    /// Void <c>Choice</c>: This method checks if the current token matches one of the values passed in its parameters.
    /// <para>If the current token matches one the values, it moves to the next token,
    /// if the current token doesn't match any of the values it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    public static void Choice(string firstChoice, string secondChoice)
    {
        if (CurrentEquals(firstChoice) || CurrentEquals(secondChoice))
        {
            Continue();
            return;
        }
        
        ErrorHandler
        .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, """
            Unexpected token.
            """)
        .WithSourceLine(Current(), $"""
            Expected one of the following: {firstChoice}, {secondChoice}.
            """)
        .CloseError();

        Continue();
    }

    public static void Choice(params string[] choices)
    {
        foreach (string choice in choices)
        {
            if (CurrentEquals(choice))
            {
                Continue();
                return;
            }
        }

        ErrorHandler
        .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, """
            Unexpected token.
            """)
        .WithSourceLine(Current(), $"""
            Expected one of the following: {string.Join(", ", choices)},
            """)
        .CloseError();

        Continue();
    }

    /// <summary>
    /// Void <c>Optional</c>: This method checks if the current token is equal to it's first parameter.
    /// <para>If the current token matches the value, it moves to the next token,
    /// if the current token doesn't match the value it ignores the token and returns without moving to the next token</para>
    /// </summary>
    public static void Optional(string optional)
    {
        if (CurrentEquals(optional)) Continue();
    }

    public static void OptionalChoice(params string[] choices)
    {
        foreach (string choice in choices)
        {
            if (CurrentEquals(choice))
            {
                Continue();
                return;
            }
        }
    }

    /// <summary>
    /// Bool <c>Expected</c>: This method checks if the current token is equal to it's first parameter.
    /// <para>If the current token matches the value, it moves to the next token,
    /// if the current token doesn't match the value it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    public static bool Expected(string expected, bool useDefaultError = true)
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
