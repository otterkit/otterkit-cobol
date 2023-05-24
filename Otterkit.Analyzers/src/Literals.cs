using static Otterkit.Types.TokenHandling;
using Otterkit.Types;
using System.Text;

namespace Otterkit.Analyzers;

public static class Literals
{
    public static bool IsAny()
    {
        return CurrentEquals(TokenType.Numeric | TokenType.String | TokenType.HexString | TokenType.Boolean | TokenType.HexBoolean | TokenType.National | TokenType.HexNational | TokenType.Figurative);
    }

    public static Option<Token> Any()
    {
        if (CurrentEquals(TokenType.String | TokenType.HexString))
        {
            return Literals.String();
        }

        if (CurrentEquals(TokenType.National | TokenType.HexNational))
        {
            return Literals.National();
        }

        if (CurrentEquals(TokenType.Boolean | TokenType.HexBoolean))
        {
            return Literals.Boolean();
        }

        if (CurrentEquals(TokenType.Numeric))
        {
            return Literals.Numeric();
        }

        if (CurrentEquals(TokenType.Figurative))
        {
            return Literals.Figurative();
        }

        // Return empty Option<Token> if no literal is found.
        return null;
    }

    public static Option<Token> Numeric()
    {
        if (!CurrentEquals(TokenType.Numeric))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 1, """
                Unexpected token type.
                """)
            .WithSourceLine(Current(), $"""
                Expected a numeric literal.
                """)
            .CloseError();

            Continue();

            return null;
        }

        Continue();

        return Peek(-1);
    }

    public static Option<Token> String()
    {
        if (!CurrentEquals(TokenType.String | TokenType.HexString))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 1, """
                Unexpected token type.
                """)
            .WithSourceLine(Current(), $"""
                Expected an alphanumeric literal.
                """)
            .CloseError();

            Continue();

            return null;
        }

        var literal = Current();

        // TODO: Also need to handle fixed continuation lines:
        while (FloatingContinuationLine(Current().Value)) Continue();

        Continue();

        return literal;
    }

    public static Option<Token> Boolean()
    {
        if (!CurrentEquals(TokenType.Boolean | TokenType.HexBoolean))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 1, """
                Unexpected token type.
                """)
            .WithSourceLine(Current(), $"""
                Expected a boolean literal.
                """)
            .CloseError();

            Continue();

            return null;
        }

        Continue();

        return Peek(-1);
    }

    public static Option<Token> National()
    {
        if (!CurrentEquals(TokenType.National | TokenType.HexNational))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 1, """
                Unexpected token type.
                """)
            .WithSourceLine(Current(), $"""
                Expected a national literal.
                """)
            .CloseError();

            Continue();

            return null;
        }

        Continue();

        return Peek(-1);
    }

    public static Option<Token> Figurative()
    {
        if (!CurrentEquals(TokenType.Figurative))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 1, """
                Unexpected token type.
                """)
            .WithSourceLine(Current(), $"""
                Expected a figurative literal.
                """)
            .CloseError();

            Continue();

            return null;
        }

        Continue();

        return Peek(-1);
    }

    private static bool FloatingContinuationLine(ReadOnlySpan<char> literal)
    {
        return literal switch
        {
            [.. _, '"' or '\'', '-'] => true,
            [..] => false
        };
    }
}
