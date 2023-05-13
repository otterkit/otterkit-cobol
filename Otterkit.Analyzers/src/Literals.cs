using static Otterkit.Types.TokenHandling;
using Otterkit.Types;

namespace Otterkit.Analyzers;

public static class Literals
{
    public static void Numeric()
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
            return;
        }

        Continue();
    }

    public static void String()
    {
        if (!CurrentEquals(TokenType.String | TokenType.HexString))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 1, """
                Unexpected token type.
                """)
            .WithSourceLine(Current(), $"""
                Expected a string type literal.
                """)
            .CloseError();

            Continue();
            return;
        }

        Continue();
    }

    public static void Boolean()
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
            return;
        }

        Continue();
    }

    public static void National()
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
            return;
        }

        Continue();
    }

    public static void Figurative()
    {
        if (!CurrentEquals(TokenType.FigurativeLiteral))
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
            return;
        }

        Continue();
    }
}
