namespace Otterkit;

/// <summary>
/// Otterkit COBOL Syntax and Semantic Analyzer
/// <para>This parser was built to be easily extensible, with some reusable COBOL parts.</para>
/// <para>It requires a List of Tokens generated from the Lexer and the Token Classifier.</para>
/// </summary>
public static partial class Analyzer
{
    // Method responsible for parsing the ENVIRONMENT DIVISION.
    // That includes the CONFIGURATION and the INPUT-OUTPUT sections.
    // It is also responsible for showing appropriate error messages when an error occurs in the ENVIRONMENT DIVISION.
    public static void ENVIRONMENT()
    {
        Expected("ENVIRONMENT");
        Expected("DIVISION");
        CurrentSection = CurrentScope.EnvironmentDivision;

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                Division header, missing separator period.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected a separator period '. ' after this token
                """)
            .WithNote("""
                Every division header must end with a separator period
                """)
            .CloseError();
        }

        if (CurrentEquals("CONFIGURATION"))
        {
            Expected("CONFIGURATION");
            Expected("SECTION");

            if (!Expected(".", false))
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                    Section header, missing separator period.
                    """)
                .WithSourceLine(Lookahead(-1), """
                    Expected a separator period '. ' after this token
                    """)
                .WithNote("""
                    Every section header must end with a separator period
                    """)
                .CloseError();
            }

            if (CurrentEquals("REPOSITORY")) REPOSITORY();
        }
    }

    public static void REPOSITORY()
    {
        Expected("REPOSITORY");
        CurrentSection = CurrentScope.Repository;

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                Paragraph header, missing separator period.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected a separator period '. ' after this token.
                """)
            .WithNote("""
                Every paragraph header must end with a separator period.
                """)
            .CloseError();
        }

        while (CurrentEquals("CLASS", "INTERFACE", "FUNCTION", "PROGRAM", "PROPERTY"))
        {
            if (CurrentEquals("CLASS"))
            {
                Expected("CLASS");
                Identifier();

                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    String();
                }

                if (CurrentEquals("EXPANDS"))
                {
                    Expected("EXPANDS");
                    Identifier();

                    Expected("USING");
                    if (!CurrentEquals(TokenType.Identifier))
                    {
                        Error
                        .Build(ErrorType.Analyzer, ConsoleColor.Red, 105,"""
                            Missing USING phrase class or interface name.
                            """)
                        .WithSourceLine(Lookahead(-1), """
                            The USING phrase must define at least one class or interface name.
                            """)
                        .CloseError();

                        AnchorPoint("CLASS", "INTERFACE", "FUNCTION", "PROGRAM", "PROPERTY", "DATA", "PROCEDURE");
                    }

                    Identifier();

                    while (CurrentEquals(TokenType.Identifier)) Identifier();
                }
            }

            if (CurrentEquals("INTERFACE"))
            {
                Expected("INTERFACE");
                Identifier();

                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    String();
                }

                if (CurrentEquals("EXPANDS"))
                {
                    Expected("EXPANDS");
                    Identifier();

                    Expected("USING");
                    if (!CurrentEquals(TokenType.Identifier))
                    {
                        Error
                        .Build(ErrorType.Analyzer, ConsoleColor.Red, 105,"""
                            Missing USING phrase class or interface name.
                            """)
                        .WithSourceLine(Lookahead(-1), """
                            The USING phrase must define at least one class or interface name.
                            """)
                        .CloseError();

                        AnchorPoint("CLASS", "INTERFACE", "FUNCTION", "PROGRAM", "PROPERTY", "DATA", "PROCEDURE");
                    }

                    Identifier();

                    while (CurrentEquals(TokenType.Identifier)) Identifier();
                }
            }

            if (CurrentEquals("FUNCTION"))
            {
                Expected("FUNCTION");
                if (CurrentEquals("ALL"))
                {
                    Expected("ALL");
                    Expected("INTRINSIC");
                }
                else if (CurrentEquals(TokenType.IntrinsicFunction))
                {
                    Continue();
                    while (CurrentEquals(TokenType.IntrinsicFunction) || CurrentEquals("RANDOM"))
                    {
                        Continue();
                    }

                    Expected("INTRINSIC");
                }
                else
                {
                    Identifier();
                    if (CurrentEquals("AS"))
                    {
                        Expected("AS");
                        String();
                    }
                }
            }

            if (CurrentEquals("PROGRAM"))
            {
                Expected("PROGRAM");
                Identifier();
                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    String();
                }
            }

            if (CurrentEquals("PROPERTY"))
            {
                Expected("PROPERTY");
                Identifier();
                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    String();
                }
            }
        }

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                Paragraph body, missing separator period.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected a separator period '. ' after this token.
                """)
            .WithNote("""
                Every paragraph body must end with a separator period.
                """)
            .CloseError();
        }
    }
}
