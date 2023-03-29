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

        Expected(".", """
        Missing separator period at the end of this ENVIRONMENT DIVISION header, every division header must end with a separator period
        """, -1, "DATA", "PROCEDURE", "PROGRAM-ID", "FUNCTION-ID");

        if (CurrentEquals("CONFIGURATION"))
        {
            Expected("CONFIGURATION");
            Expected("SECTION");
            Expected(".", """
            Missing separator period at the end of this CONFIGURATION SECTION header, every section must end with a separator period
            """, -1, "REPOSITORY", "DATA", "PROCEDURE", "PROGRAM-ID", "FUNCTION-ID");

            if (CurrentEquals("REPOSITORY")) REPOSITORY();
        }
    }

    public static void REPOSITORY()
    {
        Expected("REPOSITORY");
        CurrentSection = CurrentScope.Repository;

        Expected(".", """
        Missing separator period at the end of this REPOSITORY paragraph header, every paragraph must end with a separator period
        """, -1, "CLASS", "INTERFACE", "FUNCTION", "PROGRAM", "PROPERTY", "DATA", "PROCEDURE");

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
                        ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                        The USING clause must contain at least one class, object or interface name.
                        """);
                        ErrorHandler.Analyzer.PrettyError(FileName, Current());
                    }

                    if (!CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, TokenType.Identifier))
                    {
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
                        ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                        The USING clause must contain at least one class, object or interface name.
                        """);
                        ErrorHandler.Analyzer.PrettyError(FileName, Current());
                    }

                    if (!CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, TokenType.Identifier))
                    {
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

                    if (!CurrentEquals("CLASS", "INTERFACE", "FUNCTION", "PROGRAM", "PROPERTY", "."))
                    {
                        AnchorPoint("CLASS", "INTERFACE", "FUNCTION", "PROGRAM", "PROPERTY", "DATA", "PROCEDURE");
                    }
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

        Expected(".", """
        Missing separator period at the end of this REPOSITORY paragraph body, the last definition in the REPOSITORY paragraph must end with a period
        """, -1, "CLASS", "INTERFACE", "FUNCTION", "PROGRAM", "PROPERTY", "DATA", "PROCEDURE");
    }
}
