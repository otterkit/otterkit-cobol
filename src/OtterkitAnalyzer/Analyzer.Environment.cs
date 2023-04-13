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
    private static void Environment()
    {
        Expected("ENVIRONMENT");
        Expected("DIVISION");
        CurrentScope = CurrentScope.EnvironmentDivision;

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
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
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
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

            if (CurrentEquals("SOURCE-COMPUTER"))
            {
                SourceComputer();
            }

            if (CurrentEquals("OBJECT-COMPUTER"))
            {
                ObjectComputer();
            }

            if (CurrentEquals("SPECIAL-NAMES"))
            {
                SpecialNames();
            }

            if (CurrentEquals("REPOSITORY"))
            {
                Repository();
            }
        }

        if (CurrentEquals("INPUT-OUTPUT"))
        {
            Expected("INPUT-OUTPUT");
            Expected("SECTION");

            if (!Expected(".", false))
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
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

            if (CurrentEquals("FILE-CONTROL"))
            {
                FileControl();
            }

            if (CurrentEquals("I-O-CONTROL"))
            {
                IoControl();
            }
        }
    }

    private static void SourceComputer()
    {
        Expected("SOURCE-COMPUTER");
        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
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

        if (CurrentEquals(TokenType.Identifier))
        {
            Identifier(Current());

            Expected(".");
        }
    }

    private static void ObjectComputer()
    {
        Expected("OBJECT-COMPUTER");
        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
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

        var shouldHaveSeparator = false;

        if (CurrentEquals(TokenType.Identifier))
        {
            Identifier(Current());
            shouldHaveSeparator = true;
        }

        if (CurrentEquals("CHARACTER", "CLASSIFICATION"))
        {
            CharacterClassificationClause();
            shouldHaveSeparator = true;
        }

        if (CurrentEquals("PROGRAM", "COLLATING", "SEQUENCE"))
        {
            ProgramCollatingSequenceClause();
            shouldHaveSeparator = true;
        }

        if (shouldHaveSeparator)
        {
            Expected(".");
        }
    }

    private static void SpecialNames()
    {
        Expected("SPECIAL-NAMES");
        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
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

        while (CurrentEquals("ALPHABET"))
        {
            AlphabetNameClause();
        }

        while (CurrentEquals("CLASS"))
        {
            ClassNameClause();
        }

        if (CurrentEquals("CRT"))
        {
            Expected("CRT");
            Expected("STATUS");
            Optional("IS");

            Identifier();
        }
        
        while (CurrentEquals("CURRENCY"))
        {
            Expected("CURRENCY");
            Optional("SIGN");
            Optional("IS");

            StringLiteral();

            if (CurrentEquals("WITH", "PICTURE"))
            {
                Optional("WITH");
                Expected("PICTURE");
                Expected("SYMBOL");

                StringLiteral();
            }
        }

        if (CurrentEquals("CURSOR"))
        {
            Expected("CURSOR");
            Optional("IS");

            Identifier();
        }

        if (CurrentEquals("DECIMAL-POINT"))
        {
            Expected("DECIMAL-POINT");
            Optional("IS");
            Expected("COMMA");
        }

        while (CurrentEquals("DYNAMIC"))
        {
            DynamicLengthStructureClause();
        }

        while (CurrentEquals("LOCALE"))
        {
            Expected("LOCALE");
            Optional("IS");

            // TODO: Define allowed locale names
            StringLiteral();
        }

        while (CurrentEquals(TokenType.Identifier))
        {
            // TODO: Define allowed device, feature and switch names
            // and replace TokenType.Identifier with the allowed names
            Identifier();
            Optional("IS");

            // Mnemonic name
            Identifier();
        }

        while (CurrentEquals("SYMBOLIC"))
        {
            SymbolicCharactersClause();
        }

        if (CurrentEquals("ORDER"))
        {
            Expected("ORDER");
            Expected("TABLE");
            Identifier();

            Optional("IS");
            StringLiteral();
        }

        if (!LookaheadEquals(-2, "SPECIAL-NAMES") && !LookaheadEquals(-1, "SPECIAL-NAMES"))
        {
            Expected(".");
        }
    }
    
    private static void Repository()
    {
        Expected("REPOSITORY");
        CurrentScope = CurrentScope.Repository;

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
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
                    StringLiteral();
                }

                if (CurrentEquals("EXPANDS"))
                {
                    Expected("EXPANDS");
                    Identifier();

                    Expected("USING");
                    if (!CurrentEquals(TokenType.Identifier))
                    {
                        Error
                        .Build(ErrorType.Analyzer, ConsoleColor.Red, 105, """
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
                    StringLiteral();
                }

                if (CurrentEquals("EXPANDS"))
                {
                    Expected("EXPANDS");
                    Identifier();

                    Expected("USING");
                    if (!CurrentEquals(TokenType.Identifier))
                    {
                        Error
                        .Build(ErrorType.Analyzer, ConsoleColor.Red, 105, """
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
                        StringLiteral();
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
                    StringLiteral();
                }
            }

            if (CurrentEquals("PROPERTY"))
            {
                Expected("PROPERTY");
                Identifier();
                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    StringLiteral();
                }
            }
        }

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
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

    private static void IoControl()
    {
        Expected("I-O-CONTROL");
        CurrentScope = CurrentScope.FileControl;

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
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

        if (CurrentEquals("APPLY"))
        {
            Expected("APPLY");
            Expected("COMMIT");
            Optional("ON");

            Identifier();

            while (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }

            Expected(".");
        }

        if (CurrentEquals("SAME"))
        {
            while (CurrentEquals("SAME"))
            {
                SameClause();
            }

            Expected(".");
        }
    }

    private static void FileControl()
    {
        Expected("FILE-CONTROL");
        CurrentScope = CurrentScope.FileControl;

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
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

        while (CurrentEquals("SELECT"))
        {
            FileControlEntry();
        }
    }

    private static void FileControlEntry()
    {
        Expected("SELECT");

        if (CurrentEquals("OPTIONAL"))
        {
            Expected("OPTIONAL");
        }

        Token fileToken = Current();
        string fileName = fileToken.Value;

        Identifier();

        var fileControl = AssignClause(fileToken);

        if (!CurrentEquals(TokenContext.IsClause) && !CurrentEquals("."))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 2,"""
                Unexpected token.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected file control clauses or a separator period after this token.
                """)
            .CloseError();
        }

        while (CurrentEquals(TokenContext.IsClause))
        {
            FileControlClauses(fileControl);
        }

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                File control, missing separator period.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected a separator period '. ' after this token.
                """)
            .WithNote("""
                Every file control item must end with a separator period.
                """)
            .CloseError();
        }

        // We're returning during a resolution pass
        if (IsResolutionPass) return;

        // Because we don't want to run this again during it
        var sourceUnit = CurrentCallable;

        if (sourceUnit.FileEntries.EntryExists(fileName))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 30,"""
                Duplicate root level definition.
                """)
            .WithSourceLine(fileToken, """
                A root level variable already exists with this name.
                """)
            .WithNote("""
                Every root level item must have a unique name. 
                """)
            .CloseError();
        }

        sourceUnit.FileEntries.AddEntry(fileName, fileControl);
    }
}
