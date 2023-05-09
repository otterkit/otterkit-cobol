using static Otterkit.Types.TokenHandling;
using Otterkit.Types;

namespace Otterkit.Analyzers;

/// <summary>
/// Otterkit COBOL Syntax and Semantic Analyzer
/// <para>This parser was built to be easily extensible, with some reusable COBOL parts.</para>
/// <para>It requires a List of Tokens generated from the Lexer and the Token Classifier.</para>
/// </summary>
public static partial class EnvironmentDivision
{
    // Method responsible for parsing the ENVIRONMENT DIVISION.
    // That includes the CONFIGURATION and the INPUT-OUTPUT sections.
    // It is also responsible for showing appropriate error messages when an error occurs in the ENVIRONMENT DIVISION.
    public static void Parse()
    {
        Expected("ENVIRONMENT");
        Expected("DIVISION");
        CompilerContext.ActiveScope = ActiveScope.EnvironmentDivision;

        if (!Expected(".", false))
        {
            ErrorHandler
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
                ErrorHandler
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
                ErrorHandler
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
            ErrorHandler
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
            References.Identifier(Current());

            Expected(".");
        }
    }

    private static void ObjectComputer()
    {
        Expected("OBJECT-COMPUTER");
        if (!Expected(".", false))
        {
            ErrorHandler
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
            References.Identifier(Current());
            shouldHaveSeparator = true;
        }

        if (CurrentEquals("CHARACTER", "CLASSIFICATION"))
        {
            CharacterClassification();
            shouldHaveSeparator = true;
        }

        if (CurrentEquals("PROGRAM COLLATING SEQUENCE", true))
        {
            ProgramCollatingSequence();
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
            ErrorHandler
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
            AlphabetName();
        }

        while (CurrentEquals("CLASS"))
        {
            ClassName();
        }

        if (CurrentEquals("CRT"))
        {
            Expected("CRT");
            Expected("STATUS");
            Optional("IS");

            References.Identifier();
        }
        
        while (CurrentEquals("CURRENCY"))
        {
            Expected("CURRENCY");
            Optional("SIGN");
            Optional("IS");

            Literals.String();

            if (CurrentEquals("WITH", "PICTURE"))
            {
                Optional("WITH");
                Expected("PICTURE");
                Expected("SYMBOL");

                Literals.String();
            }
        }

        if (CurrentEquals("CURSOR"))
        {
            Expected("CURSOR");
            Optional("IS");

            References.Identifier();
        }

        if (CurrentEquals("DECIMAL-POINT"))
        {
            Expected("DECIMAL-POINT");
            Optional("IS");
            Expected("COMMA");
        }

        while (CurrentEquals("DYNAMIC"))
        {
            DynamicLengthStructure();
        }

        while (CurrentEquals("LOCALE"))
        {
            Expected("LOCALE");
            Optional("IS");

            // TODO: Define allowed locale names
            Literals.String();
        }

        while (CurrentEquals(TokenType.Identifier))
        {
            // TODO: Define allowed device, feature and switch names
            // and replace TokenType.Identifier with the allowed names
            References.Identifier();
            Optional("IS");

            // Mnemonic name
            References.Identifier();
        }

        while (CurrentEquals("SYMBOLIC"))
        {
            SymbolicCharacters();
        }

        if (CurrentEquals("ORDER"))
        {
            Expected("ORDER");
            Expected("TABLE");
            References.Identifier();

            Optional("IS");
            Literals.String();
        }

        if (!LookaheadEquals(-2, "SPECIAL-NAMES") && !LookaheadEquals(-1, "SPECIAL-NAMES"))
        {
            Expected(".");
        }
    }
    
    private static void Repository()
    {
        Expected("REPOSITORY");
        CompilerContext.ActiveScope = ActiveScope.Repository;

        if (!Expected(".", false))
        {
            ErrorHandler
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

        while (CurrentEquals("CLASS INTERFACE FUNCTION PROGRAM PROPERTY", true))
        {
            if (CurrentEquals("CLASS"))
            {
                Expected("CLASS");
                References.Identifier();

                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    Literals.String();
                }

                if (CurrentEquals("EXPANDS"))
                {
                    Expected("EXPANDS");
                    References.Identifier();

                    Expected("USING");
                    if (!CurrentEquals(TokenType.Identifier))
                    {
                        ErrorHandler
                        .Build(ErrorType.Analyzer, ConsoleColor.Red, 105, """
                            Missing USING phrase class or interface name.
                            """)
                        .WithSourceLine(Lookahead(-1), """
                            The USING phrase must define at least one class or interface name.
                            """)
                        .CloseError();

                        AnchorPoint("CLASS INTERFACE FUNCTION PROGRAM PROPERTY DATA PROCEDURE");
                    }

                    References.Identifier();

                    while (CurrentEquals(TokenType.Identifier)) References.Identifier();
                }
            }

            if (CurrentEquals("INTERFACE"))
            {
                Expected("INTERFACE");
                References.Identifier();

                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    Literals.String();
                }

                if (CurrentEquals("EXPANDS"))
                {
                    Expected("EXPANDS");
                    References.Identifier();

                    Expected("USING");
                    if (!CurrentEquals(TokenType.Identifier))
                    {
                        ErrorHandler
                        .Build(ErrorType.Analyzer, ConsoleColor.Red, 105, """
                            Missing USING phrase class or interface name.
                            """)
                        .WithSourceLine(Lookahead(-1), """
                            The USING phrase must define at least one class or interface name.
                            """)
                        .CloseError();

                        AnchorPoint("CLASS INTERFACE FUNCTION PROGRAM PROPERTY DATA PROCEDURE");
                    }

                    References.Identifier();

                    while (CurrentEquals(TokenType.Identifier)) References.Identifier();
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
                    References.Identifier();
                    if (CurrentEquals("AS"))
                    {
                        Expected("AS");
                        Literals.String();
                    }
                }
            }

            if (CurrentEquals("PROGRAM"))
            {
                Expected("PROGRAM");
                References.Identifier();
                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    Literals.String();
                }
            }

            if (CurrentEquals("PROPERTY"))
            {
                Expected("PROPERTY");
                References.Identifier();
                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    Literals.String();
                }
            }
        }

        if (!Expected(".", false))
        {
            ErrorHandler
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
        CompilerContext.ActiveScope = ActiveScope.FileControl;

        if (!Expected(".", false))
        {
            ErrorHandler
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

            References.Identifier();

            while (CurrentEquals(TokenType.Identifier))
            {
                References.Identifier();
            }

            Expected(".");
        }

        if (CurrentEquals("SAME"))
        {
            while (CurrentEquals("SAME"))
            {
                Same();
            }

            Expected(".");
        }
    }

    private static void FileControl()
    {
        Expected("FILE-CONTROL");
        CompilerContext.ActiveScope = ActiveScope.FileControl;

        if (!Expected(".", false))
        {
            ErrorHandler
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

        References.Identifier();

        var fileControl = Assign(fileToken);

        if (!CurrentEquals(TokenContext.IsClause) && !CurrentEquals("."))
        {
            ErrorHandler
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
            ErrorHandler
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
        if (CompilerContext.IsResolutionPass) return;

        // Because we don't want to run this again during it
        var sourceUnit = CompilerContext.ActiveCallable;

        if (sourceUnit.FileEntries.EntryExists(fileToken))
        {
            ErrorHandler
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

        sourceUnit.FileEntries.AddEntry(fileToken, fileControl);
    }
}
