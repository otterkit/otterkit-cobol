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

            if (CurrentEquals("REPOSITORY")) Repository();
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

    private static FileControlEntry AssignClause(Token fileToken)
    {
        Expected("ASSIGN");

        FileControlEntry fileControl;

        if (CurrentEquals("USING"))
        {
            Expected("USING");

            fileControl = new(fileToken, EntryType.FileControl, true);

            fileControl.Assign.Add(Current());
            Identifier();
        }
        else
        {
            Optional("TO");
            fileControl = new(fileToken, EntryType.FileControl, false);
            
            fileControl.Assign.Add(Current());
            IdentifierOrLiteral(TokenType.String);

            while(CurrentEquals(TokenType.Identifier, TokenType.String))
            {
                fileControl.Assign.Add(Current());
                IdentifierOrLiteral(TokenType.String);
            }
        }

        fileControl.Section = CurrentScope;

        return fileControl;
    }

    private static void FileControlClauses(FileControlEntry fileControl)
    {
        if (CurrentEquals("ACCESS"))
        {
            AccessClause(fileControl);
        }

        if (CurrentEquals("RECORD") && !LookaheadEquals(1, "DELIMITER"))
        {
            RecordClause(fileControl);
        }

        if (CurrentEquals("RECORD") && LookaheadEquals(1, "DELIMITER"))
        {
            RecordDelimiterClause(fileControl);
        }

        while (CurrentEquals("ALTERNATE"))
        {
            AlternateRecordClause(fileControl);
        }

        if (CurrentEquals("COLLATING", "SEQUENCE"))
        {
            CollatingSequenceClause(fileControl);
        }

        if (CurrentEquals("RELATIVE"))
        {
            RelativeClause(fileControl);
        }

        if (CurrentEquals("FILE", "STATUS"))
        {
            FileStatusClause(fileControl);
        }

        if (CurrentEquals("LOCK"))
        {
            LockClause(fileControl);
        }

        if (CurrentEquals("ORGANIZATION", "INDEXED", "RELATIVE", "LINE", "RECORD", "SEQUENTIAL"))
        {
            OrganizationClause(fileControl);
        }

        if (CurrentEquals("RESERVE"))
        {
            ReserveClause(fileControl);
        }

        if (CurrentEquals("SHARING"))
        {
            SharingClause(fileControl);
        }
    }

    private static void AccessClause(FileControlEntry fileControl)
    {
        Expected("ACCESS");
        Optional("MODE");
        Optional("IS");

        Choice("DYNAMIC", "RANDOM", "SEQUENTIAL");
    }

    private static void RecordClause(FileControlEntry fileControl)
    {
        Expected("RECORD");

        Optional("KEY");
        Optional("IS");

        if (!LookaheadEquals(1, "SOURCE"))
        {
            Identifier();

            return;
        }

        // If Lookahead(1) does equal SOURCE:
        Identifier();
        Expected("SOURCE");

        Optional("IS");
        Identifier();

        while (CurrentEquals(TokenType.Identifier))
        {
            Identifier();
        }
    }

    private static void AlternateRecordClause(FileControlEntry fileControl)
    {
        Expected("ALTERNATE");
        Expected("RECORD");

        Optional("KEY");
        Optional("IS");

        if (!LookaheadEquals(1, "SOURCE"))
        {
            Identifier();

            if (CurrentEquals("WITH", "DUPLICATES"))
            {
                Optional("WITH");
                Expected("DUPLICATES");
            }

            if (CurrentEquals("SUPPRESS"))
            {
                Expected("SUPPRESS");
                Optional("WHEN");

                String();
            }

            return;
        }

        // If Lookahead(1) does equal SOURCE:
        Identifier();
        Expected("SOURCE");

        Optional("IS");
        Identifier();

        while (CurrentEquals(TokenType.Identifier))
        {
            Identifier();
        }

        if (CurrentEquals("WITH", "DUPLICATES"))
        {
            Optional("WITH");
            Expected("DUPLICATES");
        }

        if (CurrentEquals("SUPPRESS"))
        {
            Expected("SUPPRESS");
            Optional("WHEN");

            String();
        }
    }

    private static void LockClause(FileControlEntry fileControl)
    {
        Expected("LOCK");
        Optional("MODE");
        Optional("IS");

        Choice("MANUAL", "AUTOMATIC");

        if (CurrentEquals("WITH", "LOCK"))
        {
            Optional("WITH");
            Expected("LOCK");
            Expected("ON");

            if (CurrentEquals("MULTIPLES"))
            {
                Expected("MULTIPLE");
            }

            Choice("RECORD", "RECORDS");
        }
    }

    private static void FileStatusClause(FileControlEntry fileControl)
    {
        Optional("FILE");
        Expected("STATUS");
        Optional("IS");

        Identifier();
    }

    private static void CollatingSequenceClause(FileControlEntry fileControl)
    {
        Optional("COLLATING");
        Expected("SEQUENCE");

        if (CurrentEquals("OF"))
        {
            Expected("OF");

            Identifier();

            while(CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }

            Optional("IS");
            Identifier();
            return;
        }

        if (CurrentEquals("FOR", "ALPHANUMERIC", "NATIONAL"))
        {
            ForAlphanumericForNational();
            return;
        }

        Optional("IS");
        Identifier();

        if (CurrentEquals(TokenType.Identifier))
        {
            Identifier();
        } 
    }

    private static void OrganizationClause(FileControlEntry fileControl)
    {
        if (CurrentEquals("ORGANIZATION"))
        {
            Expected("ORGANIZATION");
            Optional("IS");
        }

        if (CurrentEquals("LINE"))
        {
            Expected("LINE");
            Expected("SEQUENTIAL");

            return;
        }
        
        if (CurrentEquals("RECORD", "SEQUENTIAL"))
        {
            Optional("RECORD");
            Expected("SEQUENTIAL");

            return;
        }

        Choice("INDEXED", "RELATIVE");
    }

    private static void RelativeClause(FileControlEntry fileControl)
    {
        Expected("RELATIVE");
        Optional("KEY");
        Optional("IS");

        Identifier();
    }

    private static void RecordDelimiterClause(FileControlEntry fileControl)
    {
        Expected("RECORD");
        Expected("DELIMITER");
        Optional("IS");

        if (CurrentEquals("STANDARD-1"))
        {
            Expected("STANDARD-1");
        }
        else
        {
            // We have to define the names for these later:
            Identifier();
        }
    }

    private static void ReserveClause(FileControlEntry fileControl)
    {
        Expected("RESERVE");
        Number();

        if (CurrentEquals("AREA", "AREAS"))
        {
            Expected(Current().Value);
        }
    }

    private static void SharingClause(FileControlEntry fileControl)
    {
        Expected("SHARING");
        Optional("WITH");

        if (CurrentEquals("ALL"))
        {
            Expected("ALL");
            Optional("OTHER");
            return;
        }

        if (CurrentEquals("NO"))
        {
            Expected("NO");
            Optional("OTHER");
            return;
        }

        if (CurrentEquals("READ"))
        {
            Expected("READ");
            Expected("ONLY");
            return;
        }
    }
}
