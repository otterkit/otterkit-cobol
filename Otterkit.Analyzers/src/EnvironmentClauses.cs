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
    private static void AlphabetName()
    {
        Expected("ALPHABET");
        References.Identifier();

        if (CurrentEquals("NATIONAL") || LookaheadEquals(1, "NATIONAL"))
        {
            Optional("FOR");
            Expected("NATIONAL");
            Optional("IS");

            if (CurrentEquals("LOCALE"))
            {
                Expected("LOCALE");
                References.Identifier();

                return;
            }

            if (CurrentEquals("NATIVE", "UCS-4", "UTF-8", "UTF-16", "UTF-32"))
            {
                Expected(Current().Value);

                return;
            }

            while (CurrentEquals(TokenType.National))
            {
                NationalLiteralPhrase();
            }

            return;
        }

        if (CurrentEquals("ALPHANUMERIC") || LookaheadEquals(1, "ALPHANUMERIC"))
        {
            Optional("FOR");
            Expected("ALPHANUMERIC");
        }

        Optional("IS");
        if (CurrentEquals("NATIVE", "STANDARD-1", "STANDARD-2", "UTF-8"))
        {
            Expected(Current().Value);

            return;
        }
        while (CurrentEquals(TokenType.String))
        {
            AlphanumericLiteralPhrase();
        }
    }

    private static void AlphanumericLiteralPhrase()
    {
        Literals.String();

        if (CurrentEquals("THROUGH", "THRU"))
        {
            Choice("THROUGH", "THRU");
            Literals.String();
            return;
        }

        while (CurrentEquals("ALSO"))
        {
            Expected("ALSO");
            Literals.String();
        }
    }

    private static void NationalLiteralPhrase()
    {
        Literals.National();

        if (CurrentEquals("THROUGH", "THRU"))
        {
            Choice("THROUGH", "THRU");
            Literals.National();
            return;
        }

        while (CurrentEquals("ALSO"))
        {
            Expected("ALSO");
            Literals.National();
        }
    }

    private static void ClassName()
    {
        Expected("CLASS");
        References.Identifier();

        if (CurrentEquals("FOR", "ALPHANUMERIC", "NATIONAL"))
        {
            Optional("FOR");
            Choice("ALPHANUMERIC", "NATIONAL");
        }

        Optional("IS");

        // Lookbehind:
        var national = LookaheadEquals(-2, "NATIONAL");

        var chosenType = national ? TokenType.National : TokenType.String;

        while(CurrentEquals(TokenType.String, TokenType.National))
        {
            ClassLiteralPhrase(chosenType);
        }

        if (CurrentEquals("IN"))
        {
            Expected("IN");
            References.Identifier();
        }
    }

    private static void ClassLiteralPhrase(TokenType literalType)
    {
        if (literalType is TokenType.National)
        {
            Literals.National();
            if (CurrentEquals("THROUGH", "THRU"))
            {
                Choice("THROUGH", "THRU");
                Literals.National();
            }

            return;
        }

        Literals.String();
        if (CurrentEquals("THROUGH", "THRU"))
        {
            Choice("THROUGH", "THRU");
            Literals.String();
        }
    }
    
    private static void DynamicLengthStructure()
    {
        Expected("DYNAMIC");
        Expected("LENGTH");
        Optional("STRUCTURE");

        References.Identifier();
        Optional("IS");

        if (CurrentEquals(TokenType.Identifier))
        {
            // TODO: Specify which physical structure names are allowed
            References.Identifier();
            return;
        }

        if (CurrentEquals("SIGNED", "SHORT", "DELIMITED"))
        {
            Optional("SIGNED");
            Optional("SHORT");

            Expected("PREFIXED");
        }

        if (CurrentEquals("DELIMITED"))
        {
            Expected("DELIMITED");
        }

        if (!LookaheadEquals(-1, "PREFIXED", "DELIMITED"))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
                Missing clause.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected PREFIXED and/or DELIMITED.
                """)
            .WithNote("""
                At least of the two must be present.
                """)
            .CloseError();
        }
    }

    private static void SymbolicCharacters()
    {
        Expected("SYMBOLIC");
        Optional("CHARACTERS");

        if (CurrentEquals("FOR", "ALPHANUMERIC", "NATIONAL"))
        {
            Optional("FOR");
            Choice("ALPHANUMERIC", "NATIONAL");
        }

        while (CurrentEquals(TokenType.Identifier))
        {
            while (CurrentEquals(TokenType.Identifier))
            {
                References.Identifier();
            }

            if (CurrentEquals("IS", "ARE"))
            {
                Choice("IS", "ARE");
            }

            while (CurrentEquals(TokenType.Numeric))
            {
                Literals.Numeric();
            }
        }

        if (CurrentEquals("IN"))
        {
            Expected("IS");
            References.Identifier();
        }
    }

    private static void CharacterClassification()
    {
        Optional("CHARACTER");
        Expected("CLASSIFICATION");

        if (CurrentEquals("FOR", "ALPHANUMERIC", "NATIONAL"))
        {
            Common.ForAlphanumericForNational();
            return;
        }

        Optional("IS");
        LocalePhrase();

        if (CurrentEquals(TokenType.Identifier) || CurrentEquals("LOCALE", "SYSTEM-DEFAULT", "USER-DEFAULT"))
        {
            LocalePhrase();
        }
    }

    private static void ForAlphaForNationalLocale(bool forAlphanumericExists = false, bool forNationalExists = false)
    {
        if (CurrentEquals("FOR") && LookaheadEquals(1, "ALPHANUMERIC") || CurrentEquals("ALPHANUMERIC"))
        {
            if (forAlphanumericExists)
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    For alphanumeric phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    FOR ALPHANUMERIC can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to FOR NATIONAL.
                    """)
                .CloseError();
            }
            forAlphanumericExists = true;

            Optional("FOR");
            Expected("ALPHANUMERIC");
            Optional("IS");

            LocalePhrase();

            ForAlphaForNationalLocale(forAlphanumericExists, forNationalExists);
        }

        if (CurrentEquals("FOR") && LookaheadEquals(1, "NATIONAL") || CurrentEquals("NATIONAL"))
        {
            if (forNationalExists)
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    For national phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    FOR NATIONAL can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to FOR ALPHANUMERIC.
                    """)
                .CloseError();
            }
            forNationalExists = true;

            Optional("FOR");
            Expected("NATIONAL");
            Optional("IS");

            LocalePhrase();

            ForAlphaForNationalLocale(forAlphanumericExists, forNationalExists);
        }
    }

    private static void LocalePhrase()
    {
        if (CurrentEquals("LOCALE", "SYSTEM-DEFAULT", "USER-DEFAULT"))
        {
            Expected(Current().Value);
        }
        else
        {
            References.Identifier();
        }
    }

    private static void ProgramCollatingSequence()
    {
        Optional("PROGRAM");
        Optional("COLLATING");
        Expected("SEQUENCE");

        if (CurrentEquals("FOR", "ALPHANUMERIC", "NATIONAL"))
        {
            Common.ForAlphanumericForNational();
            return;
        }

        Optional("IS");
        References.Identifier();

        if (CurrentEquals(TokenType.Identifier))
        {
            References.Identifier();
        }
    }

    private static void Same()
    {
        Expected("SAME");

        if (CurrentEquals("RECORD", "SORT", "SORT-MERGE"))
        {
            Expected(Current().Value);
        }

        Optional("AREA");
        Optional("FOR");

        References.Identifier();
        References.Identifier();

        while (CurrentEquals(TokenType.Identifier))
        {
            References.Identifier();
        }
    }

    private static FileControlEntry Assign(Token fileToken)
    {
        Expected("ASSIGN");

        FileControlEntry fileControl;

        if (CurrentEquals("USING"))
        {
            Expected("USING");

            fileControl = new(fileToken, EntryKind.FileControl, true);

            fileControl.Assign.Add(Current());
            References.Identifier();
        }
        else
        {
            Optional("TO");
            fileControl = new(fileToken, EntryKind.FileControl, false);

            fileControl.Assign.Add(Current());
            Common.IdentifierOrLiteral(TokenType.String);

            while (CurrentEquals(TokenType.Identifier, TokenType.String))
            {
                fileControl.Assign.Add(Current());
                Common.IdentifierOrLiteral(TokenType.String);
            }
        }

        fileControl.Section = CompilerContext.ActiveScope;

        return fileControl;
    }

    private static void FileControlClauses(FileControlEntry fileControl)
    {
        if (CurrentEquals("ACCESS"))
        {
            Access(fileControl);
        }

        if (CurrentEquals("RECORD") && !LookaheadEquals(1, "DELIMITER"))
        {
            Record(fileControl);
        }

        if (CurrentEquals("RECORD") && LookaheadEquals(1, "DELIMITER"))
        {
            RecordDelimiter(fileControl);
        }

        while (CurrentEquals("ALTERNATE"))
        {
            AlternateRecord(fileControl);
        }

        if (CurrentEquals("COLLATING", "SEQUENCE"))
        {
            CollatingSequence(fileControl);
        }

        if (CurrentEquals("RELATIVE"))
        {
            Relative(fileControl);
        }

        if (CurrentEquals("FILE", "STATUS"))
        {
            FileStatus(fileControl);
        }

        if (CurrentEquals("LOCK"))
        {
            Lock(fileControl);
        }

        if (CurrentEquals("ORGANIZATION", "INDEXED", "RELATIVE", "LINE", "RECORD", "SEQUENTIAL"))
        {
            Organization(fileControl);
        }

        if (CurrentEquals("RESERVE"))
        {
            Reserve(fileControl);
        }

        if (CurrentEquals("SHARING"))
        {
            Sharing(fileControl);
        }
    }

    private static void Access(FileControlEntry fileControl)
    {
        Expected("ACCESS");
        Optional("MODE");
        Optional("IS");

        Choice("DYNAMIC", "RANDOM", "SEQUENTIAL");
    }

    private static void Record(FileControlEntry fileControl)
    {
        Expected("RECORD");

        Optional("KEY");
        Optional("IS");

        if (!LookaheadEquals(1, "SOURCE"))
        {
            References.Identifier();

            return;
        }

        // If Lookahead(1) does equal SOURCE:
        References.Identifier();
        Expected("SOURCE");

        Optional("IS");
        References.Identifier();

        while (CurrentEquals(TokenType.Identifier))
        {
            References.Identifier();
        }
    }

    private static void AlternateRecord(FileControlEntry fileControl)
    {
        Expected("ALTERNATE");
        Expected("RECORD");

        Optional("KEY");
        Optional("IS");

        if (!LookaheadEquals(1, "SOURCE"))
        {
            References.Identifier();

            if (CurrentEquals("WITH", "DUPLICATES"))
            {
                Optional("WITH");
                Expected("DUPLICATES");
            }

            if (CurrentEquals("SUPPRESS"))
            {
                Expected("SUPPRESS");
                Optional("WHEN");

                Literals.String();
            }

            return;
        }

        // If Lookahead(1) does equal SOURCE:
        References.Identifier();
        Expected("SOURCE");

        Optional("IS");
        References.Identifier();

        while (CurrentEquals(TokenType.Identifier))
        {
            References.Identifier();
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

            Literals.String();
        }
    }

    private static void Lock(FileControlEntry fileControl)
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

    private static void FileStatus(FileControlEntry fileControl)
    {
        Optional("FILE");
        Expected("STATUS");
        Optional("IS");

        References.Identifier();
    }

    private static void CollatingSequence(FileControlEntry fileControl)
    {
        Optional("COLLATING");
        Expected("SEQUENCE");

        if (CurrentEquals("OF"))
        {
            Expected("OF");

            References.Identifier();

            while (CurrentEquals(TokenType.Identifier))
            {
                References.Identifier();
            }

            Optional("IS");
            References.Identifier();
            return;
        }

        if (CurrentEquals("FOR", "ALPHANUMERIC", "NATIONAL"))
        {
            Common.ForAlphanumericForNational();
            return;
        }

        Optional("IS");
        References.Identifier();

        if (CurrentEquals(TokenType.Identifier))
        {
            References.Identifier();
        }
    }

    private static void Organization(FileControlEntry fileControl)
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

    private static void Relative(FileControlEntry fileControl)
    {
        Expected("RELATIVE");
        Optional("KEY");
        Optional("IS");

        References.Identifier();
    }

    private static void RecordDelimiter(FileControlEntry fileControl)
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
            References.Identifier();
        }
    }

    private static void Reserve(FileControlEntry fileControl)
    {
        Expected("RESERVE");
        Literals.Numeric();

        if (CurrentEquals("AREA", "AREAS"))
        {
            Expected(Current().Value);
        }
    }

    private static void Sharing(FileControlEntry fileControl)
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
