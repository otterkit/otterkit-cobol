using static Otterkit.Types.TokenHandling;
using Otterkit.Types;

namespace Otterkit.Analyzers;

public static partial class DataDivision
{
    // The following methods are responsible for parsing data division clauses, 
    // each method is responsible for parsing only a single clause (Never parse two clauses with one method).
    // The IsClauseErrorCheck() method handles an 'IS' keyword potentially missing its accompanying clause.
    private static void IsClauseErrorCheck()
    {
        ErrorHandler
        .Build(ErrorType.Analyzer, ConsoleColor.Red, 35, """
            Missing clause or potential clause mismatch.
            """)
        .WithSourceLine(Current(), """
            The 'IS' clause must only be followed by EXTERNAL, GLOBAL or TYPEDEF.
            """)
        .CloseError();
    }

    private static void FileEntryClauses(DataEntry fileLocal)
    {
        if (CurrentEquals("IS") && !LookaheadEquals(1, "EXTERNAL", "GLOBAL"))
        {
            IsClauseErrorCheck();
        }

        if ((CurrentEquals("IS") && LookaheadEquals(1, "EXTERNAL")) || CurrentEquals("EXTERNAL"))
        {
            ExternalClause(fileLocal);
        }

        if ((CurrentEquals("IS") && LookaheadEquals(1, "GLOBAL")) || CurrentEquals("GLOBAL"))
        {
            GlobalClause(fileLocal);
        }

        if (CurrentEquals("FORMAT"))
        {
            // TODO: This needs to be fixed later
            Expected("FORMAT");
            Choice("BIT", "CHARACTER", "NUMERIC");
            Optional("DATA");
        }

        if (CurrentEquals("BLOCK"))
        {
            Expected("BLOCK");
            Optional("CONTAINS");
            
            if (LookaheadEquals(1, "TO"))
            {
                Numeric();
                Expected("TO");
            }

            Numeric();
            Choice("CHARACTERS", "RECORDS");
        }

        if (CurrentEquals("RECORD"))
        {
            RecordClause(fileLocal);
        }

        if (CurrentEquals("LINAGE"))
        {
            LinageClause(fileLocal);
        }

        if (CurrentEquals("CODE-SET"))
        {
            CodeSetClause(fileLocal);
        }

        if (CurrentEquals("REPORT", "REPORTS"))
        {
            ReportsClause(fileLocal);
        }
    }

    private static void DataEntryClauses(DataEntry dataLocal)
    {
        if (CurrentEquals("IS") && !LookaheadEquals(1, "EXTERNAL", "GLOBAL", "TYPEDEF"))
        {
            IsClauseErrorCheck();
        }

        if ((CurrentEquals("IS") && LookaheadEquals(1, "EXTERNAL")) || CurrentEquals("EXTERNAL"))
        {
            ExternalClause(dataLocal);
        }

        if ((CurrentEquals("IS") && LookaheadEquals(1, "GLOBAL")) || CurrentEquals("GLOBAL"))
        {
            GlobalClause(dataLocal);
        }

        if ((CurrentEquals("IS") && LookaheadEquals(1, "TYPEDEF")) || CurrentEquals("TYPEDEF"))
        {
            TypedefClause(dataLocal);
        }

        if (CurrentEquals("REDEFINES"))
        {
            RedefinesClause(dataLocal);
        }

        if (CurrentEquals("ALIGNED"))
        {
            AlignedClause(dataLocal);
        }

        if (CurrentEquals("ANY") && LookaheadEquals(1, "LENGTH"))
        {
            AnyLengthClause(dataLocal);
        }

        if (CurrentEquals("BASED"))
        {
            BasedClause(dataLocal);
        }

        if (CurrentEquals("BLANK"))
        {
            BlankWhenClause(dataLocal);
        }

        if (CurrentEquals("CONSTANT") && LookaheadEquals(1, "RECORD"))
        {
            ConstantRecordClause(dataLocal);
        }

        if (CurrentEquals("DYNAMIC"))
        {
            DynamicClause(dataLocal);
        }

        if (CurrentEquals("GROUP-USAGE"))
        {
            GroupUsageClause(dataLocal);
        }

        if (CurrentEquals("JUSTIFIED", "JUST"))
        {
            JustifiedClause(dataLocal);
        }

        if (CurrentEquals("SYNCHRONIZED", "SYNC"))
        {
            SynchronizedClause(dataLocal);
        }

        if (CurrentEquals("PROPERTY"))
        {
            PropertyClause(dataLocal);
        }

        if (CurrentEquals("SAME"))
        {
            SameAsClause(dataLocal);
        }

        if (CurrentEquals("TYPE"))
        {
            TypeClause(dataLocal);
        }

        if (CurrentEquals("OCCURS"))
        {
            OccursClause(dataLocal);
        }

        if (CurrentEquals("PIC", "PICTURE"))
        {
            PictureClause(dataLocal);
        }

        if (CurrentEquals("VALUE"))
        {
            ValueClause(dataLocal);
        }

        if (CurrentEquals("USAGE"))
        {
            UsageClause(dataLocal);
        }
    }

    private static void ReportEntryClauses()
    {
        if ((CurrentEquals("IS") && LookaheadEquals(1, "GLOBAL")) || CurrentEquals("GLOBAL"))
        {
            Optional("IS");
            Expected("GLOBAL");
        }

        if (CurrentEquals("CODE"))
        {
            Expected("CODE");
            Optional("IS");
            Common.IdentifierOrLiteral(TokenType.String);
        }

        if (CurrentEquals("CONTROL", "CONTROLS"))
        {
            if (CurrentEquals("CONTROL"))
            {
                Expected("CONTROL");
                Optional("IS");
            }
            else
            {
                Expected("CRONTROLS");
                Optional("ARE");
            }

            if (CurrentEquals("FINAL"))
            {
                Expected("FINAL");

                while (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }

                return;
            }

            Identifier();
            while (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }
        }
    
        if (CurrentEquals("PAGE"))
        {
            Expected("PAGE");

            if (CurrentEquals("LIMIT"))
            {
                Optional("LIMIT");
                Optional("IS");
            }
            else
            {
                Optional("LIMITS");
                Optional("ARE");
            }

            Numeric();

            if (CurrentEquals("LINE", "LINES"))
            {
                Choice("LINE", "LINES");
            }

            if (LookaheadEquals(1, "COL", "COLUMNS") && !LookaheadEquals(-1, TokenType.Numeric))
            {
                Numeric();
                Choice("COL", "COLUMNS");
            }
            else if (CurrentEquals("COL", "COLUMNS") && LookaheadEquals(-1, TokenType.Numeric))
            {
                Choice("COL", "COLUMNS");
            }

            if (CurrentEquals("HEADING"))
            {
                Expected("HEADING");
                Optional("IS");
                Numeric();
            }

            if (CurrentEquals("FIRST"))
            {
                Expected("FIRST");
                Choice("DETAIL", "DE");
                Optional("IS");
                Numeric();
            }

            if (CurrentEquals("LAST") && LookaheadEquals(1, "CONTROL", "CH"))
            {
                Expected("LAST");
                if (CurrentEquals("CONTROL"))
                {
                    Expected("CONTROL");
                    Expected("HEADING");
                }
                else
                {
                    Expected("CH");
                }

                Optional("IS");
                Numeric();
            }

            if (CurrentEquals("LAST") && LookaheadEquals(1, "DETAIL", "DE"))
            {
                Expected("LAST");
                Choice("DETAIL", "DE");
                Optional("IS");
                Numeric();
            }

            if (CurrentEquals("FOOTING"))
            {
                Expected("FOOTING");
                Optional("IS");
                Numeric();
            }
        }
    }

    private static void ReportGroupClauses(DataEntry reportEntry)
    {
        if (CurrentEquals("TYPE"))
        {
            ReportTypeClause(reportEntry);
        }

        if (CurrentEquals("NEXT"))
        {
            Expected("NEXT");
            Expected("GROUP");
            Optional("IS");

            if (CurrentEquals("NEXT"))
            {
                Expected("NEXT");
                Expected("PAGE");

                if (CurrentEquals("WITH", "RESET"))
                {
                    Optional("WITH");
                    Expected("RESET");
                }
            }
            else if (CurrentEquals("+", "PLUS"))
            {
                Choice("+", "PLUS");
                Numeric();
            }
            else
            {
                Numeric();
            }
        }

        if (CurrentEquals("LINE", "LINES"))
        {
            if (CurrentEquals("LINES"))
            {
                Expected("LINES");
                Optional("ARE");
            }
            else
            {
                Expected("LINE");
                if (CurrentEquals("NUMBER"))
                {
                    Optional("NUMBER");
                    Optional("IS");
                }
                else
                {
                    Optional("NUMBERS");
                    Optional("ARE");
                }
            }

            while (CurrentEquals(TokenType.Numeric) || CurrentEquals("+", "PLUS", "ON", "NEXT"))
            {
                ReportLineClauseLoop();
            }
        }

        if (CurrentEquals("COLUMN", "COLUMNS", "COL", "COLS"))
        {
            if (CurrentEquals("COLUMN"))
            {
                Expected("COLUMN");
                OptionalChoice("NUMBER", "NUMBERS");
            }
            else if (CurrentEquals("COLUMNS"))
            {
                Expected("COLUMNS");
            }
            else if (CurrentEquals("COL"))
            {
                Expected("COL");
                OptionalChoice("NUMBER", "NUMBERS");
            }
            else
            {
                Expected("COLS");
            }

            if (CurrentEquals("CENTER", "RIGHT"))
            {
                Choice("CENTER", "RIGHT");
            }
            else
            {
                Optional("LEFT");
            }

            OptionalChoice("IS", "ARE");

            while(CurrentEquals(TokenType.Numeric) || CurrentEquals("+", "PLUS"))
            {
                if (CurrentEquals("+", "PLUS"))
                {
                    Choice("+", "PLUS");
                    Numeric();
                }
                else
                {
                    Numeric();
                }
            }
        }

        if (CurrentEquals("PICTURE", "PIC"))
        {
            PictureClause(reportEntry);
        }

        if (CurrentEquals("SIGN"))
        {
            SignClause(reportEntry);
        }

        if (CurrentEquals("JUSTIFIED", "JUST"))
        {
            JustifiedClause(reportEntry);
        }

        if (CurrentEquals("BLANK"))
        {
            BlankWhenClause(reportEntry);
        }

        if (CurrentEquals("PRESENT"))
        {
            Expected("PRESENT");
            Expected("WHEN");
            Common.Condition(TokenContext.IsClause);
        }

        if (CurrentEquals("GROUP"))
        {
            Expected("GROUP");
            Optional("INDICATE");
        }

        if (CurrentEquals("OCCURS"))
        {
            Expected("OCCURS");
            if (LookaheadEquals(1, "TO"))
            {
                Numeric();
                Expected("TO");
            }

            Numeric();
            Optional("TIMES");

            if (CurrentEquals("DEPENDING"))
            {
                Expected("DEPENDING");
                Optional("ON");
                Identifier();
            }

            if (CurrentEquals("STEP"))
            {
                Expected("STEP");
                Numeric();
            }
        }

        if (CurrentEquals("USAGE"))
        {
            Expected("USAGE");
            Optional("IS");

            Choice("DISPLAY", "NATIONAL");
        }
        
        if (CurrentEquals("SUM"))
        {
            while (CurrentEquals("SUM"))
            {
                Expected("SUM");
                Optional("OF");

                // TODO: 
                // This clause has other formats
                // that require identifier resolution
                Common.Arithmetic(TokenContext.IsClause);

                if (CurrentEquals("UPON"))
                {
                    Expected("UPON");
                    Identifier();

                    while(CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                }
            }

            if (CurrentEquals("RESET"))
            {
                Expected("RESET");
                Optional("ON");
                if (CurrentEquals("FINAL"))
                {
                    Expected("FINAL");
                }
                else
                {
                    Identifier();
                }
            }

            if (CurrentEquals("ROUNDED"))
            {
                Common.RoundedPhrase();
            }
        }

        if (CurrentEquals("SOURCE", "SOURCES"))
        {
            if (CurrentEquals("SOURCES"))
            {
                Expected("SOURCES");
                Optional("ARE");
            }
            else
            {
                Expected("SOURCE");
                Optional("IS");
            }

            Common.Arithmetic(TokenContext.IsClause);

            if (CurrentEquals("ROUNDED"))
            {
                Common.RoundedPhrase();
            }
        }

        if (CurrentEquals("VALUE", "VALUES"))
        {
            if (CurrentEquals("VALUE"))
            {
                Expected("VALUE");
                Optional("IS");
            }
            else
            {
                Expected("VALUE");
                Optional("ARE");
            }
            
            StringLiteral();

            while (CurrentEquals(TokenType.String))
            {
                StringLiteral();
            }
        }

        if (CurrentEquals("VARYING"))
        {
            Expected("VARYING");

            while (CurrentEquals(TokenType.Identifier))
            {
                Identifier();

                if (CurrentEquals("FROM"))
                {
                    Expected("FROM");
                    Common.Arithmetic();
                }

                if (CurrentEquals("BY"))
                {
                    Expected("BY");
                    Common.Arithmetic();
                }
            }
        }
    }

    private static void ReportLineClauseLoop()
    {
        if (CurrentEquals("ON", "NEXT"))
        {
            Optional("ON");
            Expected("NEXT");
            Expected("PAGE");
        }
        else if (CurrentEquals("+", "PLUS"))
        {
            Choice("+", "PLUS");
            Numeric();
        }
        else
        {
            Numeric();
            if (CurrentEquals("ON", "NEXT"))
            {
                Optional("ON");
                Expected("NEXT");
                Expected("PAGE");
            }
        }
    }

    private static void ScreenEntryClauses(DataEntry screenLocal)
    {
        if ((CurrentEquals("IS") && LookaheadEquals(1, "GLOBAL")) || CurrentEquals("GLOBAL"))
        {
            GlobalClause(screenLocal);
        }

        if (CurrentEquals("LINE"))
        {
            LineClause(screenLocal);
        }

        if (CurrentEquals("COLUMN", "COL"))
        {
            ColumnClause(screenLocal);
        }

        if (CurrentEquals("PICTURE", "PIC"))
        {
            PictureClause(screenLocal);
        }

        if (CurrentEquals("BLANK") && LookaheadEquals(1, "SCREEN", "LINE"))
        {
            Expected("BLANK");
            Choice("SCREEN", "LINE");
        }

        if (CurrentEquals("BLANK") && !LookaheadEquals(1, "SCREEN", "LINE"))
        {
            BlankWhenClause(screenLocal);
        }

        if (CurrentEquals("JUSTIFIED", "JUST"))
        {
            JustifiedClause(screenLocal);
        }

        if (CurrentEquals("SIGN"))
        {
            SignClause(screenLocal);
        }

        if (CurrentEquals("FULL"))
        {
            Expected("FULL");
        }

        if (CurrentEquals("AUTO"))
        {
            Expected("AUTO");
        }

        if (CurrentEquals("SECURE"))
        {
            Expected("SECURE");
        }

        if (CurrentEquals("REQUIRED"))
        {
            Expected("REQUIRED");
        }

        if (CurrentEquals("BELL"))
        {
            Expected("BELL");
        }

        if (CurrentEquals("HIGHLIGHT", "LOWLIGHT"))
        {
            Choice("HIGHLIGHT", "LOWLIGHT");
        }

        if (CurrentEquals("REVERSE-VIDEO"))
        {
            Expected("REVERSE-VIDEO");
        }

        if (CurrentEquals("UNDERLINE"))
        {
            Expected("UNDERLINE");
        }

        if (CurrentEquals("FOREGROUND-COLOR"))
        {
            Expected("FOREGROUND-COLOR");
            Optional("IS");
            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }
            else
            {
                Numeric();
            }
        }

        if (CurrentEquals("BACKGROUND-COLOR"))
        {
            Expected("BACKGROUND-COLOR");
            Optional("IS");
            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }
            else
            {
                Numeric();
            }
        }

        if (CurrentEquals("OCCURS"))
        {
            Expected("OCCURS");
            Numeric();

            Optional("TIMES");
        }

        if (CurrentEquals("USAGE"))
        {
            Expected("USAGE");
            Optional("IS");

            Choice("DISPLAY", "NATIONAL");
        }

        ScreenValueClause(screenLocal);
    }

    private static void LinageClause(DataEntry fileLocal)
    {
        Expected("LINAGE");
        Optional("IS");

        fileLocal[DataClause.Linage] = true;

        Common.IdentifierOrLiteral(TokenType.Numeric);
        Optional("LINES");

        if (CurrentEquals("WITH", "FOOTING"))
        {
            Optional("WITH");
            Expected("FOOTING");
            Common.IdentifierOrLiteral(TokenType.Numeric);
        }

        if (CurrentEquals("LINES", "AT", "TOP"))
        {
            Optional("LINES");
            Optional("AT");
            Expected("TOP");
            Common.IdentifierOrLiteral(TokenType.Numeric);
        }

        if (CurrentEquals("LINES", "AT", "BOTTOM"))
        {
            Optional("LINES");
            Optional("AT");
            Expected("BOTTOM");
            Common.IdentifierOrLiteral(TokenType.Numeric);
        }
    }

    private static void RecordClause(DataEntry fileLocal)
    {
        Expected("RECORD");

        fileLocal[DataClause.Record] = true;

        if (CurrentEquals("IS", "VARYING"))
        {
            Optional("IS");
            Expected("VARYING");
            Optional("IN");
            Optional("SIZE");

            if (CurrentEquals("FROM") || CurrentEquals(TokenType.Numeric))
            {
                Optional("FROM");
                Numeric();
            }

            if (CurrentEquals("TO"))
            {
                Optional("TO");
                Numeric();
            }
            
            if (CurrentEquals("BYTES", "CHARACTERS"))
            {
                Expected(Current().Value);
            }

            if (CurrentEquals("DEPENDING"))
            {
                Expected("DEPENDING");
                Optional("ON");
                Identifier();
            }

            return;
        }

        // If the record is not varying in size
        Optional("CONTAINS");
        
        if (!LookaheadEquals(1, "TO"))
        {
            Numeric();

            if (CurrentEquals("BYTES", "CHARACTERS"))
            {
                Expected(Current().Value);
            }

            return;
        }

        // If the record is fixed-or-variable
        Numeric();

        Expected("TO");

        Numeric();

        if (CurrentEquals("BYTES", "CHARACTERS"))
        {
            Expected(Current().Value);
        }
    }

    private static void ReportsClause(DataEntry fileLocal)
    {
        Choice("REPORT", "REPORTS");

        fileLocal[DataClause.Report] = true;

        if (LookaheadEquals(-1, "REPORT"))
        {
            Optional("IS");
        }
        else
        {
            Optional("ARE");
        }

        Identifier();

        while (CurrentEquals(TokenType.Identifier))
        {
            Identifier();
        }
    }

    private static void CodeSetClause(DataEntry fileLocal)
    {
        Expected("CODE-SET");

        fileLocal[DataClause.CodeSet] = true;

        if (CurrentEquals("FOR", "ALPHANUMERIC", "NATIONAL"))
        {
            Common.ForAlphanumericForNational();
            return;
        }

        Optional("IS");
        Identifier();

        if (CurrentEquals(TokenType.Identifier))
        {
            Identifier();
        } 
    }

    private static void ScreenValueClause(DataEntry screenLocal)
    {
        if (CurrentEquals("FROM"))
        {
            Expected("FROM");
            Common.IdentifierOrLiteral();

            screenLocal[DataClause.From] = true;

            return;
        }

        if (CurrentEquals("TO"))
        {
            Expected("TO");
            Identifier();

            screenLocal[DataClause.To] = true;

            return;
        }

        if (CurrentEquals("USING"))
        {
            Expected("USING");
            Identifier();

            screenLocal[DataClause.Using] = true;

            return;
        }

        if (CurrentEquals("VALUE"))
        {
            Expected("VALUE");
            Optional("IS");

            screenLocal[DataClause.Value] = true;

            Common.ParseLiteral(true, true);
        }
    }

    private static void LineClause(DataEntry screenLocal)
    {
        Expected("LINE");
        Optional("NUMBER");
        Optional("IS");

        screenLocal[DataClause.Line] = true;

        if (CurrentEquals("PLUS", "+", "MINUS", "-"))
        {
            Expected(Current().Value);
        }

        if (CurrentEquals(TokenType.Identifier))
        {
            Identifier();
        }
        else
        {
            Numeric();
        }
    }

    private static void ColumnClause(DataEntry screenLocal)
    {
        Choice("COLUMN", "COL");
        Optional("NUMBER");
        Optional("IS");

        screenLocal[DataClause.Column] = true;

        if (CurrentEquals("PLUS", "+", "MINUS", "-"))
        {
            Expected(Current().Value);
        }

        if (CurrentEquals(TokenType.Identifier))
        {
            Identifier();
        }
        else
        {
            Numeric();
        }
    }

    private static void SignClause(DataEntry entryLocal)
    {
        Expected("SIGN");
        Optional("IS");

        entryLocal[DataClause.Sign] = true;

        Choice("LEADING", "TRAILING");

        if (CurrentEquals("SEPARATE"))
        {
            Expected("SEPARATE");
            Optional("CHARACTER");
        }
    }

    private static void ExternalClause(DataEntry entryLocal)
    {
        Optional("IS");
        Expected("EXTERNAL");
        if (CurrentEquals("AS"))
        {
            Expected("AS");

            entryLocal[DataClause.External] = true;

            entryLocal.ExternalizedName = Current().Value;

            StringLiteral();
        }

        if (!CurrentEquals("AS"))
        {
            entryLocal[DataClause.External] = true;

            entryLocal.ExternalizedName = Current().Value;
        }
    }

    private static void GlobalClause(DataEntry entryLocal)
    {
        Optional("IS");
        Expected("GLOBAL");

        entryLocal[DataClause.Global] = true;
    }

    private static void TypedefClause(DataEntry entryLocal)
    {
        Optional("IS");
        Expected("TYPEDEF");
        
        entryLocal[DataClause.Typedef] = true;

        if (CurrentEquals("STRONG")) Expected("STRONG");
    }

    private static void RedefinesClause(DataEntry entryLocal)
    {
        Expected("REDEFINES");
        Identifier();

        entryLocal[DataClause.Redefines] = true;
    }

    private static void AlignedClause(DataEntry entryLocal)
    {
        Expected("ALIGNED");

        entryLocal[DataClause.Aligned] = true;
    }

    private static void AnyLengthClause(DataEntry entryLocal)
    {
        Expected("ANY");
        Expected("LENGTH");

        entryLocal[DataClause.AnyLength] = true;
    }

    private static void BasedClause(DataEntry entryLocal)
    {
        Expected("BASED");

        entryLocal[DataClause.Based] = true;
    }

    private static void BlankWhenClause(DataEntry entryLocal)
    {
        Expected("BLANK");
        Optional("WHEN");
        Expected("ZERO");

        entryLocal[DataClause.BlankWhenZero] = true;
    }

    private static void ConstantRecordClause(DataEntry entryLocal)
    {
        Expected("CONSTANT");
        Expected("RECORD");

        entryLocal[DataClause.ConstantRecord] = true;
    }

    private static void DynamicClause(DataEntry entryLocal)
    {
        Expected("DYNAMIC");
        Optional("LENGTH");
        
        entryLocal[DataClause.DynamicLength] = true;

        if (CurrentEquals(TokenType.Identifier)) Identifier();

        if (CurrentEquals("LIMIT"))
        {
            Expected("LIMIT");
            Optional("IS");
            Numeric();
        }
    }

    private static void GroupUsageClause(DataEntry entryLocal)
    {
        Expected("GROUP-USAGE");
        Optional("IS");
        Choice("BIT", "NATIONAL");

        entryLocal[DataClause.GroupUsage] = true;
    }

    private static void JustifiedClause(DataEntry entryLocal)
    {
        Choice("JUSTIFIED", "JUST");
        Optional("RIGHT");

        entryLocal[DataClause.Justified] = true;
    }

    private static void SynchronizedClause(DataEntry entryLocal)
    {
        Choice("SYNCHRONIZED", "SYNC");

        entryLocal[DataClause.Synchronized] = true;

        if (CurrentEquals("LEFT"))
        {
            Expected("LEFT");
        } 
        else if (CurrentEquals("RIGHT")) 
        {
            Expected("RIGHT");
        }
    }

    private static void PropertyClause(DataEntry entryLocal)
    {
        Expected("PROPERTY");

        entryLocal[DataClause.Property] = true;

        if (CurrentEquals("WITH", "NO"))
        {
            Optional("WITH");
            Expected("NO");
            Choice("GET", "SET");
        }

        if (CurrentEquals("IS", "FINAL"))
        {
            Optional("IS");
            Expected("FINAL");
        }
    }

    private static void SameAsClause(DataEntry entryLocal)
    {
        Expected("SAME");
        Expected("AS");
        Identifier();

        entryLocal[DataClause.SameAs] = true;
    }

    private static void TypeClause(DataEntry entryLocal)
    {
        Expected("TYPE");
        Identifier();

        entryLocal[DataClause.Type] = true;
    }

    private static void ReportTypeClause(DataEntry reportLocal)
    {
        Expected("TYPE");
        Optional("IS");

        reportLocal[DataClause.Type] = true;

        if (CurrentEquals("REPORT"))
        {
            Expected("REPORT");
            Choice("HEADING", "FOOTING");
        }

        if (CurrentEquals("PAGE"))
        {
            Expected("PAGE");
            Choice("HEADING", "FOOTING");
        }

        if (CurrentEquals("DETAIL", "DE"))
        {
            Choice("DETAIL", "DE");
        }

        if (CurrentEquals("CONTROL", "CH", "CF"))
        {
            var isControlHeading = false;

            if (CurrentEquals("CONTROL") && LookaheadEquals(1, "HEADING"))
            {
                Expected("CONTROL");
                Expected("HEADING");
                isControlHeading = true;
            }
            else if (CurrentEquals("CONTROL") && LookaheadEquals(1, "FOOTING"))
            {
                Expected("CONTROL");
                Expected("FOOTING");
            }
            else if (CurrentEquals("CH"))
            {
                Expected("CH");
                isControlHeading = true;
            }
            else if (CurrentEquals("CF"))
            {
                Expected("CF");
            }

            if (CurrentEquals("OR", "FOR", "FINAL") || CurrentEquals(TokenType.Identifier))
            {
                OptionalChoice("ON", "FOR");
                if (CurrentEquals("FINAL"))
                {
                    Expected("FINAL");
                }
                else
                {
                    Identifier();
                }

                if (isControlHeading && CurrentEquals("OR"))
                {
                    Expected("OR");
                    Expected("PAGE");
                }
            }
        }

        if (CurrentEquals("RH", "RF", "PH", "PF"))
        {
            Expected(Current().Value);
        }
    }

    private static void OccursClause(DataEntry entryLocal)
    {
        Expected("OCCURS");

        entryLocal[DataClause.Occurs] = true;

        if (CurrentEquals("DYNAMIC"))
        {
            Expected("DYNAMIC");

            if (CurrentEquals("CAPACITY"))
            {
                Expected("CAPACITY");
                Optional("IN");
                Identifier();
            }

            if (CurrentEquals("FROM"))
            {
                Expected("FROM");
                Numeric();
            }

            if (CurrentEquals("TO"))
            {
                Expected("TO");
                Numeric();
            }

            if (CurrentEquals("INITIALIZED"))
            {
                Expected("INITIALIZED");
            }

            Common.AscendingDescendingKey();

            if (CurrentEquals("INDEXED"))
            {
                IndexedBy();
            }

            return;
        }

        if (LookaheadEquals(1, "TO"))
        {
            Numeric();
            Expected("TO");

            Numeric();
            Optional("TIMES");

            Expected("DEPENDING");
            Optional("ON");

            Identifier();

            Common.AscendingDescendingKey();

            if (CurrentEquals("INDEXED"))
            {
                IndexedBy();
            }

            return;
        }

        Numeric();
        Optional("TIMES");

        Common.AscendingDescendingKey();

        if (CurrentEquals("INDEXED"))
        {
            IndexedBy();
        }

        static void IndexedBy()
        {
            Expected("INDEXED");
            Optional("BY");

            Identifier();
            while (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }
        }
    }

    private static void PictureClause(DataEntry entryLocal)
    {
        Choice("PIC", "PICTURE");
        Optional("IS");

        var picture = Current();

        var isValidPicture = PictureString(picture.Value, out var size);

        entryLocal[DataClause.Picture] = true;

        Continue();
    }

    private static void ValueClause(DataEntry entryLocal)
    {
        Expected("VALUE");

        entryLocal[DataClause.Value] = true;

        if (!CurrentEquals(TokenType.String, TokenType.Numeric))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 2, """
                Unexpected token.
                """)
            .WithSourceLine(Current(), """
                Expected a string or numeric literal.
                """)
            .CloseError();
        }

        if (CurrentEquals(TokenType.String))
        {
            StringLiteral();
        }

        if (CurrentEquals(TokenType.Numeric))
        {
            Numeric();
        }
    }

    private static void UsageClause(DataEntry entryLocal)
    {
        Expected("USAGE");
        Optional("IS");

        entryLocal[DataClause.Usage] = true;

        if (CurrentEquals("BINARY"))
        {
            Expected("BINARY");

            entryLocal.Usage = UsageType.Binary;
            return;
        }

        if (CurrentEquals("BINARY-CHAR", "BINARY-SHORT", "BINARY-LONG", "BINARY-DOUBLE"))
        {
            Expected(Current().Value);
            if (CurrentEquals("SIGNED"))
            {
                Expected("SIGNED");
            }
            else if (CurrentEquals("UNSIGNED"))
            {
                Expected("UNSIGNED");
            }
            
            return;  
        }

        if (CurrentEquals("BIT"))
        {
            Expected("BIT");

            entryLocal.Usage = UsageType.Bit;
            return; 
        }

        if (CurrentEquals("COMP", "COMPUTATIONAL"))
        {
            Expected(Current().Value);

            entryLocal.Usage = UsageType.Computational;
            return;
        }

        if (CurrentEquals("DISPLAY"))
        {
            Expected("DISPLAY");

            entryLocal.Usage = UsageType.Display;
            return; 
        }

        if (CurrentEquals("FLOAT-BINARY-32"))
        {
            Expected("FLOAT-BINARY-32");
            Choice("HIGH-ORDER-LEFT", "HIGH-ORDER-RIGHT");
            
            entryLocal.Usage = UsageType.FloatBinary32;
            return; 
        }

        if (CurrentEquals("FLOAT-BINARY-64"))
        {
            Expected("FLOAT-BINARY-64");
            Choice("HIGH-ORDER-LEFT", "HIGH-ORDER-RIGHT");
            
            entryLocal.Usage = UsageType.FloatBinary64;
            return; 
        }

        if (CurrentEquals("FLOAT-BINARY-128"))
        {
            Expected("FLOAT-BINARY-128");
            Choice("HIGH-ORDER-LEFT", "HIGH-ORDER-RIGHT");
            
            entryLocal.Usage = UsageType.FloatBinary128;
            return; 
        }

        if (CurrentEquals("FLOAT-DECIMAL-16"))
        {
            Expected("FLOAT-DECIMAL-16");
            Common.EncodingEndianness();
            
            entryLocal.Usage = UsageType.FloatDecimal16;
            return; 
        }

        if (CurrentEquals("FLOAT-DECIMAL-32"))
        {
            Expected("FLOAT-DECIMAL-32");
            Common.EncodingEndianness();
            
            entryLocal.Usage = UsageType.FloatDecimal32;
            return; 
        }

        if (CurrentEquals("FLOAT-EXTENDED"))
        {
            Expected("FLOAT-EXTENDED");
            
            entryLocal.Usage = UsageType.FloatExtended;
            return; 
        }

        if (CurrentEquals("FLOAT-LONG"))
        {
            Expected("FLOAT-LONG");
            
            entryLocal.Usage = UsageType.FloatLong;
            return; 
        }

        if (CurrentEquals("FLOAT-SHORT"))
        {
            Expected("FLOAT-SHORT");
            
            entryLocal.Usage = UsageType.FloatShort;
            return; 
        }

        if (CurrentEquals("INDEX"))
        {
            Expected("INDEX");
            
            entryLocal.Usage = UsageType.Index;
            return; 
        }

        if (CurrentEquals("MESSAGE-TAG"))
        {
            Expected("MESSAGE-TAG");
            
            entryLocal.Usage = UsageType.MessageTag;
            return; 
        }

        if (CurrentEquals("NATIONAL"))
        {
            Expected("NATIONAL");
            
            entryLocal.Usage = UsageType.National;
            return; 
        }

        if (CurrentEquals("OBJECT"))
        {
            Expected("OBJECT");
            Expected("REFERENCE");
            // var isFactory = false;
            // var isStronglyTyped = false;

            // Need implement identifier resolution first
            // To parse the rest of this using clause correctly
            if (CurrentEquals("Factory"))
            {
                Expected("FACTORY");
                Optional("OF");
                // isFactory = true;
            }

            if (CurrentEquals("ACTIVE-CLASS"))
            {
                Expected("ACTIVE-CLASS");
                return;
            }

            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }

            if (CurrentEquals("ONLY"))
            {
                Expected("ONLY");
                // isStronglyTyped = true
            }

            entryLocal.Usage = UsageType.ObjectReference;
            return; 
        }

        if (CurrentEquals("POINTER"))
        {
            Expected("POINTER");

            if (CurrentEquals("TO") || CurrentEquals(TokenType.Identifier))
            {
                Optional("TO");
                Identifier();
            }

            entryLocal.Usage = UsageType.DataPointer;
            return; 
        }

        if (CurrentEquals("FUNCTION-POINTER"))
        {
            Expected("FUNCTION-POINTER");
            Optional("TO");
            
            Identifier();

            entryLocal.Usage = UsageType.FunctionPointer;
            return; 
        }

        if (CurrentEquals("PROGRAM-POINTER"))
        {
            Expected("PROGRAM-POINTER");

            if (CurrentEquals("TO") || CurrentEquals(TokenType.Identifier))
            {
                Optional("TO");
                
                Identifier();
            }
            
            entryLocal.Usage = UsageType.ProgramPointer;
            return; 
        }

        ErrorHandler
        .Build(ErrorType.Analyzer, ConsoleColor.Red, 50, """
            Unrecognized USAGE clause.
            """)
        .WithSourceLine(Lookahead(-1))
        .WithNote("""
            This could be due to an unsupported third-party extension.
            """)
        .CloseError();

        AnchorPoint(TokenContext.IsClause);
    }
}