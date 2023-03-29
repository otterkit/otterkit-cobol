namespace Otterkit;

public static partial class Analyzer
{
    /// <summary>
    /// Stack int <c>LevelStack</c> is used in the parser whenever it needs to know which data item level it is currently parsing.
    /// <para>This is used when handling the level number syntax rules, like which clauses are allowed for a particular level number or group item level number rules</para>
    /// </summary>
    private static readonly Stack<int> LevelStack = new();

    /// <summary>
    /// Stack string <c>GroupStack</c> is used in the parser whenever it needs to know which group the current data item belongs to.
    /// <para>This is used when handling the group item syntax rules, like which data items belong to which groups</para>
    /// </summary>
    private static readonly Stack<string> GroupStack = new();

    // Method responsible for parsing the DATA DIVISION.
    // That includes the FILE, WORKING-STORAGE, LOCAL-STORAGE, LINKAGE, REPORT and SCREEN sections.
    // It is also responsible for showing appropriate error messages when an error occurs in the DATA DIVISION.
    private static void DATA()
    {
        Expected("DATA");
        Expected("DIVISION");
        CurrentSection = CurrentScope.DataDivision;

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

            AnchorPoint("WORKING-STORAGE", "LOCAL-STORAGE", "LINKAGE", "PROCEDURE");
        }

        if (CurrentEquals("WORKING-STORAGE"))
            WorkingStorage();

        if (CurrentEquals("LOCAL-STORAGE"))
            LocalStorage();

        if (CurrentEquals("LINKAGE"))
            LinkageSection();
    }


    // The following methods are responsible for parsing the DATA DIVISION sections
    // They are technically only responsible for parsing the section header, 
    // the Entries() method handles parsing the actual data items in their correct sections.
    private static void WorkingStorage()
    {
        Expected("WORKING-STORAGE");
        Expected("SECTION");
        CurrentSection = CurrentScope.WorkingStorage;

        Expected(".");
        while (Current().Type == TokenType.Numeric)
            Entries();
    }

    private static void LocalStorage()
    {
        Expected("LOCAL-STORAGE");
        Expected("SECTION");
        CurrentSection = CurrentScope.LocalStorage;

        Expected(".");
        while (Current().Type is TokenType.Numeric)
            Entries();
    }

    private static void LinkageSection()
    {
        Expected("LINKAGE");
        Expected("SECTION");
        CurrentSection = CurrentScope.LinkageSection;

        Expected(".");
        while (Current().Type is TokenType.Numeric)
            Entries();
    }


    // The following methods are responsible for parsing the DATA DIVISION data items
    // The Entries() method is responsible for identifying which kind of data item to 
    // parse based on it's level number.

    // The RecordEntry(), BaseEntry(), and ConstantEntry() are then responsible for correctly
    // parsing each data item, or in the case of the RecordEntry() a group item or 01-level elementary item.
    private static void Entries()
    {
        if (CurrentEquals("77"))
            BaseEntry();

        if ((CurrentEquals("01") || CurrentEquals("1")) && !LookaheadEquals(2, "CONSTANT"))
            RecordEntry();

        if (LookaheadEquals(2, "CONSTANT"))
            ConstantEntry();
    }

    private static void RecordEntry()
    {
        BaseEntry();
        _ = int.TryParse(Current().Value, out int outInt);
        while (outInt > 1 && outInt < 50)
        {
            BaseEntry();
            _ = int.TryParse(Current().Value, out outInt);
        }

        LevelStack.Clear();
        GroupStack.Clear();
    }

    private static void BaseEntry()
    {
        int levelNumber = int.Parse(Current().Value);
        Number();

        Token itemToken = Current();
        string dataName = itemToken.Value;

        CheckLevelNumber(levelNumber);
        
        Identifier();

        if (SymbolTable.DataLocals.ReferenceExists(dataName) && levelNumber is 1)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 30,"""
                Duplicate root level definition.
                """)
            .WithSourceLine(Current(), """
                A 01 level variable already exists with this name
                """)
            .WithNote("""
                Every root level item must have a unique name. 
                """)
            .CloseError();
        }


        DataSignature dataReference = new();

        dataReference.Identifier = dataName;
        dataReference.LevelNumber = levelNumber;
        dataReference.Section = CurrentSection;

        if (GroupStack.Count == 0)
        {
            dataReference.Parent = "root";
        }
        else
        {
            dataReference.Parent = GroupStack.Peek();
        }

        if (!CurrentEquals(TokenContext.IsClause) && !CurrentEquals("."))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 2,"""
                Unexpected token.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected data item clauses or a separator period after this token
                """)
            .CloseError();
        }

        while (CurrentEquals(TokenContext.IsClause))
        {
            if (CurrentEquals("IS") && !LookaheadEquals(1, "EXTERNAL", "GLOBAL", "TYPEDEF"))
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 35,"""
                    Missing clause or possible clause mismatch.
                    """)
                .WithSourceLine(Current(), """
                    The 'IS' clause must only be followed by EXTERNAL, GLOBAL or TYPEDEF.
                    """)
                .CloseError();
            }

            if ((CurrentEquals("IS") && LookaheadEquals(1, "EXTERNAL")) || CurrentEquals("EXTERNAL"))
            {
                Optional("IS");
                Expected("EXTERNAL");
                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    dataReference.IsExternal = true;
                    dataReference.ExternalName = Current().Value;

                    String();
                }

                if (!CurrentEquals("AS"))
                {
                    dataReference.IsExternal = true;
                    dataReference.ExternalName = Current().Value;
                }
            }

            if ((CurrentEquals("IS") && LookaheadEquals(1, "GLOBAL")) || CurrentEquals("GLOBAL"))
            {
                Optional("IS");
                Expected("GLOBAL");
                dataReference.IsGlobal = true;
            }

            if ((CurrentEquals("IS") && LookaheadEquals(1, "TYPEDEF")) || CurrentEquals("TYPEDEF"))
            {
                Optional("IS");
                Expected("TYPEDEF");
                dataReference.IsTypedef = true;

                if (CurrentEquals("STRONG")) Expected("STRONG");
            }

            if (CurrentEquals("REDEFINES"))
            {
                Expected("REDEFINES");
                Identifier();
                dataReference.IsRedefines = true;
            }

            if (CurrentEquals("ALIGNED")) Expected("ALIGNED");

            if (CurrentEquals("ANY") && LookaheadEquals(1, "LENGTH"))
            {
                Expected("ANY");
                Expected("LENGTH");
                dataReference.IsAnyLength = true;
            }

            if (CurrentEquals("BASED")) Expected("BASED");

            if (CurrentEquals("BLANK"))
            {
                Expected("BLANK");
                Optional("WHEN");
                Expected("ZERO");
                dataReference.IsBlank = true;
            }

            if (CurrentEquals("CONSTANT") && LookaheadEquals(1, "RECORD"))
            {
                Expected("CONSTANT");
                Expected("RECORD");
                dataReference.IsConstantRecord = true;
            }

            if (CurrentEquals("DYNAMIC"))
            {
                Expected("DYNAMIC");
                Optional("LENGTH");
                dataReference.IsDynamicLength = true;

                if (CurrentEquals(TokenType.Identifier)) Identifier();

                if (CurrentEquals("LIMIT"))
                {
                    Expected("LIMIT");
                    Optional("IS");
                    Number();
                }
            }

            if (CurrentEquals("GROUP-USAGE"))
            {
                Expected("GROUP-USAGE");
                Optional("IS");
                Choice("BIT", "NATIONAL");
            }

            if (CurrentEquals("JUSTIFIED", "JUST"))
            {
                Choice("JUSTIFIED", "JUST");
                Optional("RIGHT");
            }

            if (CurrentEquals("SYNCHRONIZED", "SYNC"))
            {
                Choice("SYNCHRONIZED", "SYNC");
                if (CurrentEquals("LEFT")) Expected("LEFT");

                else if (CurrentEquals("RIGHT")) Expected("RIGHT");
            }

            if (CurrentEquals("PROPERTY"))
            {
                Expected("PROPERTY");
                dataReference.IsProperty = true;
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

            if (CurrentEquals("SAME"))
            {
                Expected("SAME");
                Expected("AS");
                Identifier();
            }

            if (CurrentEquals("TYPE"))
            {
                Expected("TYPE");
                Identifier();
            }

            if (CurrentEquals("PIC", "PICTURE"))
            {
                Choice("PIC", "PICTURE");
                Optional("IS");

                var picture = Current();
            
                var isValidPicture = PictureString(picture.Value, out var size);

                dataReference.PictureString = picture.Value;

                dataReference.PictureLength = size;

                dataReference.HasPicture = true;

                Continue();
            }

            if (CurrentEquals("VALUE"))
            {
                Expected("VALUE");

                if (!CurrentEquals(TokenType.String, TokenType.Numeric))
                {
                    Error
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 2,"""
                        Unexpected token.
                        """)
                    .WithSourceLine(Current(), """
                        Expected a string or numeric literal.
                        """)
                    .CloseError();
                }

                if (CurrentEquals(TokenType.String))
                {
                    dataReference.DefaultValue = Current().Value;
                    String();
                }

                if (CurrentEquals(TokenType.Numeric))
                {
                    dataReference.DefaultValue = Current().Value;
                    Number();
                }
            }

            if (CurrentEquals("USAGE"))
            {
                UsageClause(dataReference);
            }

        }

        if (CurrentEquals(".") && LookaheadEquals(1, TokenType.Numeric))
        {
            if (LevelStack.Count == 0)
            {
                dataReference.IsElementary = true;
            }
            else
            {
                _ = int.TryParse(Lookahead(1).Value, out int outInt);
                var currentLevel = LevelStack.Peek();

                if (currentLevel == 1 && outInt >= 2 && outInt <= 49 || outInt >= 2 && outInt <= 49 && outInt > currentLevel)
                {
                    dataReference.IsGroup = true;
                }
                else
                {
                    dataReference.IsElementary = true;
                }
            }
        }

        CheckClauses(dataReference, itemToken);

        if (dataReference.IsGroup) GroupStack.Push(dataName);

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                Data item definition, missing separator period.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected a separator period '. ' after this token
                """)
            .WithNote("""
                Every item must end with a separator period
                """)
            .CloseError();
        }

        SymbolTable.DataLocals.AddOrUpdateReference(dataName, dataReference);

        CheckConditionNames(dataName);
    }

    private static void ConstantEntry()
    {
        if (!CurrentEquals("01") && !CurrentEquals("1"))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 40,"""
                Invalid level number.
                """)
            .WithSourceLine(Current(), """
                CONSTANT variables must have a level number of '1' or '01'.
                """)
            .CloseError();
        }

        var levelNumber = int.Parse(Current().Value);
        Number();

        var dataName = Current().Value;
        Identifier();

        if (SymbolTable.DataLocals.ReferenceExists(dataName))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 30,"""
                Duplicate root level definition.
                """)
            .WithSourceLine(Lookahead(-1), """
                A 01 level variable already exists with this name
                """)
            .WithNote("""
                Root level items must have a unique name. 
                """)
            .CloseError();
        }

        DataSignature dataReference = new();

        dataReference.Identifier = dataName;
        dataReference.LevelNumber = levelNumber;
        dataReference.Section = CurrentSection;
        dataReference.IsConstant = true;

        Expected("CONSTANT");
        if (CurrentEquals("IS") || CurrentEquals("GLOBAL"))
        {
            Optional("IS");
            Expected("GLOBAL");
                dataReference.IsGlobal = true;
        }

        if (CurrentEquals("FROM"))
        {
            Expected("FROM");
            Identifier();
        }
        else
        {
            Optional("AS");
            switch (Current().Type)
            {
                case TokenType.String:
                    String();
                    break;

                case TokenType.Numeric:
                    Number();
                    break;

                case TokenType.FigurativeLiteral:
                    FigurativeLiteral();
                    break;
            }

            if (CurrentEquals("LENGTH"))
            {
                Expected("LENGTH");
                Optional("OF");
                Identifier();
            }

            if (CurrentEquals("BYTE-LENGTH"))
            {
                Expected("BYTE-LENGTH");
                Optional("OF");
                Identifier();
            }

        }

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                Data item definition, missing separator period.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected a separator period '. ' after this token
                """)
            .WithNote("""
                Every item must end with a separator period
                """)
            .CloseError();
        }

        SymbolTable.DataLocals.AddOrUpdateReference(dataName, dataReference);
    }

    private static void CheckLevelNumber(int level)
    {
        if (level is 66 or 77 or 88) return;

        if (level is 1)
        {
            LevelStack.Push(level);
            return;
        }

        var currentLevel = LevelStack.Peek();

        if (level == currentLevel) return;

        if (level > currentLevel && level <= 49)
        {
            LevelStack.Push(level);
            return;
        }

        if (level < currentLevel)
        {
            var current = LevelStack.Pop();
            var lowerLevel = LevelStack.Peek();
            if (level == lowerLevel)
            {
                GroupStack.Pop();
                return;
            }

            if (level != lowerLevel)
            {
                LevelStack.Push(current);

                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 40,"""
                    Invalid level number.
                    """)
                .WithSourceLine(Current(), $"""
                    This variable should have a level number of {lowerLevel}.
                    """)
                .CloseError();
            }
        }
    }

    private static void CheckClauses(DataSignature localReference, Token itemToken)
    {
        var dataItem = localReference;

        bool usageCannotHavePicture = dataItem.UsageType switch
        {
            UsageType.BinaryChar => true,
            UsageType.BinaryDouble => true,
            UsageType.BinaryLong => true,
            UsageType.BinaryShort => true,
            UsageType.FloatShort => true,
            UsageType.FloatLong => true,
            UsageType.FloatExtended => true,
            UsageType.Index => true,
            UsageType.MessageTag => true,
            UsageType.ObjectReference => true,
            UsageType.DataPointer => true,
            UsageType.FunctionPointer => true,
            UsageType.ProgramPointer => true,
            _ => false
        };

        if (usageCannotHavePicture && dataItem.HasPicture)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 45,"""
                Invalid clause combination.
                """)
            .WithSourceLine(Current(), $"""
                Items with USAGE {dataItem.UsageType.Display()} must not contain a PICTURE clause.
                """)
            .CloseError();
        }

        if (!usageCannotHavePicture && dataItem.IsElementary && !dataItem.HasPicture && !dataItem.HasUsage && !dataItem.HasValue)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 45,"""
                Invalid clause combination.
                """)
            .WithSourceLine(Current(), $"""
                Elementary items must contain a PICTURE clause.
                """)
            .CloseError();
        }

        if (dataItem.IsGroup && dataItem.HasPicture)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 45,"""
                Invalid clause combination.
                """)
            .WithSourceLine(Current(), $"""
                Group items must not contain a PICTURE clause.
                """)
            .CloseError();
        }

        if (dataItem.IsRenames && dataItem.HasPicture)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 45,"""
                Invalid clause combination.
                """)
            .WithSourceLine(Current(), $"""
                Items with a RENAMES clause must not contain a PICTURE clause.
                """)
            .CloseError();
        }

        bool usageCannotHaveValue = dataItem.UsageType switch
        {
            UsageType.Index => true,
            UsageType.MessageTag => true,
            UsageType.ObjectReference => true,
            UsageType.DataPointer => true,
            UsageType.FunctionPointer => true,
            UsageType.ProgramPointer => true,
            _ => false
        };

        if (usageCannotHaveValue && dataItem.HasValue)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 45,"""
                Invalid clause combination.
                """)
            .WithSourceLine(Current(), $"""
                Items with USAGE {dataItem.UsageType.Display()} must not contain a VALUE clause.
                """)
            .CloseError();
        }
    }

    private static void CheckConditionNames(string parentName)
    {
        if (!CurrentEquals("88")) return;

        while (CurrentEquals("88"))
        {
            Expected("88");

            Token itemToken = Current();
            string dataName = itemToken.Value;

            Identifier();

            if (SymbolTable.DataLocals.ReferenceExists(dataName))
            {
                ErrorHandler.Analyzer.Report(FileName, itemToken, ErrorType.General, $"""
                A data item with this name already exists in this source unit, data items must have a unique name.
                """);
                ErrorHandler.Analyzer.PrettyError(FileName, itemToken);
            }

            DataSignature dataReference = new();

            dataReference.Parent = parentName;
            dataReference.Identifier = dataName;
            dataReference.LevelNumber = 88;
            dataReference.Section = CurrentSection;

            if (CurrentEquals("VALUES"))
            {
                Expected("VALUES");
                Optional("ARE");
            }
            else
            {
                Expected("VALUE");
                Optional("IS");
            }

            var firstConditionType = Current().Type;

            switch (Current().Type)
            {
                case TokenType.Numeric: Number(); break;
                
                case TokenType.String:
                case TokenType.HexString:
                case TokenType.Boolean:
                case TokenType.HexBoolean:
                case TokenType.National:
                case TokenType.HexNational:
                    String(); break;
            }

            if (CurrentEquals("THROUGH", "THRU"))
            {
                Choice("THROUGH", "THRU");

                switch (firstConditionType)
                {
                    case TokenType.Numeric: Number(); break;
                    
                    case TokenType.String:
                    case TokenType.HexString:
                    case TokenType.Boolean:
                    case TokenType.HexBoolean:
                    case TokenType.National:
                    case TokenType.HexNational:
                        String(); break;
                }
            }

            if (!Expected(".", false))
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                    Data item definition, missing separator period.
                    """)
                .WithSourceLine(Lookahead(-1), """
                    Expected a separator period '. ' after this token
                    """)
                .WithNote("""
                    Every item must end with a separator period
                    """)
                .CloseError();
            }

            SymbolTable.DataLocals.AddOrUpdateReference(dataName, dataReference);
        }
    }

    private static void UsageClause(DataSignature dataReference)
    {
        Expected("USAGE");
        Optional("IS");
        switch (Current().Value)
        {
            case "BINARY":
                Expected("BINARY");
                dataReference.UsageType = UsageType.Binary;
                break;

            case "BINARY-CHAR":
            case "BINARY-SHORT":
            case "BINARY-LONG":
            case "BINARY-DOUBLE":
                Expected(Current().Value);
                if (CurrentEquals("SIGNED"))
                {
                    Expected("SIGNED");
                }
                else if (CurrentEquals("UNSIGNED"))
                {
                    Expected("UNSIGNED");
                }
                break;

            case "BIT":
                Expected("BIT");
                dataReference.UsageType = UsageType.Bit;
                break;

            case "COMP":
            case "COMPUTATIONAL":
                Expected(Current().Value);
                dataReference.UsageType = UsageType.Computational;
                break;

            case "DISPLAY":
                Expected("DISPLAY");
                dataReference.UsageType = UsageType.Display;
                break;

            case "FLOAT-BINARY-32":
                Expected("FLOAT-BINARY-32");
                Choice("HIGH-ORDER-LEFT", "HIGH-ORDER-RIGHT");
                break;

            case "FLOAT-BINARY-64":
                Expected("FLOAT-BINARY-64");
                Choice("HIGH-ORDER-LEFT", "HIGH-ORDER-RIGHT");
                break;

            case "FLOAT-BINARY-128":
                Expected("FLOAT-BINARY-128");
                Choice("HIGH-ORDER-LEFT", "HIGH-ORDER-RIGHT");
                break;

            case "FLOAT-DECIMAL-16":
                Expected("FLOAT-DECIMAL-16");
                EncodingEndianness();
                break;

            case "FLOAT-DECIMAL-32":
                Expected("FLOAT-DECIMAL-32");
                EncodingEndianness();
                break;

            case "FLOAT-EXTENDED":
                Expected("FLOAT-EXTENDED");
                break;

            case "FLOAT-LONG":
                Expected("FLOAT-LONG");
                break;

            case "FLOAT-SHORT":
                Expected("FLOAT-SHORT");
                break;

            case "INDEX":
                Expected("INDEX");
                dataReference.UsageType = UsageType.Index;
                break;

            case "MESSAGE-TAG":
                Expected("MESSAGE-TAG");
                dataReference.UsageType = UsageType.MessageTag;
                break;

            case "NATIONAL":
                Expected("NATIONAL");
                dataReference.UsageType = UsageType.National;
                break;

            case "OBJECT":
                Expected("OBJECT");
                Expected("REFERENCE");
                // var isFactory = false;
                // var isStronglyTyped = false;

                // Need implement identifier resolution first
                // To parse the rest of this using clause correctly
                dataReference.UsageType = UsageType.ObjectReference;
                if (CurrentEquals("Factory"))
                {
                    Expected("FACTORY");
                    Optional("OF");
                    // isFactory = true;
                }

                if (CurrentEquals("ACTIVE-CLASS"))
                {
                    Expected("ACTIVE-CLASS");
                    break;
                }

                Continue();

                if (CurrentEquals("ONLY"))
                {
                    Expected("ONLY");
                    // isStronglyTyped = true
                }

                break;

            case "PACKED-DECIMAL":
                Expected("PACKED-DECIMAL");
                if (CurrentEquals("WITH", "NO"))
                {
                    Optional("WITH");
                    Expected("NO");
                    Expected("SIGN");
                }
                break;

            case "POINTER":
                Expected("POINTER");
                if (CurrentEquals("TO") || CurrentEquals(TokenType.Identifier))
                {
                    Optional("TO");
                    dataReference.UsageType = UsageType.DataPointer;
                    dataReference.UsageContext = Current().Value;
                    Identifier();
                }
                else
                {
                    dataReference.UsageType = UsageType.DataPointer;
                }
                break;

            case "FUNCTION-POINTER":
                Expected("FUNCTION-POINTER");
                Optional("TO");
                dataReference.UsageType = UsageType.FunctionPointer;
                dataReference.UsageContext = Current().Value;
                Identifier();
                break;

            case "PROGRAM-POINTER":
                Expected("PROGRAM-POINTER");
                if (CurrentEquals("TO") || CurrentEquals(TokenType.Identifier))
                {
                    Optional("TO");
                    dataReference.UsageType = UsageType.ProgramPointer;
                    dataReference.UsageContext = Current().Value;
                    Identifier();
                }
                else
                {
                    dataReference.UsageType = UsageType.ProgramPointer;
                }
                break;

            default:
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 50,"""
                    Unrecognized USAGE clause.
                    """)
                .WithSourceLine(Lookahead(-1))
                .WithNote("""
                    This could be due to an unsupported third-party extension.
                    """)
                .CloseError();

                AnchorPoint(TokenContext.IsClause);
                break;
        }
    }
}
