using System.Diagnostics;
using System.Text;

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
        Expected("DATA", "data division");
        Expected("DIVISION");
        CurrentSection = CurrentScope.DataDivision;

        Expected(".", """
        Missing separator period at the end of this DATA DIVISION header, every division header must end with a separator period
        """, -1, "WORKING-STORAGE", "LOCAL-STORAGE", "LINKAGE", "PROCEDURE");

        if (CurrentEquals("WORKING-STORAGE"))
            WorkingStorage();

        if (CurrentEquals("LOCAL-STORAGE"))
            LocalStorage();

        if (CurrentEquals("LINKAGE"))
            LinkageSection();

        if (!CurrentEquals("PROCEDURE"))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "Data Division data items and sections");
            ErrorHandler.Parser.PrettyError(FileName, Current());
            Continue();
        }
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
        while (Current().type == TokenType.Numeric)
            Entries();
    }

    private static void LocalStorage()
    {
        Expected("LOCAL-STORAGE");
        Expected("SECTION");
        CurrentSection = CurrentScope.LocalStorage;

        Expected(".");
        while (Current().type is TokenType.Numeric)
            Entries();
    }

    private static void LinkageSection()
    {
        Expected("LINKAGE");
        Expected("SECTION");
        CurrentSection = CurrentScope.LinkageSection;

        Expected(".");
        while (Current().type is TokenType.Numeric)
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
        _ = int.TryParse(Current().value, out int outInt);
        while (outInt > 1 && outInt < 50)
        {
            BaseEntry();
            _ = int.TryParse(Current().value, out outInt);
        }

        LevelStack.Clear();
    }

    private static void BaseEntry()
    {
        string dataType;
        int LevelNumber = int.Parse(Current().value);
        Number();

        Token DataItem = Current();
        string DataName = DataItem.value;

        CheckLevelNumber(LevelNumber);
        
        Identifier();

        string DataItemHash = $"{SourceId.Peek()}#{DataName}";
        if (SymbolTable.SymbolExists(DataItemHash))
        {
            ErrorHandler.Parser.Report(FileName, DataItem, ErrorType.General, $"""
            A data item with this name already exists in this source unit, data items must have a unique name.
            """);
            ErrorHandler.Parser.PrettyError(FileName, DataItem);
        }
        else
        {
            SymbolTable.AddSymbol(DataItemHash, SymbolType.DataItem);
        }

        DataItemInfo dataItem = SymbolTable.GetDataItem(DataItemHash);

        dataItem.Identifier = DataName;
        dataItem.LevelNumber = LevelNumber;
        dataItem.Section = CurrentSection;

        if (GroupStack.Count == 0)
        {
            dataItem.Parent = "Root";
        }
        else
        {
            dataItem.Parent = GroupStack.Peek();
        }

        if (!CurrentEquals(TokenContext.IsClause) && !CurrentEquals("."))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
            Expected data division clauses or a separator period after this data item's identifier.
            Token found ("{Current().value}") was not a data division clause reserved word.
            """);
            ErrorHandler.Parser.PrettyError(FileName, Current());
        }

        while (CurrentEquals(TokenContext.IsClause))
        {
            if (CurrentEquals("IS") && !LookaheadEquals(1, "EXTERNAL", "GLOBAL", "TYPEDEF"))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                Missing clause or possible clause mismatch, in this context the "IS" word must be followed by the EXTERNAL, GLOBAL or TYPEDEF clauses only (IS TYPEDEF), or must be in the middle of the PICTURE clause (PIC IS ...) 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            if ((CurrentEquals("IS") && LookaheadEquals(1, "EXTERNAL")) || CurrentEquals("EXTERNAL"))
            {
                Optional("IS");
                Expected("EXTERNAL");
                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    dataItem.IsExternal = true;
                    dataItem.ExternalName = Current().value;

                    String("""
                    Missing externalized name, the "AS" word on the EXTERNAL clause must be followed by an alphanumeric or national literal
                    """, -1);
                }

                if (!CurrentEquals("AS"))
                {
                    dataItem.IsExternal = true;
                    dataItem.ExternalName = Current().value;
                }
            }

            if ((CurrentEquals("IS") && LookaheadEquals(1, "GLOBAL")) || CurrentEquals("GLOBAL"))
            {
                Optional("IS");
                Expected("GLOBAL");
                dataItem.IsGlobal = true;
            }

            if ((CurrentEquals("IS") && LookaheadEquals(1, "TYPEDEF")) || CurrentEquals("TYPEDEF"))
            {
                Optional("IS");
                Expected("TYPEDEF");
                dataItem.IsTypedef = true;

                if (CurrentEquals("STRONG")) Expected("STRONG");
            }

            if (CurrentEquals("REDEFINES"))
            {
                Expected("REDEFINES");
                Identifier();
                dataItem.IsRedefines = true;
            }

            if (CurrentEquals("ALIGNED")) Expected("ALIGNED");

            if (CurrentEquals("ANY") && LookaheadEquals(1, "LENGTH"))
            {
                Expected("ANY");
                Expected("LENGTH");
                dataItem.IsAnyLength = true;
            }

            if (CurrentEquals("BASED")) Expected("BASED");

            if (CurrentEquals("BLANK"))
            {
                Expected("BLANK");
                Optional("WHEN");
                Expected("ZERO");
                dataItem.IsBlank = true;
            }

            if (CurrentEquals("CONSTANT") && LookaheadEquals(1, "RECORD"))
            {
                Expected("CONSTANT");
                Expected("RECORD");
                dataItem.IsConstantRecord = true;
            }

            if (CurrentEquals("DYNAMIC"))
            {
                Expected("DYNAMIC");
                Optional("LENGTH");
                dataItem.IsDynamicLength = true;

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
                dataItem.IsProperty = true;
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
                dataType = Current().value switch
                {
                    "S9" => "S9",
                    "9" => "9",
                    "X" => "X",
                    "A" => "A",
                    "N" => "N",
                    "1" => "1",
                    _ => "Error"
                };

                if (dataType == "Error")
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    Unrecognized type, PICTURE type must be S9, 9, X, A, N or 1. These are Signed Numeric, Unsigned Numeric, Alphanumeric, Alphabetic, National and Boolean respectively
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                dataItem.Type = dataType;
                dataItem.IsPicture = true;
                Choice("S9", "9", "X", "A", "N", "1");

                Expected("(");
                string DataLength = Current().value;
                Number();
                Expected(")");
                if (CurrentEquals("V9") && dataType != "S9" && dataType != "9")
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, "V9 cannot be used with non-numeric types");
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                if (CurrentEquals("V9"))
                {
                    Expected("V9");
                    Expected("(");
                    DataLength += $"V{Current().value}";
                    Number();
                    Expected(")");
                }

                dataItem.PictureLength = DataLength;
            }

            if (CurrentEquals("VALUE"))
            {
                Expected("VALUE");

                if (!CurrentEquals(TokenType.String, TokenType.Numeric))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The only tokens allowed after a VALUE clause are type literals, like an Alphanumeric literal ("Hello, World!") or a Numeric literal (123.456).
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                if (CurrentEquals(TokenType.String))
                {
                    dataItem.DefaultValue = Current().value;
                    String();
                }

                if (CurrentEquals(TokenType.Numeric))
                {
                    dataItem.DefaultValue = Current().value;
                    Number();
                }
            }

            if (CurrentEquals("USAGE"))
            {
                UsageClause(dataItem);
            }

        }

        if (CurrentEquals(".") && LookaheadEquals(1, TokenType.Numeric))
        {
            if (LevelStack.Count == 0)
            {
                dataItem.IsElementary = true;
            }
            else
            {
                _ = int.TryParse(Lookahead(1).value, out int outInt);
                var currentLevel = LevelStack.Peek();

                if (currentLevel == 1 && outInt >= 2 && outInt <= 49 || outInt >= 2 && outInt <= 49 && outInt > currentLevel)
                {
                    dataItem.IsGroup = true;
                }
                else
                {
                    dataItem.IsElementary = true;
                }
            }
        }

        CheckClauses(DataItemHash, DataItem);

        if (dataItem.IsGroup) GroupStack.Push(DataItemHash);

        Expected(".", """
        Missing separator period at the end of this data item definition, each data item must end with a separator period
        """, -1, "PROCEDURE");
    }

    private static void ConstantEntry()
    {
        if (!CurrentEquals("01") && !CurrentEquals("1"))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
            Invalid level number for this data item, CONSTANT data items must have a level number of 1 or 01
            """);
            ErrorHandler.Parser.PrettyError(FileName, Current());
        }

        var LevelNumber = int.Parse(Current().value);
        Number();

        var DataName = Current().value;
        Identifier();

        var DataItemHash = $"{SourceId.Peek()}#{DataName}";
        if (SymbolTable.SymbolExists(DataItemHash))
        {
            var originalItem = SymbolTable.GetDataItem(DataItemHash);

            ErrorHandler.Parser.Report(FileName, Lookahead(-1), ErrorType.General, $"""
            A data item with this name already exists in this program, data items in a program must have a unique name.
            The original {originalItem.Identifier} data item can be found on line {originalItem.Line}. 
            """);
            ErrorHandler.Parser.PrettyError(FileName, Lookahead(-1));
        }
        else
        {
            SymbolTable.AddSymbol(DataItemHash, SymbolType.DataItem);
        }

        DataItemInfo dataItem = SymbolTable.GetDataItem(DataItemHash);

        dataItem.Identifier = DataName;
        dataItem.LevelNumber = LevelNumber;
        dataItem.Section = CurrentSection;
        dataItem.IsConstant = true;

        Expected("CONSTANT");
        if (CurrentEquals("IS") || CurrentEquals("GLOBAL"))
        {
            Optional("IS");
            Expected("GLOBAL");
                dataItem.IsGlobal = true;
        }

        if (CurrentEquals("FROM"))
        {
            Expected("FROM");
            Identifier();
        }
        else
        {
            Optional("AS");
            switch (Current().type)
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

        Expected(".");
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
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                All data items that are immediate members of a group item must have equal level numbers, and it should be greater than the level number used for that group item. 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
        }
    }

    private static void CheckClauses(string dataItemHash, Token itemToken)
    {
        DataItemInfo dataItem = SymbolTable.GetDataItem(dataItemHash);

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

        if (usageCannotHavePicture && dataItem.IsPicture)
        {
            ErrorHandler.Parser.Report(FileName, itemToken, ErrorType.General, $"""
            Data items defined with USAGE {dataItem.UsageType} cannot contain a PICTURE clause
            """);
            ErrorHandler.Parser.PrettyError(FileName, itemToken);
        }

        if (!usageCannotHavePicture && dataItem.IsElementary && !dataItem.IsPicture && !dataItem.IsValue)
        {
            ErrorHandler.Parser.Report(FileName, itemToken, ErrorType.General, """
            Elementary data items must contain a PICTURE clause. Except when an alphanumeric, boolean, or national literal is defined in the VALUE clause 
            """);
            ErrorHandler.Parser.PrettyError(FileName, itemToken);
        }

        if (dataItem.IsGroup && dataItem.IsPicture)
        {
            ErrorHandler.Parser.Report(FileName, itemToken, ErrorType.General, """
            Group items must not contain a PICTURE clause. The PICTURE clause can only be specified on elementary data items
            """);
            ErrorHandler.Parser.PrettyError(FileName, itemToken);
        }

        if (dataItem.IsRenames && dataItem.IsPicture)
        {
            ErrorHandler.Parser.Report(FileName, itemToken, ErrorType.General, """
            Data items with a RENAMES clause must not contain a PICTURE clause
            """);
            ErrorHandler.Parser.PrettyError(FileName, itemToken);
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

        if (usageCannotHaveValue && dataItem.IsValue)
        {
            ErrorHandler.Parser.Report(FileName, itemToken, ErrorType.General, $"""
            Data items defined with USAGE {dataItem.UsageType} cannot contain a VALUE clause
            """);
            ErrorHandler.Parser.PrettyError(FileName, itemToken);
        }

    }

    private static void UsageClause(DataItemInfo dataitem)
    {
        Expected("USAGE");
        Optional("IS");
        switch (Current().value)
        {
            
            case "BINARY":
                Expected("BINARY");
                dataitem.UsageType = UsageType.Binary;
                break;

            case "BINARY-CHAR":
            case "BINARY-SHORT":
            case "BINARY-LONG":
            case "BINARY-DOUBLE":
                Expected(Current().value);
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
                dataitem.UsageType = UsageType.Bit;
                break;

            case "COMP":
            case "COMPUTATIONAL":
                Expected(Current().value);
                dataitem.UsageType = UsageType.Computational;
                break;

            case "DISPLAY":
                Expected("DISPLAY");
                dataitem.UsageType = UsageType.Display;
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
                dataitem.UsageType = UsageType.Index;
                break;

            case "MESSAGE-TAG":
                Expected("MESSAGE-TAG");
                dataitem.UsageType = UsageType.MessageTag;
                break;

            case "NATIONAL":
                Expected("NATIONAL");
                break;

            case "OBJECT":
                Expected("OBJECT");
                Expected("REFERENCE");
                // var isFactory = false;
                // var isStronglyTyped = false;

                // Need implement identifier resolution first
                // To parse the rest of this using clause correctly
                dataitem.UsageType = UsageType.ObjectReference;
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
                    dataitem.UsageType = UsageType.DataPointer;
                    dataitem.UsageContext = Current().value;
                    Identifier();
                }
                else
                {
                    dataitem.UsageType = UsageType.DataPointer;
                }
                break;

            case "FUNCTION-POINTER":
                Expected("FUNCTION-POINTER");
                Optional("TO");
                dataitem.UsageType = UsageType.FunctionPointer;
                dataitem.UsageContext = Current().value;
                Identifier();
                break;

            case "PROGRAM-POINTER":
                Expected("PROGRAM-POINTER");
                if (CurrentEquals("TO") || CurrentEquals(TokenType.Identifier))
                {
                    Optional("TO");
                    dataitem.UsageType = UsageType.ProgramPointer;
                    dataitem.UsageContext = Current().value;
                    Identifier();
                }
                else
                {
                    dataitem.UsageType = UsageType.ProgramPointer;
                }
                break;

            default:
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Recovery, """
                Unrecognized USAGE clause. This could be due to an unsupported third-party extension. 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current(), ConsoleColor.Blue);

                AnchorPoint(TokenContext.IsClause);
                break;
        }
    }

}