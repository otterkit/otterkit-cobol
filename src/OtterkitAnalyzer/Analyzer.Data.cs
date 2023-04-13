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
    private static readonly Stack<DataEntry> GroupStack = new();

    // Method responsible for parsing the DATA DIVISION.
    // That includes the FILE, WORKING-STORAGE, LOCAL-STORAGE, LINKAGE, REPORT and SCREEN sections.
    // It is also responsible for showing appropriate error messages when an error occurs in the DATA DIVISION.
    private static void DATA()
    {
        Expected("DATA");
        Expected("DIVISION");
        CurrentScope = CurrentScope.DataDivision;

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

        if (CurrentEquals("FILE"))
            FileSection();

        if (CurrentEquals("WORKING-STORAGE"))
            WorkingStorage();

        if (CurrentEquals("LOCAL-STORAGE"))
            LocalStorage();

        if (CurrentEquals("LINKAGE"))
            LinkageSection();

        if (CurrentEquals("SCREEN"))
            ScreenSection();
    }


    // The following methods are responsible for parsing the DATA DIVISION sections
    // They are technically only responsible for parsing the section header, 
    // the Entries() method handles parsing the actual data items in their correct sections.
    private static void FileSection()
    {
        Expected("FILE");
        Expected("SECTION");
        CurrentScope = CurrentScope.FileSection;

        Expected(".");
        while (CurrentEquals("FD", "SD"))
        {
            FileEntry();
        }
    }
    
    private static void ScreenSection()
    {
        Expected("SCREEN");
        Expected("SECTION");
        CurrentScope = CurrentScope.ScreenSection;

        Expected(".");
        while (CurrentEquals(TokenType.Numeric))
        {
            ScreenEntries();
        }
    }

    private static void WorkingStorage()
    {
        Expected("WORKING-STORAGE");
        Expected("SECTION");
        CurrentScope = CurrentScope.WorkingStorage;

        Expected(".");
        while (CurrentEquals(TokenType.Numeric))
        {
            DataEntries();
        }
    }

    private static void LocalStorage()
    {
        Expected("LOCAL-STORAGE");
        Expected("SECTION");
        CurrentScope = CurrentScope.LocalStorage;

        Expected(".");
        while (Current().Type is TokenType.Numeric)
        {
            DataEntries();
        }
    }

    private static void LinkageSection()
    {
        Expected("LINKAGE");
        Expected("SECTION");
        CurrentScope = CurrentScope.LinkageSection;

        Expected(".");
        while (Current().Type is TokenType.Numeric)
        {
            DataEntries();
        }
    }


    // The following methods are responsible for parsing the DATA DIVISION data items
    // The Entries() method is responsible for identifying which kind of data item to 
    // parse based on it's level number.

    // The GroupEntry(), DataEntry(), and ConstantEntry() are then responsible for correctly
    // parsing each data item, or in the case of the GroupEntry() a group item or 01-level elementary item.
    private static void DataEntries()
    {
        if (CurrentEquals("77"))
            DataEntry();

        if ((CurrentEquals("01") || CurrentEquals("1")) && !LookaheadEquals(2, "CONSTANT"))
            GroupEntry();

        if (LookaheadEquals(2, "CONSTANT"))
            ConstantEntry();
    }

    private static void RecordEntries()
    {
        if (!CurrentEquals("01", "1"))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 2,"""
                Unexpected token.
                """)
            .WithSourceLine(Current(), """
                Expected a 01 level number.
                """)
            .WithNote("""
                Root level records must have a 01 level number.
                """)
            .CloseError(); 
        }

        if (CurrentEquals("01", "1") && !LookaheadEquals(2, "CONSTANT"))
            GroupEntry();

        if (LookaheadEquals(2, "CONSTANT"))
            ConstantEntry();
    }

    private static void ScreenEntries()
    {
        if (!CurrentEquals("01", "1"))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 2,"""
                Unexpected token.
                """)
            .WithSourceLine(Current(), """
                Expected a 01 level number.
                """)
            .WithNote("""
                Root level screen items must have a 01 level number.
                """)
            .CloseError(); 
        }

        if (CurrentEquals("01", "1") && !LookaheadEquals(2, "CONSTANT"))
            GroupEntry();

        if (LookaheadEquals(2, "CONSTANT"))
            ConstantEntry();
    }

    private static void FileEntry()
    {
        Choice("FD", "SD");

        Token itemToken = Current();
        string fileName = itemToken.Value;

        Identifier();

        DataEntry fileLocal = new(itemToken, EntryType.FileDescription);

        fileLocal.Section = CurrentScope;

        if (!CurrentEquals(TokenContext.IsClause) && !CurrentEquals("."))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 2,"""
                Unexpected token.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected file description clauses or a separator period after this token
                """)
            .CloseError();
        }

        while (CurrentEquals(TokenContext.IsClause))
        {
            FileEntryClauses(fileLocal);
        }

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                File description, missing separator period.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected a separator period '. ' after this token
                """)
            .WithNote("""
                Every FD item must end with a separator period
                """)
            .CloseError();
        }

        while(CurrentEquals(TokenType.Numeric))
        {
            RecordEntries();
        }

        // We're returning during a resolution pass
        if (IsResolutionPass) return;

        // Because we don't want to run this again during it
        var sourceUnit = CurrentCallable;

        if (sourceUnit.DataEntries.EntryExists(fileName))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 30,"""
                Duplicate root level definition.
                """)
            .WithSourceLine(itemToken, """
                A root level variable already exists with this name.
                """)
            .WithNote("""
                Every root level item must have a unique name. 
                """)
            .CloseError();
        }

        sourceUnit.DataEntries.AddEntry(fileName, fileLocal);
    }



    private static void GroupEntry()
    {
        if (CurrentScope is CurrentScope.ScreenSection)
        {
            ScreenEntry();
        }
        else
        {
            DataEntry();
        }

        _ = int.TryParse(Current().Value, out int outInt);
        
        while (outInt > 1 && outInt < 50)
        {
            if (CurrentScope is CurrentScope.ScreenSection)
            {
                ScreenEntry();
            }
            else
            {
                DataEntry();
            }

            _ = int.TryParse(Current().Value, out outInt);
        }

        LevelStack.Clear();
        GroupStack.Clear();
    }
    
    private static void DataEntry()
    {
        int levelNumber = int.Parse(Current().Value);
        Number();

        Token itemToken = Current();
        string dataName = itemToken.Value;

        CheckLevelNumber(levelNumber);

        if (CurrentEquals("FILLER"))
        {
            Expected("FILLER");
        }
        else if (CurrentEquals(TokenType.Identifier))
        {
            Identifier();
        }

        DataEntry dataLocal = new(itemToken, EntryType.DataDescription);

        dataLocal.LevelNumber = levelNumber;
        dataLocal.Section = CurrentScope;

        if (GroupStack.Count is not 0)
        {
            dataLocal.Parent = GroupStack.Peek();
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
            DataEntryClauses(dataLocal);
        }

        HandleLevelStack(dataLocal);

        CheckClauseCompatibility(dataLocal, itemToken);

        if (dataLocal.IsGroup) GroupStack.Push(dataLocal);

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

        CheckConditionNames(dataLocal);

        // We're returning during a resolution pass
        if (IsResolutionPass) return;

        if (dataName is "FILLER") return;

        // Because we don't want to run this again during it
        var sourceUnit = CurrentCallable;

        if (sourceUnit.DataEntries.EntryExists(dataName) && levelNumber is 1 or 77)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 30,"""
                Duplicate root level definition.
                """)
            .WithSourceLine(itemToken, """
                A 01 or 77 level variable already exists with this name.
                """)
            .WithNote("""
                Every root level item must have a unique name. 
                """)
            .CloseError();
        }

        sourceUnit.DataEntries.AddEntry(dataName, dataLocal);
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

        Token itemToken = Current();
        string dataName = itemToken.Value;

        Identifier();

        DataEntry dataLocal = new(itemToken, EntryType.DataDescription);

        dataLocal.LevelNumber = levelNumber;
        dataLocal.Section = CurrentScope;
        dataLocal.IsConstant = true;

        Expected("CONSTANT");
        if (CurrentEquals("IS") || CurrentEquals("GLOBAL"))
        {
            Optional("IS");
            Expected("GLOBAL");
                dataLocal.IsGlobal = true;
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
                    StringLiteral();
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

        // We're returning during a resolution pass
        if (IsResolutionPass) return;

        // Because we don't want to run this again during it
        var sourceUnit = CurrentCallable;

        if (sourceUnit.DataEntries.EntryExists(dataName) && levelNumber is 1 or 77)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 30,"""
                Duplicate root level definition.
                """)
            .WithSourceLine(itemToken, """
                A 01 or 77 level variable already exists with this name.
                """)
            .WithNote("""
                Every root level item must have a unique name. 
                """)
            .CloseError();
        }

        sourceUnit.DataEntries.AddEntry(dataName, dataLocal);
    }

    private static void ScreenEntry()
    {
        int levelNumber = int.Parse(Current().Value);
        Number();

        Token itemToken = Current();
        string screenName = itemToken.Value;

        CheckLevelNumber(levelNumber);
        
        if (CurrentEquals("FILLER"))
        {
            Expected("FILLER");
        }
        else if (CurrentEquals(TokenType.Identifier) && !CurrentEquals(TokenContext.IsClause))
        {
            Identifier();
        }

        DataEntry screenLocal = new(itemToken, EntryType.ScreenDescription);

        screenLocal.LevelNumber = levelNumber;
        screenLocal.Section = CurrentScope;

        if (GroupStack.Count is not 0)
        {
            screenLocal.Parent = GroupStack.Peek();
        }
        
        if (!CurrentEquals(TokenContext.IsClause) && !CurrentEquals("."))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 2,"""
                Unexpected token.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected screen item clauses or a separator period after this token
                """)
            .CloseError();
        }

        while (CurrentEquals(TokenContext.IsClause))
        {
            ScreenEntryClauses(screenLocal);
        }

        HandleLevelStack(screenLocal);

        if (screenLocal.IsGroup) GroupStack.Push(screenLocal);

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                Screen item definition, missing separator period.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected a separator period '. ' after this token
                """)
            .WithNote("""
                Every item must end with a separator period
                """)
            .CloseError();
        }

        CheckConditionNames(screenLocal);

        // We're returning during a resolution pass
        if (IsResolutionPass) return;

        // Because we don't want to run this again during it
        var sourceUnit = CurrentCallable;

        if (sourceUnit.DataEntries.EntryExists(screenName) && levelNumber is 1 or 77)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 30,"""
                Duplicate root level definition.
                """)
            .WithSourceLine(itemToken, """
                A 01 or 77 level variable already exists with this name.
                """)
            .WithNote("""
                Every root level item must have a unique name. 
                """)
            .CloseError();
        }

        sourceUnit.DataEntries.AddEntry(screenName, screenLocal);
    }

    private static void HandleLevelStack(DataEntry entryLocal)
    {
        if (CurrentEquals(".") && LookaheadEquals(1, TokenType.Numeric))
        {
            if (LevelStack.Count == 0)
            {
                entryLocal.IsElementary = true;
            }
            else
            {
                _ = int.TryParse(Lookahead(1).Value, out int outInt);
                var currentLevel = LevelStack.Peek();

                if (currentLevel == 1 && outInt >= 2 && outInt <= 49 || outInt >= 2 && outInt <= 49 && outInt > currentLevel)
                {
                    entryLocal.IsGroup = true;
                }
                else
                {
                    entryLocal.IsElementary = true;
                }
            }
        }
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

    private static void CheckClauseCompatibility(DataEntry localReference, Token itemToken)
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

    private static void CheckConditionNames(DataEntry parent)
    {
        if (!CurrentEquals("88")) return;

        while (CurrentEquals("88"))
        {
            Expected("88");

            Token itemToken = Current();
            string dataName = itemToken.Value;

            Identifier();

            DataEntry dataLocal = new(itemToken, EntryType.DataDescription);

            dataLocal.Parent = parent;
            dataLocal.LevelNumber = 88;
            dataLocal.Section = CurrentScope;

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
                    StringLiteral(); break;
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
                        StringLiteral(); break;
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

            // We're returning during a resolution pass
            if (IsResolutionPass) continue;

            // Because we don't want to run this again during it
            var sourceUnit = CurrentCallable;

            if (sourceUnit.DataEntries.EntryExists(dataName))
            {
                // TODO: This is incorrect, but was done to replace the old error message system
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 30,"""
                    Duplicate condition name definition.
                    """)
                .WithSourceLine(itemToken, """
                    A condition variable already exists with this name
                    """)
                .WithNote("""
                    condition items must have a unique name. 
                    """)
                .CloseError();
            }

            sourceUnit.DataEntries.AddEntry(dataName, dataLocal);
        }
    }
}
