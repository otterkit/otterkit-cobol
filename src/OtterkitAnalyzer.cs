using System.Diagnostics;
using System.Text;

namespace Otterkit;

struct SetLcValues
{
    public bool LC_ALL;
    public bool LC_COLLATE;
    public bool LC_CTYPE;
    public bool LC_MESSAGES;
    public bool LC_MONETARY;
    public bool LC_NUMERIC;
    public bool LC_TIME;
}

/// <summary>
/// Otterkit COBOL Syntax Analyzer
/// <para>This parser was built to be easily extensible, with some reusable COBOL parts.</para>
/// <para>It requires a List of Tokens generated from the Lexer and the Token Classifier.</para>
/// </summary>
public static class Analyzer
{
    /// <summary>
    /// String <c>FileName</c> is used in the parser as a parameter for the <c>ErrorHandler</c> method.
    /// <para>The error handler will use this to fetch the file and get the correct line and column when displaying the error message.</para>
    /// </summary>
    private static string FileName;

    /// <summary>
    /// Stack string <c>SourceId</c> is used in the parser whenever it needs to know the name of the current source unit (The identifier after PROGRAM-ID).
    /// <para>This is used when checking if a variable already exists in the current source unit, and when adding them to the DataItemInformation class's variable table.
    /// The DataItemInformation class is then used to simplify the codegen process of generating data items.</para>
    /// </summary>
    private static readonly Stack<string> SourceId;

    /// <summary>
    /// Stack SourceUnit <c>SourceType</c> is used in the parser whenever it needs to know which <c>-ID</c> it is currently parsing.
    /// <para>This is used when handling certain syntax rules for different <c>-ID</c>s, like the <c>"RETURNING data-name"</c> being required for every <c>FUNCTION-ID</c> source unit.</para>
    /// </summary>
    private static readonly Stack<SourceUnit> SourceType;

    /// <summary>
    /// Stack int <c>LevelStack</c> is used in the parser whenever it needs to know which data item level it is currently parsing.
    /// <para>This is used when handling the level number syntax rules, like which clauses are allowed for a particular level number or group item level number rules</para>
    /// </summary>
    private static readonly Stack<int> LevelStack;

    /// <summary>
    /// CurrentScope <c>CurrentSection</c> is used in the parser whenever it needs to know which section it is currently parsing (WORKING-STORAGE and LOCAL-STORAGE for example).
    /// <para>This is used when handling certain syntax rules for different sections and to add extra context needed for the DataItemInformation class's variable table.
    /// This will also be used by the DataItemInformation class during codegen to simplify the process to figuring out if a variable is static or not.</para>
    /// </summary>
    private static CurrentScope CurrentSection;

    /// <summary>
    /// List of Tokens <c>TokenList</c>: This is the main data structure that the parser will be iterating through.
    /// <para>The parser expects a list of already preprocessed and classified COBOL tokens in the form of full COBOL words (CALL, END-IF, COMPUTE for example)</para>
    /// </summary>
    private static List<Token> TokenList;

    /// <summary>
    /// Int <c>Index</c>: This is the index of the current token, used by most helper methods including Continue, Current and Lookahead.
    /// <para>The index should only move forwards, but if the previous token is needed you can use the Lookahead and LookaheadEquals methods with a negative integer parameter</para>
    /// </summary>
    private static int Index;

    /// <summary>
    /// Int <c>FileIndex</c>: This is the index of the current file name, used by the parser to point to the correct file name when showing error messages.
    /// <para>The file index should only move forwards.</para>
    /// </summary>
    private static int FileIndex;

    static Analyzer()
    {
        FileName = string.Empty;
        SourceId = new();
        SourceType = new();
        TokenList = new();
        LevelStack = new();
    }

    /// <summary>
    /// Otterkit COBOL Syntax Analyzer
    /// <para>This parser was built to be easily extensible, with some reusable COBOL parts.</para>
    /// <para>It requires a List of Tokens generated from the Lexer and the Token Classifier.</para>
    /// </summary>
    public static List<Token> Analyze(List<Token> tokenList, string fileName)
    {
        FileName = fileName;
        TokenList = tokenList;

        // Call the parser's main method
        // This should only return when the parser reaches the true EOF token
        Source();

        // If a parsing error has occured, terminate the compilation process.
        // We do not want the compiler to continue when the source code is not valid.
        if (ErrorHandler.Error) ErrorHandler.Terminate("parsing");

        // Return parsed list of tokens.
        return TokenList;

        // Source() is the main method of the parser.
        // It's responsible for parsing COBOL divisions until the EOF token.
        // If EOF was not returned as the last Token in the list then,
        // the parser has not finished reading through the list of tokens correctly.
        void Source()
        {
            IDENTIFICATION();

            if (CurrentEquals("ENVIRONMENT")) ENVIRONMENT();

            if (CurrentEquals("DATA")) DATA();

            bool notClassOrInterface = SourceType.Peek() switch
            {
                SourceUnit.Class => false,
                SourceUnit.Interface => false,
                _ => true
            };

            if (notClassOrInterface)
            {
                if (CurrentEquals("PROCEDURE")) PROCEDURE();
            }
            else if (SourceType.Peek() == SourceUnit.Class)
            {
                FactoryObject();
            }
            else if (SourceType.Peek() == SourceUnit.Interface)
            {
                InterfaceProcedure();
            }

            EndMarker();

            if (CurrentEquals("IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID"))
            {
                Source();
            }

            if (CurrentEquals("EOF") && Index < TokenList.Count - 1)
            {
                FileName = OtterkitCompiler.Options.FileNames[FileIndex++];

                Continue();
                Source();
            }
        }


        // Method responsible for parsing the IDENTIFICATION DIVISION.
        // That includes PROGRAM-ID, FUNCTION-ID, CLASS-ID, METHOD-ID, INTERFACE-ID, OBJECT, FACTORY and OPTIONS paragraphs.
        // It is also responsible for showing appropriate error messages when an error occurs in the IDENTIFICATION DIVISION.
        void IDENTIFICATION()
        {
            if (CurrentEquals("IDENTIFICATION"))
            {
                Expected("IDENTIFICATION");
                Expected("DIVISION");

                Expected(".", """
                Missing separator period at the end of this IDENTIFICATION DIVISION header, every division header must end with a separator period
                """, -1, "PROGRAM-ID", "FUNCTION-ID", "ENVIRONMENT", "DATA", "PROCEDURE");
            }

            if (!CurrentEquals("PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "METHOD-ID", "INTERFACE-ID", "OBJECT", "FACTORY"))
            {
                Expected("PROGRAM-ID", """
                Missing source unit ID name (PROGRAM-ID, FUNCTION-ID, CLASS-ID...), the identification division header is optional but every source unit must still have an ID.
                """, 0, "OPTIONS", "ENVIRONMENT", "DATA", "PROCEDURE");
            }

            if (CurrentEquals("PROGRAM-ID"))
            {
                ProgramId();
            }

            else if (CurrentEquals("FUNCTION-ID"))
            {
                FunctionId();
            }

            else if (CurrentEquals("CLASS-ID"))
            {
                ClassId();
            }

            else if (CurrentEquals("INTERFACE-ID"))
            {
                InterfaceId();
            }

            else if (SourceType.Peek() is SourceUnit.Class && CurrentEquals("FACTORY"))
            {
                Factory();
            }

            else if (SourceType.Peek() is SourceUnit.Class && CurrentEquals("OBJECT"))
            {
                Object();
            }

            else if (SourceType.Peek() is SourceUnit.Object or SourceUnit.Factory or SourceUnit.Interface && CurrentEquals("METHOD-ID"))
            {
                MethodId();
            }

            if (CurrentEquals("OPTIONS"))
            {
                Options();
            }
        }

        void Options()
        {
            bool shouldHavePeriod = false;

            Expected("OPTIONS");
            Expected(".");

            if (CurrentEquals("ARITHMETIC"))
            {
                Expected("ARITHMETIC");
                Optional("IS");
                Choice("NATIVE", "STANDARD-BINARY", "STANDARD-DECIMAL");

                shouldHavePeriod = true;
            }

            if (CurrentEquals("DEFAULT"))
            {
                Expected("DEFAULT");
                Expected("ROUNDED");
                Optional("MODE");
                Optional("IS");
                Choice(
                    "AWAY-FROM-ZERO", "NEAREST-AWAY-FROM-ZERO",
                    "NEAREST-EVEN", "NEAREST-TOWARD-ZERO",
                    "PROHIBITED", "TOWARD-GREATER",
                    "TOWARD-LESSER", "TRUNCATION"
                );

                shouldHavePeriod = true;
            }

            if (CurrentEquals("ENTRY-CONVENTION"))
            {
                Expected("ENTRY-CONVENTION");
                Optional("IS");
                Expected("COBOL");

                shouldHavePeriod = true;
            }

            if (shouldHavePeriod) Expected(".");
        }


        // The following methods are responsible for parsing the -ID paragraph.
        // That includes the program, user-defined function, method, class, interface, factory or object identifier that should be specified right after.
        // This is where SourceId and SourceType get their values for a COBOL source unit.
        void ProgramId()
        {
            Token ProgramIdentifier;

            Expected("PROGRAM-ID");
            Expected(".");

            ProgramIdentifier = Current();
            SourceId.Push(ProgramIdentifier.value);
            SourceType.Push(SourceUnit.Program);
            CurrentSection = CurrentScope.ProgramId;

            Identifier();
            if (CurrentEquals("AS"))
            {
                Expected("AS");
                String();
            }

            if (CurrentEquals("IS", "COMMON", "INITIAL", "RECURSIVE", "PROTOTYPE"))
            {
                bool isCommon = false;
                bool isInitial = false;
                bool isPrototype = false;
                bool isRecursive = false;

                Optional("IS");

                while (CurrentEquals("COMMON", "INITIAL", "RECURSIVE", "PROTOTYPE"))
                {
                    if (CurrentEquals("COMMON"))
                    {
                        Expected("COMMON");
                        isCommon = true;
                    }

                    if (CurrentEquals("INITIAL"))
                    {
                        Expected("INITIAL");
                        isInitial = true;
                    }

                    if (CurrentEquals("RECURSIVE"))
                    {
                        Expected("RECURSIVE");
                        isRecursive = true;
                    }

                    if (CurrentEquals("PROTOTYPE"))
                    {
                        Expected("PROTOTYPE");
                        SourceType.Pop();
                        SourceType.Push(SourceUnit.ProgramPrototype);
                        isPrototype = true;
                    }
                }

                if (isPrototype && (isCommon || isInitial || isRecursive))
                {
                    ErrorHandler.Parser.Report(FileName, ProgramIdentifier, ErrorType.General, """
                    Invalid prototype. Program prototypes cannot be defined as common, initial or recursive.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, ProgramIdentifier);
                }

                if (isInitial && isRecursive)
                {
                    ErrorHandler.Parser.Report(FileName, ProgramIdentifier, ErrorType.General, """
                    Invalid program definition. Initial programs cannot be defined as recursive.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, ProgramIdentifier);
                }

                if (!isPrototype) Optional("PROGRAM");
            }

            Expected(".", """
            Missing separator period at the end of this program definition
            """, -1, "OPTION", "ENVIRONMENT", "DATA", "PROCEDURE");
        }

        void FunctionId()
        {
            Expected("FUNCTION-ID");
            Expected(".");

            SourceId.Push(Current().value);
            SourceType.Push(SourceUnit.Function);
            CurrentSection = CurrentScope.FunctionId;

            Identifier();

            if (CurrentEquals("AS"))
            {
                Expected("AS");
                String();
            }

            if (CurrentEquals("IS", "PROTOTYPE"))
            {
                Optional("IS");
                Expected("PROTOTYPE");
                SourceType.Pop();
                SourceType.Push(SourceUnit.FunctionPrototype);
            }

            Expected(".", """
            Missing separator period at the end of this function definition
            """, -1, "OPTION", "ENVIRONMENT", "DATA", "PROCEDURE");
        }

        void ClassId()
        {
            Expected("CLASS-ID");
            Expected(".");

            SourceId.Push(Current().value);
            SourceType.Push(SourceUnit.Class);
            CurrentSection = CurrentScope.ClassId;

            Identifier();

            if (CurrentEquals("AS"))
            {
                Expected("AS");
                String();
            }

            if (CurrentEquals("IS", "FINAL"))
            {
                Optional("IS");
                Expected("FINAL");
            }

            if (CurrentEquals("INHERITS"))
            {
                Expected("INHERITS");
                Optional("FROM");
                if (!CurrentEquals(TokenType.Identifier))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The INHERITS FROM clause must contain at least one class or object name.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                Identifier();
                while (CurrentEquals(TokenType.Identifier)) Identifier();
            }

            if (CurrentEquals("USING"))
            {
                Expected("USING");
                if (!CurrentEquals(TokenType.Identifier))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The USING clause must contain at least one parameter.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                Identifier();
                while (CurrentEquals(TokenType.Identifier)) Identifier();
            }

            Expected(".", """
            Missing separator period at the end of this class definition
            """, -1, "OPTION", "ENVIRONMENT", "DATA", "FACTORY", "OBJECT");
        }

        void InterfaceId()
        {
            Expected("INTERFACE-ID");
            Expected(".");

            SourceId.Push(Current().value);
            SourceType.Push(SourceUnit.Interface);
            CurrentSection = CurrentScope.InterfaceId;

            Identifier();

            if (CurrentEquals("AS"))
            {
                Expected("AS");
                String();
            }

            if (CurrentEquals("INHERITS"))
            {
                Expected("INHERITS");
                Optional("FROM");
                if (!CurrentEquals(TokenType.Identifier))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The INHERITS FROM clause must contain at least one class or object name.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                Identifier();
                while (CurrentEquals(TokenType.Identifier)) Identifier();
            }

            if (CurrentEquals("USING"))
            {
                Expected("USING");
                if (!CurrentEquals(TokenType.Identifier))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The USING clause must contain at least one parameter.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                Identifier();
                while (CurrentEquals(TokenType.Identifier)) Identifier();
            }

            Expected(".", """
            Missing separator period at the end of this interface definition
            """, -1, "OPTION", "ENVIRONMENT", "DATA", "FACTORY", "OBJECT");
        }

        void MethodId()
        {
            Expected("METHOD-ID");
            Expected(".");

            CurrentSection = CurrentScope.MethodId;
            var currentSource = SourceType.Peek();

            if (currentSource != SourceUnit.Interface && CurrentEquals("GET"))
            {
                Expected("GET");
                Expected("PROPERTY");
                SourceId.Push($"GET {Current().value}");
                Identifier();

                SourceType.Push(SourceUnit.MethodGetter);
            }
            else if (currentSource != SourceUnit.Interface && CurrentEquals("SET"))
            {
                Expected("SET");
                Expected("PROPERTY");
                SourceId.Push($"SET {Current().value}");
                Identifier();

                SourceType.Push(SourceUnit.MethodSetter);
            }
            else // If not a getter or a setter
            {
                SourceId.Push(Current().value);
                Identifier();
                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    String();
                }

                if (currentSource == SourceUnit.Interface)
                {
                    SourceType.Push(SourceUnit.MethodPrototype);
                }
                else
                {
                    SourceType.Push(SourceUnit.Method);
                }
            }

            if (CurrentEquals("OVERRIDE")) Expected("OVERRIDE");

            if (CurrentEquals("IS", "FINAL"))
            {
                Optional("IS");
                Expected("FINAL");
            }

            Expected(".", """
            Missing separator period at the end of this method definition
            """, -1, "OPTION", "ENVIRONMENT", "DATA", "PROCEDURE");
        }

        void Factory()
        {
            Expected("FACTORY");
            Expected(".");

            SourceType.Push(SourceUnit.Factory);

            if (CurrentEquals("IMPLEMENTS"))
            {
                Expected("IMPLEMENTS");
                if (!CurrentEquals(TokenType.Identifier))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The IMPLEMENTS clause must contain at least one interface name.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                Identifier();
                while (CurrentEquals(TokenType.Identifier)) Identifier();

                Expected(".");
            }
        }

        void Object()
        {
            Expected("OBJECT");
            Expected(".");

            SourceType.Push(SourceUnit.Object);

            if (CurrentEquals("IMPLEMENTS"))
            {
                Expected("IMPLEMENTS");
                if (!CurrentEquals(TokenType.Identifier))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The IMPLEMENTS clause must contain at least one interface name.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                Identifier();
                while (CurrentEquals(TokenType.Identifier)) Identifier();

                Expected(".");
            }
        }

        void FactoryObject()
        {
            if (CurrentEquals("FACTORY") || CurrentEquals("IDENTIFICATION") && LookaheadEquals(3, "FACTORY"))
            {
                IDENTIFICATION();

                if (CurrentEquals("ENVIRONMENT")) ENVIRONMENT();

                if (CurrentEquals("DATA")) DATA();
                Expected("PROCEDURE");
                Expected("DIVISION");
                Expected(".");

                while (CurrentEquals("METHOD-ID") || CurrentEquals("IDENTIFICATION") && LookaheadEquals(3, "METHOD-ID"))
                {
                    IDENTIFICATION();
                    if (CurrentEquals("ENVIRONMENT")) ENVIRONMENT();

                    if (CurrentEquals("DATA")) DATA();

                    PROCEDURE();

                    EndMarker();
                }

                EndMarker();
            }

            if (CurrentEquals("OBJECT") || CurrentEquals("IDENTIFICATION") && LookaheadEquals(3, "OBJECT"))
            {
                IDENTIFICATION();

                if (CurrentEquals("ENVIRONMENT")) ENVIRONMENT();

                if (CurrentEquals("DATA")) DATA();

                Expected("PROCEDURE");
                Expected("DIVISION");
                Expected(".");

                while (CurrentEquals("METHOD-ID") || (CurrentEquals("IDENTIFICATION") && LookaheadEquals(3, "METHOD-ID")))
                {
                    IDENTIFICATION();
                    if (CurrentEquals("ENVIRONMENT")) ENVIRONMENT();

                    if (CurrentEquals("DATA")) DATA();

                    PROCEDURE();

                    EndMarker();
                }

                EndMarker();
            }
        }

        void InterfaceProcedure()
        {
            if (CurrentEquals("PROCEDURE"))
            {
                Expected("PROCEDURE");
                Expected("DIVISION");
                Expected(".", """
                Missing separator period at the end of this PROCEDURE DIVISION header, every division header must end with a separator period
                """, -1, "METHOD-ID", "END");

                while (CurrentEquals("METHOD-ID") || CurrentEquals("IDENTIFICATION") && LookaheadEquals(3, "METHOD-ID"))
                {
                    IDENTIFICATION();

                    if (CurrentEquals("ENVIRONMENT")) ENVIRONMENT();

                    if (CurrentEquals("DATA")) DATA();

                    if (CurrentEquals("PROCEDURE")) PROCEDURE();

                    EndMarker();
                }
            }
        }

        // Method responsible for parsing the ENVIRONMENT DIVISION.
        // That includes the CONFIGURATION and the INPUT-OUTPUT sections.
        // It is also responsible for showing appropriate error messages when an error occurs in the ENVIRONMENT DIVISION.
        void ENVIRONMENT()
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

        void REPOSITORY()
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
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            The USING clause must contain at least one class, object or interface name.
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());
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
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            The USING clause must contain at least one class, object or interface name.
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());
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


        // Method responsible for parsing the DATA DIVISION.
        // That includes the FILE, WORKING-STORAGE, LOCAL-STORAGE, LINKAGE, REPORT and SCREEN sections.
        // It is also responsible for showing appropriate error messages when an error occurs in the DATA DIVISION.
        void DATA()
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
        void WorkingStorage()
        {
            Expected("WORKING-STORAGE");
            Expected("SECTION");
            CurrentSection = CurrentScope.WorkingStorage;

            Expected(".");
            while (Current().type == TokenType.Numeric)
                Entries();
        }

        void LocalStorage()
        {
            Expected("LOCAL-STORAGE");
            Expected("SECTION");
            CurrentSection = CurrentScope.LocalStorage;

            Expected(".");
            while (Current().type is TokenType.Numeric)
                Entries();
        }

        void LinkageSection()
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
        void Entries()
        {
            if (CurrentEquals("77"))
                BaseEntry();

            if ((CurrentEquals("01") || CurrentEquals("1")) && !LookaheadEquals(2, "CONSTANT"))
                RecordEntry();

            if (LookaheadEquals(2, "CONSTANT"))
                ConstantEntry();
        }

        void RecordEntry()
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

        void BaseEntry()
        {
            string dataType;
            int LevelNumber = int.Parse(Current().value);
            CheckLevelNumber(LevelNumber);
            Number();

            Token DataItem = Current();
            string DataName = DataItem.value;
            Identifier();

            string DataItemHash = $"{SourceId}#{DataName}";
            if (Information.DataItems.ValueExists(DataItemHash))
            {
                DataItemInfo originalItem = Information.DataItems.GetValue(DataItemHash);

                ErrorHandler.Parser.Report(FileName, DataItem, ErrorType.General, $"""
                A data item with this name already exists in this source unit, data items must have a unique name.
                The original {originalItem.Identifier} data item can be found at line {originalItem.Line}. 
                """);
                ErrorHandler.Parser.PrettyError(FileName, DataItem);
            }
            else
            {
                Information.DataItems.AddDataItem(DataItemHash, DataName, LevelNumber, DataItem);
            }

            Information.DataItems.AddSection(DataItemHash, CurrentSection);

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
                        Information.DataItems.IsExternal(DataItemHash, true, Current().value);

                        String("""
                        Missing externalized name, the "AS" word on the EXTERNAL clause must be followed by an alphanumeric or national literal
                        """, -1);
                    }

                    if (!CurrentEquals("AS"))
                        Information.DataItems.IsExternal(DataItemHash, true, DataName);
                }

                if ((CurrentEquals("IS") && LookaheadEquals(1, "GLOBAL")) || CurrentEquals("GLOBAL"))
                {
                    Optional("IS");
                    Expected("GLOBAL");
                }

                if ((CurrentEquals("IS") && LookaheadEquals(1, "TYPEDEF")) || CurrentEquals("TYPEDEF"))
                {
                    Optional("IS");
                    Expected("TYPEDEF");

                    if (CurrentEquals("STRONG")) Expected("STRONG");
                }

                if (CurrentEquals("REDEFINES"))
                {
                    Expected("REDEFINES");
                    Identifier();
                }

                if (CurrentEquals("ALIGNED")) Expected("ALIGNED");

                if (CurrentEquals("ANY") && LookaheadEquals(1, "LENGTH"))
                {
                    Expected("ANY");
                    Expected("LENGTH");
                }

                if (CurrentEquals("BASED")) Expected("BASED");

                if (CurrentEquals("BLANK"))
                {
                    Expected("BLANK");
                    Optional("WHEN");
                    Expected("ZERO");
                }

                if (CurrentEquals("CONSTANT") && LookaheadEquals(1, "RECORD"))
                {
                    Expected("CONSTANT");
                    Expected("RECORD");
                }

                if (CurrentEquals("DYNAMIC"))
                {
                    Expected("DYNAMIC");
                    Optional("LENGTH");

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

                    Information.DataItems.AddType(DataItemHash, dataType);
                    Information.DataItems.IsPicture(DataItemHash, true);
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

                    Information.DataItems.AddPicture(DataItemHash, DataLength);
                }

                if (CurrentEquals("VALUE"))
                {
                    Expected("VALUE");

                    if (CurrentEquals(TokenType.String, TokenType.Numeric))
                    {
                        ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                        The only tokens allowed after a VALUE clause are type literals, like an Alphanumeric literal ("Hello, World!") or a Numeric literal (123.456).
                        """);
                        ErrorHandler.Parser.PrettyError(FileName, Current());
                    }

                    if (CurrentEquals(TokenType.String))
                    {
                        Information.DataItems.AddDefault(DataItemHash, Current().value);
                        String();
                    }

                    if (CurrentEquals(TokenType.Numeric))
                    {
                        Information.DataItems.AddDefault(DataItemHash, Current().value);
                        Number();
                    }
                }

                if (CurrentEquals("USAGE"))
                {
                    UsageClause(DataItemHash);
                }

            }

            if (CurrentEquals(".") && LookaheadEquals(1, TokenType.Numeric))
            {
                if (LevelStack.Count == 0)
                {
                    Information.DataItems.IsElementary(DataItemHash, true);
                }
                else
                {
                    _ = int.TryParse(Lookahead(1).value, out int outInt);
                    var currentLevel = LevelStack.Peek();

                    if (currentLevel == 1 && outInt >= 2 && outInt <= 49 || outInt >= 2 && outInt <= 49 && outInt > currentLevel)
                    {
                        Information.DataItems.IsGroup(DataItemHash, true);
                    }
                    else
                    {
                        Information.DataItems.IsElementary(DataItemHash, true);
                    }
                }
            }

            CheckClauses(DataItemHash, DataItem);

            Expected(".", """
            Missing separator period at the end of this data item definition, each data item must end with a separator period
            """, -1, "PROCEDURE");
        }

        void ConstantEntry()
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

            var DataItemHash = $"{SourceId}#{DataName}";
            if (Information.DataItems.ValueExists(DataItemHash))
            {
                var originalItem = Information.DataItems.GetValue(DataItemHash);

                ErrorHandler.Parser.Report(FileName, Lookahead(-1), ErrorType.General, $"""
                A data item with this name already exists in this program, data items in a program must have a unique name.
                The original {originalItem.Identifier} data item can be found on line {originalItem.Line}. 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Lookahead(-1));
            }
            else
            {
                Information.DataItems.AddDataItem(DataItemHash, DataName, LevelNumber, Lookahead(-1));
            }

            Information.DataItems.IsConstant(DataItemHash, true);
            Information.DataItems.AddSection(DataItemHash, CurrentSection);

            Expected("CONSTANT");
            if (CurrentEquals("IS") || CurrentEquals("GLOBAL"))
            {
                Optional("IS");
                Expected("GLOBAL");
                Information.DataItems.IsGlobal(DataItemHash, true);
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

        void CheckLevelNumber(int level)
        {
            if (level is 77) return;

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
                if (level == lowerLevel) return;

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

        void CheckClauses(string dataItemHash, Token itemToken)
        {
            DataItemInfo dataItem = Information.DataItems.GetValue(dataItemHash);

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

        // Method responsible for parsing the PROCEDURE DIVISION.
        // That includes the user-defined paragraphs, sections and declaratives
        // or when parsing OOP COBOL code, it's responsible for parsing COBOL methods, objects and factories. 
        // It is also responsible for showing appropriate error messages when an error occurs in the PROCEDURE DIVISION.
        void PROCEDURE()
        {
            Expected("PROCEDURE");
            Expected("DIVISION");
            CurrentSection = CurrentScope.ProcedureDivision;
            var currentSource = SourceType.Peek();

            if (CurrentEquals("USING"))
            {
                Expected("USING");

                while (CurrentEquals("BY", "REFERENCE", "VALUE"))
                {
                    if (CurrentEquals("BY") && !LookaheadEquals(1, "VALUE", "REFERENCE"))
                    {
                        ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                        The USING BY clause in the procedure division header must be followed by "VALUE" or "REFERENCE"
                        """);
                        ErrorHandler.Parser.PrettyError(FileName, Current());

                        CombinedAnchorPoint(TokenContext.IsStatement, "RETURNING", ".");
                    }

                    if (CurrentEquals("REFERENCE") || CurrentEquals("BY") && LookaheadEquals(1, "REFERENCE"))
                    {
                        Optional("BY");
                        Expected("REFERENCE");

                        if (CurrentEquals("OPTIONAL")) Expected("OPTIONAL");

                        if (!CurrentEquals(TokenType.Identifier))
                        {
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            The USING BY VALUE clause must contain at least one data item name.
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());
                        }

                        Identifier();
                        while (CurrentEquals(TokenType.Identifier) || CurrentEquals("OPTIONAL"))
                        {
                            if (CurrentEquals("OPTIONAL")) Expected("OPTIONAL");
                            Identifier();
                        }
                    }

                    if (CurrentEquals("VALUE") || CurrentEquals("BY") && LookaheadEquals(1, "VALUE"))
                    {
                        Optional("BY");
                        Expected("VALUE");
                        if (!CurrentEquals(TokenType.Identifier))
                        {
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            The USING BY VALUE clause must contain at least one data item name.
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());
                        }

                        Identifier();
                        while (CurrentEquals(TokenType.Identifier)) Identifier();
                    }
                }
            }

            if (SourceType.Peek() == SourceUnit.Function)
            {
                Expected("RETURNING");
                ReturningDataName();
            }
            else if (CurrentEquals("RETURNING"))
            {
                Expected("RETURNING");
                ReturningDataName();
            }

            Expected(".", """
            Missing separator period at the end of this PROCEDURE DIVISION header, every division header must end with a separator period
            """, -1, TokenContext.IsStatement);

            bool isProcedureDeclarative = CurrentEquals("DECLARATIVES")
                || CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "SECTION");

            bool canContainStatements = currentSource switch
            {
                SourceUnit.FunctionPrototype => false,
                SourceUnit.ProgramPrototype => false,
                SourceUnit.MethodPrototype => false,
                _ => true
            };

            if (canContainStatements && !isProcedureDeclarative) ParseStatements();

            if (canContainStatements && isProcedureDeclarative) DeclarativeProcedure();

            if (!canContainStatements && (CurrentEquals(TokenContext.IsStatement) || CurrentEquals(TokenType.Identifier)))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The procedure division of function, program and method prototypes must not contain any statements, sections or paragraphs
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());

                AnchorPoint("END");
            }

        }

        // This method is part of the PROCEDURE DIVISION parsing. It's used to parse the "RETURNING" data item specified in
        // the PROCEDURE DIVISION header. It's separate from the previous method because its code is needed more than once.
        // COBOL user-defined functions should always return a data item.
        void ReturningDataName()
        {
            if (!CurrentEquals(TokenType.Identifier))
            {
                ErrorHandler.Parser.Report(FileName, Lookahead(-1), ErrorType.General, """
                Missing returning data item after this RETURNING definition.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Lookahead(-1));
                return;
            }

            Identifier();
        }

        void EndMarker()
        {
            SourceUnit currentSource = SourceType.Peek();

            if (currentSource != SourceUnit.Program && !CurrentEquals("END") || currentSource == SourceUnit.Program && (!CurrentEquals(TokenType.EOF) && !CurrentEquals("END")))
            {
                string errorMessageChoice = currentSource switch
                {
                    SourceUnit.Program or SourceUnit.ProgramPrototype => """
                    Missing END PROGRAM marker. If another source unit is present after the end of a program or program prototype, the program must contain an END marker.
                    """,

                    SourceUnit.Function or SourceUnit.FunctionPrototype => """
                    Missing END FUNCTION marker. User-defined functions and function prototypes must always end with an END FUNCTION marker.
                    """,

                    SourceUnit.Method or SourceUnit.MethodPrototype or SourceUnit.MethodGetter or SourceUnit.MethodSetter => """
                    Missing END METHOD marker. Method definitions and property getter/setter must always end with an END METHOD marker.
                    """,

                    SourceUnit.Class => """
                    Missing END CLASS marker. Class definitions must always end with an END CLASS marker.
                    """,

                    SourceUnit.Interface => """
                    Missing END INTERFACE marker. Interface definitions must always end with an END INTERFACE marker.
                    """,

                    SourceUnit.Factory or SourceUnit.Object => """
                    Missing END FACTORY and END OBJECT marker. Factory and object definitions must always end with an END FACTORY and END OBJECT marker.
                    """,

                    _ => throw new UnreachableException()
                };

                ErrorHandler.Parser.Report(FileName, Lookahead(-1), ErrorType.General, errorMessageChoice);
                ErrorHandler.Parser.PrettyError(FileName, Lookahead(-1));
                return;
            }

            if (currentSource == SourceUnit.Program && CurrentEquals("EOF"))
            {
                SourceType.Pop();
                return;
            }

            switch (currentSource)
            {
                case SourceUnit.Program:
                case SourceUnit.ProgramPrototype:
                    SourceType.Pop();

                    Expected("END");
                    Expected("PROGRAM");
                    Identifier(SourceId.Pop());
                    Expected(".", """
                    Missing separator period at the end of this END PROGRAM definition
                    """, -1, "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
                    break;

                case SourceUnit.Function:
                case SourceUnit.FunctionPrototype:
                    SourceType.Pop();

                    Expected("END");
                    Expected("FUNCTION");
                    Identifier(SourceId.Pop());
                    Expected(".", """
                    Missing separator period at the end of this END FUNCTION definition
                    """, -1, "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
                    break;

                case SourceUnit.Method:
                case SourceUnit.MethodPrototype:
                case SourceUnit.MethodGetter:
                case SourceUnit.MethodSetter:
                    SourceType.Pop();

                    Expected("END");
                    Expected("METHOD");
                    if (currentSource is SourceUnit.Method or SourceUnit.MethodPrototype)
                        Identifier(SourceId.Pop());

                    if (currentSource is SourceUnit.MethodGetter or SourceUnit.MethodSetter)
                        SourceId.Pop();

                    Expected(".", """
                    Missing separator period at the end of this END METHOD definition
                    """, -1, "IDENTIFICATION", "METHOD-ID", "OBJECT", "FACTORY");
                    break;

                case SourceUnit.Class:
                    SourceType.Pop();

                    Expected("END");
                    Expected("CLASS");
                    Identifier(SourceId.Pop());
                    Expected(".", """
                    Missing separator period at the end of this END CLASS definition
                    """, -1, "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
                    break;

                case SourceUnit.Interface:
                    SourceType.Pop();

                    Expected("END");
                    Expected("INTERFACE");
                    Identifier(SourceId.Pop());
                    Expected(".", """
                    Missing separator period at the end of this END INTERFACE definition
                    """, -1, "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
                    break;

                case SourceUnit.Factory:
                    SourceType.Pop();

                    Expected("END");
                    Expected("FACTORY");
                    Expected(".", """
                    Missing separator period at the end of this END FACTORY definition
                    """, -1, "OBJECT", "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
                    break;

                case SourceUnit.Object:
                    SourceType.Pop();

                    Expected("END");
                    Expected("OBJECT");
                    Expected(".", """
                    Missing separator period at the end of this END FACTORY definition
                    """, -1, "END", "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
                    break;

            }
        }


        void ParseStatements(bool isNested = false)
        {
            bool errorCheck = Current().context != TokenContext.IsStatement
                && !(CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, ".") && !isNested)
                && !(CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "SECTION") && !isNested);

            if (errorCheck)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
                Expected the start of a statement. Instead found "{Current().value}"
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());

                AnchorPoint(TokenContext.IsStatement);
            }

            while (!isNested ? !CurrentEquals("EOF", "END") && !(CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "SECTION")) : CurrentEquals(TokenContext.IsStatement))
            {
                Statement(isNested);

                ScopeTerminator(isNested);

                if (isNested && !CurrentEquals(TokenContext.IsStatement)) return;

                if (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "SECTION")) return;
            }
        }

        // Recursive method responsible for parsing ALL COBOL statements.
        // The while loop continues parsing statements while the current token is a statement reserved word,
        // This might look like it could cause issues by parsing two statements in a single loop, but that
        // can only happen on nested statements, and in those cases it still parses every statement correctly.
        // For other statements, the separator period is required, which is then handled by the ScopeTerminator()
        void Statement(bool isNested = false)
        {
            if (CurrentEquals("ACCEPT"))
                ACCEPT();

            else if (CurrentEquals("ADD"))
                ADD();

            else if (CurrentEquals("ALLOCATE"))
                ALLOCATE();

            else if (CurrentEquals("CALL"))
                CALL();

            else if (CurrentEquals("CANCEL"))
                CANCEL();

            else if (CurrentEquals("CLOSE"))
                CLOSE();

            else if (CurrentEquals("COMMIT"))
                COMMIT();

            else if (CurrentEquals("CONTINUE"))
                CONTINUE();

            else if (CurrentEquals("COMPUTE"))
                COMPUTE();

            else if (CurrentEquals("DISPLAY"))
                DISPLAY();

            else if (CurrentEquals("DIVIDE"))
                DIVIDE();

            else if (CurrentEquals("DELETE"))
                DELETE();

            else if (CurrentEquals("IF"))
                IF();

            else if (CurrentEquals("INITIALIZE"))
                INITIALIZE();

            else if (CurrentEquals("INITIATE"))
                INITIATE();

            else if (CurrentEquals("INSPECT"))
                INSPECT();

            else if (CurrentEquals("INVOKE"))
                INVOKE();

            else if (CurrentEquals("MERGE"))
                MERGE();

            else if (CurrentEquals("MULTIPLY"))
                MULTIPLY();

            else if (CurrentEquals("MOVE"))
                MOVE();

            else if (CurrentEquals("OPEN"))
                OPEN();

            else if (CurrentEquals("EXIT"))
                EXIT();

            else if (CurrentEquals("EVALUATE"))
                EVALUATE();

            else if (CurrentEquals("FREE"))
                FREE();

            else if (CurrentEquals("GENERATE"))
                GENERATE();

            else if (CurrentEquals("GO"))
                GO();

            else if (CurrentEquals("GOBACK"))
                GOBACK();

            else if (CurrentEquals("SUBTRACT"))
                SUBTRACT();

            else if (CurrentEquals("PERFORM"))
                PERFORM();

            else if (CurrentEquals("RELEASE"))
                RELEASE();

            else if (CurrentEquals("RAISE"))
                RAISE();

            else if (CurrentEquals("READ"))
                READ();

            else if (CurrentEquals("RECEIVE"))
                RECEIVE();

            else if (CurrentEquals("RESUME"))
                RESUME();

            else if (CurrentEquals("RETURN"))
                RETURN();

            else if (CurrentEquals("REWRITE"))
                REWRITE();

            else if (CurrentEquals("ROLLBACK"))
                ROLLBACK();

            else if (CurrentEquals("SEARCH"))
                SEARCH();

            else if (CurrentEquals("SEND"))
                SEND();

            else if (CurrentEquals("SET"))
                SET();

            else if (CurrentEquals("SORT"))
                SORT();

            else if (CurrentEquals("START"))
                START();

            else if (CurrentEquals("STOP"))
                STOP();

            else if (CurrentEquals("STRING"))
                STRING();

            else if (CurrentEquals("SUPPRESS"))
                SUPPRESS();

            else if (CurrentEquals("TERMINATE"))
                TERMINATE();

            else if (CurrentEquals("UNLOCK"))
                UNLOCK();

            else if (CurrentEquals("UNSTRING"))
                UNSTRING();

            else if (CurrentEquals("VALIDATE"))
                VALIDATE();

            else if (CurrentEquals("WRITE"))
                WRITE();

            else if (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, ".") && !isNested)
                PARAGRAPH();
        }

        void DeclarativeProcedure()
        {
            if (CurrentEquals("DECLARATIVES"))
            {
                Expected("DECLARATIVES");
                Expected(".");

                Identifier();
                Expected("SECTION");
                Expected(".");
                UseStatement();
                ParseStatements();

                while (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "SECTION"))
                {
                    Identifier();
                    Expected("SECTION");
                    Expected(".");
                    UseStatement();
                    ParseStatements();
                }

                Expected("END");
                Expected("DECLARATIVES");
                Expected(".");
            }

            while (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "SECTION"))
            {
                Identifier();
                Expected("SECTION");
                Expected(".");
                ParseStatements();
            }
        }

        void UseStatement()
        {
            Expected("USE");

            bool exceptionObject = CurrentEquals("AFTER") && LookaheadEquals(1, "EXCEPTION") && LookaheadEquals(2, "OBJECT")
                || CurrentEquals("AFTER") && LookaheadEquals(1, "EO")
                || CurrentEquals("EXCEPTION") && LookaheadEquals(1, "OBJECT")
                || CurrentEquals("EO");

            bool exceptionCondition = CurrentEquals("AFTER") && LookaheadEquals(1, "EXCEPTION") && LookaheadEquals(2, "CONDITION")
                || CurrentEquals("AFTER") && LookaheadEquals(1, "EC")
                || CurrentEquals("EXCEPTION") && LookaheadEquals(1, "CONDITION")
                || CurrentEquals("EC");

            bool reporting = CurrentEquals("GLOBAL") && LookaheadEquals(1, "BEFORE") || CurrentEquals("BEFORE");

            bool fileException = CurrentEquals("GLOBAL") && LookaheadEquals(1, "AFTER", "STANDARD", "EXCEPTION", "ERROR")
                || CurrentEquals("AFTER", "STANDARD", "EXCEPTION", "ERROR");

            if (exceptionObject)
            {
                Optional("AFTER");
                if (CurrentEquals("EO"))
                {
                    Expected("EO");
                }
                else
                {
                    Expected("EXCEPTION");
                    Expected("OBJECT");
                }

                Identifier();
            }
            else if (exceptionCondition)
            {
                Optional("AFTER");
                if (CurrentEquals("EO"))
                {
                    Expected("EO");
                }
                else
                {
                    Expected("EXCEPTION");
                    Expected("OBJECT");
                }

                Identifier();
            }
            else if (reporting)
            {
                if (CurrentEquals("GLOBAL")) Expected("GLOBAL");

                Expected("BEFORE");
                Expected("REPORTING");
                Identifier();
            }
            else if (fileException)
            {
                if (CurrentEquals("GLOBAL")) Expected("GLOBAL");

                Optional("AFTER");
                Optional("STANDARD");
                Choice("EXCEPTION", "ERROR");
                Optional("PROCEDURE");
                Optional("ON");

                if (CurrentEquals("INPUT", "OUTPUT", "I-O", "EXTEND"))
                {
                    Choice("INPUT", "OUTPUT", "I-O", "EXTEND");
                }
                else
                {
                    Identifier();
                    while (CurrentEquals(TokenType.Identifier))
                        Identifier();
                }
            }
            else
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                Expected AFTER EXCEPTION OBJECT, AFTER EXCEPTION CONDITION, BEFORE REPORTING or AFTER EXCEPTION/ERROR
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());

                AnchorPoint(TokenContext.IsStatement);
            }

            ScopeTerminator(false);
        }

        // This method handles COBOL's slightly inconsistent separator period rules.
        // Statements that are nested inside another statement cannot end with a separator period,
        // since that separator period would mean the end of the containing statement and not the contained statement.
        void ScopeTerminator(bool isNested)
        {
            if (isNested) return;

            Expected(".");
        }


        // Statement parsing methods
        // All the following uppercased methods are responsible for parsing a single COBOL statement
        // When a new method is added here to parse a new statement, we need to add it to the Statement() method as well.
        // Adding extra statements to the parser only requires a new method here, and an if statement added to the Statement() method
        void DISPLAY()
        {
            Expected("DISPLAY");

            if (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "AT", "LINE", "COLUMN", "COL"))
            {
                bool isConditional = false;

                Identifier();
                Optional("AT");
                LineColumn();
                OnException(ref isConditional);

                if (isConditional) Expected("END-COMPUTE");
                return;
            }

            if (NotIdentifierOrLiteral())
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "an identifier or a literal");
                ErrorHandler.Parser.PrettyError(FileName, Current());

                AnchorPoint("UPON", "WITH", "NO");
            }

            switch (Current().type)
            {
                case TokenType.Identifier: Identifier(); break;
                case TokenType.Numeric: Number(); break;
                case TokenType.String: String(); break;
            }

            while (IdentifierOrLiteral())
            {
                switch (Current().type)
                {
                    case TokenType.Identifier: Identifier(); break;
                    case TokenType.Numeric: Number(); break;
                    case TokenType.String: String(); break;
                }
            }

            if (CurrentEquals("UPON"))
            {
                Expected("UPON");
                Choice("STANDARD-OUTPUT", "STANDARD-ERROR");
            }

            if (CurrentEquals("WITH", "NO"))
            {
                Optional("WITH");
                Expected("NO");
                Expected("ADVANCING");
            }

            Optional("END-DISPLAY");
        }

        void ACCEPT()
        {
            bool isConditional = false;

            Expected("ACCEPT");
            Identifier();
            if (CurrentEquals("FROM"))
            {
                Expected("FROM");
                switch (Current().value)
                {
                    case "STANDARD-INPUT":
                    case "COMMAND-LINE":
                        Choice("STANDARD-INPUT", "COMMAND-LINE");
                        break;

                    case "DATE":
                        Expected("DATE");
                        Optional("YYYYMMDD");
                        break;

                    case "DAY":
                        Expected("DAY");
                        Optional("YYYYDDD");
                        break;

                    case "DAY-OF-WEEK":
                        Expected("DAY-OF-WEEK");
                        break;

                    case "TIME":
                        Expected("TIME");
                        break;
                }
            }
            else if (CurrentEquals("AT", "LINE", "COLUMN", "COL"))
            {
                Optional("AT");
                if (!CurrentEquals("LINE", "COLUMN", "COL"))
                {
                    ErrorHandler.Parser.Report(FileName, Lookahead(-1), ErrorType.General, """
                    When specifying the AT keyword, it must be followed by a LINE NUMBER, COLUMN/COL NUMBER or both.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Lookahead(-1));
                }
                LineColumn();
                OnException(ref isConditional);
            }

            if (isConditional) Expected("END-ACCEPT");
        }

        void ALLOCATE()
        {
            Expected("ALLOCATE");
            if (CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, "CHARACTERS") && !LookaheadEquals(1, TokenType.Symbol))
                Identifier();

            if (CurrentEquals(TokenType.Identifier, TokenType.Numeric))
            {
                Arithmetic("CHARACTERS");
                Expected("CHARACTERS");
            }

            if (CurrentEquals("INITIALIZED"))
                Expected("INITIALIZED");

            if (CurrentEquals("RETURNING"))
            {
                Expected("RETURNING");
                Identifier();
            }
        }

        void COMPUTE()
        {
            bool isConditional = false;

            Expected("COMPUTE");
            if (!CurrentEquals(TokenType.Identifier))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier");
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            while (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }

            Expected("=");
            if (NotIdentifierOrLiteral())
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "an identifier, numeric literal or a valid arithmetic symbol");
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            Arithmetic(".");

            if (CurrentEquals(".")) return;

            SizeError(ref isConditional);

            if (isConditional) Expected("END-COMPUTE");
        }

        void CALL()
        {
            bool isConditional = false;
            bool isPrototype = false;

            Expected("CALL");
            if (CurrentEquals(TokenType.Identifier, TokenType.String))
            {
                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    String();
                }

                if (CurrentEquals("AS"))
                {
                    isPrototype = true;
                    Expected("AS");
                }
            }
            else
            {
                isPrototype = true;
            }

            if (isPrototype && CurrentEquals("NESTED"))
            {
                Expected("NESTED");
            }
            else if (isPrototype && !CurrentEquals("NESTED"))
            {
                Identifier();
            }

            if (!isPrototype && CurrentEquals("USING"))
            {
                Expected("USING");

                while (CurrentEquals("BY", "REFERENCE", "CONTENT"))
                {
                    if (CurrentEquals("VALUE") || CurrentEquals("BY") && LookaheadEquals(1, "VALUE"))
                    {
                        ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                        The USING BY VALUE clause must only be specified in a program prototype call.
                        """);
                        ErrorHandler.Parser.PrettyError(FileName, Current());
                        Continue();

                        AnchorPoint("BY", "REFERENCE", "CONTENT", "RETURNING");
                        if (CurrentEquals("RETURNING", ".")) break;

                    }

                    if (CurrentEquals("REFERENCE") || CurrentEquals("BY") && LookaheadEquals(1, "REFERENCE"))
                    {
                        Optional("BY");
                        Expected("REFERENCE");

                        if (CurrentEquals("OPTIONAL")) Expected("OPTIONAL");

                        if (!CurrentEquals(TokenType.Identifier))
                        {
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            The USING BY REFERENCE clause must contain at least one data item name.
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());
                        }

                        Identifier();
                        while (CurrentEquals(TokenType.Identifier) || CurrentEquals("OPTIONAL"))
                        {
                            if (CurrentEquals("OPTIONAL")) Expected("OPTIONAL");
                            Identifier();
                        }
                    }

                    if (CurrentEquals("CONTENT") || CurrentEquals("BY") && LookaheadEquals(1, "CONTENT"))
                    {
                        Optional("BY");
                        Expected("CONTENT");
                        if (!CurrentEquals(TokenType.Identifier))
                        {
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            The USING BY CONTENT clause must contain at least one data item name.
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());

                            AnchorPoint("BY", "REFERENCE", "CONTENT", "RETURNING");
                            if (CurrentEquals("RETURNING", ".")) break;
                        }

                        Identifier();
                        while (CurrentEquals(TokenType.Identifier)) Identifier();
                    }
                }
            }
            else if (isPrototype && CurrentEquals("USING"))
            {
                Expected("USING");

                while (CurrentEquals("BY", "REFERENCE", "CONTENT", "VALUE"))
                {
                    if (CurrentEquals("REFERENCE") || CurrentEquals("BY") && LookaheadEquals(1, "REFERENCE"))
                    {
                        Optional("BY");
                        Expected("REFERENCE");

                        if (!CurrentEquals(TokenType.Identifier) && !CurrentEquals("OMMITED"))
                        {
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            The USING BY REFERENCE clause must contain at least one data item name.
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());
                        }

                        if (CurrentEquals("OMMITED"))
                        {
                            Expected("OMMITED");
                        }
                        else
                        {
                            Identifier();
                        }
                    }

                    if (CurrentEquals("CONTENT") || CurrentEquals("BY") && LookaheadEquals(1, "CONTENT"))
                    {
                        Optional("BY");
                        Expected("CONTENT");
                        if (NotIdentifierOrLiteral())
                        {
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            The USING BY CONTENT clause must contain at least one data item name or literal.
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());
                        }

                        if (CurrentEquals(TokenType.Identifier))
                        {
                            Identifier();
                        }
                        else if (CurrentEquals(TokenType.Numeric))
                        {
                            Number();
                        }
                        else
                        {
                            String();
                        }
                    }

                    if (CurrentEquals("VALUE") || CurrentEquals("BY") && LookaheadEquals(1, "VALUE"))
                    {
                        Optional("BY");
                        Expected("VALUE");
                        if (NotIdentifierOrLiteral())
                        {
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            The USING BY VALUE clause must contain at least one data item name or literal.
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());
                        }

                        if (CurrentEquals(TokenType.Identifier))
                        {
                            Identifier();
                        }
                        else if (CurrentEquals(TokenType.Numeric))
                        {
                            Number();
                        }
                        else
                        {
                            String();
                        }
                    }
                }
            }

            if (CurrentEquals("RETURNING"))
            {
                Expected("RETURNING");
                Identifier();
            }

            OnException(ref isConditional);

            if (isConditional) Expected("END-CALL");
        }

        void CONTINUE()
        {
            Expected("CONTINUE");
            if (CurrentEquals("AFTER"))
            {
                Expected("AFTER");
                Arithmetic("SECONDS");
                Expected("SECONDS");
            }
        }

        void ADD()
        {
            bool isConditional = false;

            Expected("ADD");

            if (CurrentEquals("CORRESPONDING", "CORR"))
            {
                Continue();
                Identifier();
                Expected("TO");
                Identifier();
                SizeError(ref isConditional);

                if (isConditional) Expected("END-ADD");
                return;
            }

            if (!CurrentEquals(TokenType.Identifier, TokenType.Numeric))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            while (CurrentEquals(TokenType.Identifier, TokenType.Numeric))
            {
                if (CurrentEquals(TokenType.Identifier))
                    Identifier();

                if (CurrentEquals(TokenType.Numeric))
                    Number();
            }

            if (CurrentEquals("TO") && LookaheadEquals(2, "GIVING"))
            {
                Optional("TO");
                switch (Current().type)
                {
                    case TokenType.Identifier:
                        Identifier();
                        break;

                    case TokenType.Numeric:
                        Number();
                        break;

                    default:
                        ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                        ErrorHandler.Parser.PrettyError(FileName, Current());
                        break;
                }

                Expected("GIVING");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier");
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else if (CurrentEquals("GIVING"))
            {
                Expected("GIVING");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier");
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else if (CurrentEquals("TO"))
            {
                Expected("TO");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier");
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "TO or GIVING");
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            SizeError(ref isConditional);

            if (isConditional) Expected("END-ADD");
        }

        void SUBTRACT()
        {
            bool isConditional = false;

            Expected("SUBTRACT");
            if (Current().type != TokenType.Identifier && Current().type != TokenType.Numeric)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            while (Current().type == TokenType.Identifier
                || Current().type == TokenType.Numeric
            )
            {
                if (Current().type == TokenType.Identifier)
                    Identifier();

                if (Current().type == TokenType.Numeric)
                    Number();
            }

            if (CurrentEquals("FROM") && LookaheadEquals(2, "GIVING"))
            {
                Optional("FROM");
                switch (Current().type)
                {
                    case TokenType.Identifier:
                        Identifier();
                        break;

                    case TokenType.Numeric:
                        Number();
                        break;

                    default:
                        ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                        ErrorHandler.Parser.PrettyError(FileName, Current());
                        break;
                }

                Expected("GIVING");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier");
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else if (CurrentEquals("FROM"))
            {
                Expected("FROM");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier");
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "FROM");
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            SizeError(ref isConditional);

            if (isConditional)
                Expected("END-SUBTRACT");
        }

        void IF()
        {
            Expected("IF");
            Condition("THEN");
            Optional("THEN");
            if (CurrentEquals("NEXT") && LookaheadEquals(1, "SENTENCE"))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                Unsupported phrase: NEXT SENTENCE is an archaic feature. This phrase can be confusing and is a common source of errors.
                The CONTINUE statement can be used to accomplish the same functionality while being much clearer and less prone to error
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            ParseStatements(true);
            if (CurrentEquals("ELSE"))
            {
                Expected("ELSE");
                ParseStatements(true);
            }

            Expected("END-IF");
        }

        void INITIALIZE()
        {
            Expected("INITIALIZE");
            Identifier();
            while (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }

            if (CurrentEquals("WITH", "FILLER"))
            {
                Optional("WITH");
                Expected("FILLER");
            }

            bool IsCategoryName(string value) => value switch
            {
                "ALPHABETIC" => true,
                "ALPHANUMERIC" => true,
                "ALPHANUMERIC-EDITED" => true,
                "BOOLEAN" => true,
                "DATA-POINTER" => true,
                "FUNCTION-POINTER" => true,
                "MESSAGE-TAG" => true,
                "NATIONAL" => true,
                "NATIONAL-EDITED" => true,
                "NUMERIC" => true,
                "NUMERIC-EDITED" => true,
                "OBJECT-REFERENCE" => true,
                "PROGRAM-POINTER" => true,
                _ => false
            };

            if (IsCategoryName(Current().value))
            {
                Expected(Current().value);
                Optional("TO");
                Expected("VALUE");
            }
            else if (CurrentEquals("ALL"))
            {
                Expected("ALL");
                Optional("TO");
                Expected("VALUE");
            }

            if (CurrentEquals("THEN", "REPLACING"))
            {
                Optional("THEN");
                Expected("REPLACING");
                if (IsCategoryName(Current().value))
                {
                    Expected(Current().value);
                    Optional("DATA");
                    Expected("BY");

                    if (NotIdentifierOrLiteral())
                    {
                        ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                        Expected an identifier if message tag, object reference or a pointer as specified, and an identifier or literal otherwise
                        """);
                        ErrorHandler.Parser.PrettyError(FileName, Current());

                        AnchorPoint(TokenContext.IsStatement);
                    }

                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else if (CurrentEquals(TokenType.String))
                    {
                        String();
                    }
                    else if (CurrentEquals(TokenType.Numeric))
                    {
                        Number();
                    }

                    while (IsCategoryName(Current().value))
                    {
                        Expected(Current().value);
                        Optional("DATA");
                        Expected("BY");

                        if (NotIdentifierOrLiteral())
                        {
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            Expected an identifier if message tag, object reference or a pointer as specified, and an identifier or literal otherwise
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());

                            AnchorPoint(TokenContext.IsStatement);
                        }

                        if (CurrentEquals(TokenType.Identifier))
                        {
                            Identifier();
                        }
                        else if (CurrentEquals(TokenType.String))
                        {
                            String();
                        }
                        else if (CurrentEquals(TokenType.Numeric))
                        {
                            Number();
                        }

                    }

                }
            }

            if (CurrentEquals("THEN", "TO", "DEFAULT"))
            {
                Optional("THEN");
                Optional("TO");
                Expected("DEFAULT");
            }
        }

        void INITIATE()
        {
            Expected("INITIATE");
            if (Current().type != TokenType.Identifier)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The INITIATE statement must only contain report entry identifiers defined in the report section.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            Identifier();
            while (Current().type == TokenType.Identifier)
                Identifier();

            if (!CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The INITIATE statement must only contain report entry identifiers defined in the report section.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
        }

        void INSPECT()
        {
            Expected("INSPECT");
            if (CurrentEquals("BACKWARD")) Expected("BACKWARD");

            Identifier();

            if (CurrentEquals("CONVERTING"))
            {
                Expected("CONVERTING");
                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    String();
                }

                Expected("TO");
                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    String();
                }

                AfterBeforePhrase();
            }
            else if (CurrentEquals("REPLACING"))
            {
                Expected("REPLACING");
                ReplacingPhrase();
            }
            else if (CurrentEquals("TALLYING"))
            {
                Expected("TALLYING");
                TallyingPhrase();
                if (CurrentEquals("REPLACING"))
                {
                    Expected("REPLACING");
                    ReplacingPhrase();
                }
            }
        }

        void INVOKE()
        {
            Expected("INVOKE");
            Identifier(UsageType.ObjectReference);
            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }
            else
            {
                String();
            }

            if (CurrentEquals("USING"))
            {
                Expected("USING");

                while (CurrentEquals("BY", "REFERENCE", "CONTENT", "VALUE"))
                {
                    if (CurrentEquals("REFERENCE") || CurrentEquals("BY") && LookaheadEquals(1, "REFERENCE"))
                    {
                        Optional("BY");
                        Expected("REFERENCE");

                        if (!CurrentEquals(TokenType.Identifier) && !CurrentEquals("OMMITED"))
                        {
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            The USING BY REFERENCE clause must contain at least one data item name.
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());
                        }

                        if (CurrentEquals("OMMITED"))
                        {
                            Expected("OMMITED");
                        }
                        else
                        {
                            Identifier();
                        }
                    }

                    if (CurrentEquals("CONTENT") || CurrentEquals("BY") && LookaheadEquals(1, "CONTENT"))
                    {
                        Optional("BY");
                        Expected("CONTENT");
                        if (NotIdentifierOrLiteral())
                        {
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            The USING BY CONTENT clause must contain at least one data item name or literal.
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());
                        }

                        if (CurrentEquals(TokenType.Identifier))
                        {
                            Identifier();
                        }
                        else if (CurrentEquals(TokenType.Numeric))
                        {
                            Number();
                        }
                        else
                        {
                            String();
                        }
                    }

                    if (CurrentEquals("VALUE") || CurrentEquals("BY") && LookaheadEquals(1, "VALUE"))
                    {
                        Optional("BY");
                        Expected("VALUE");
                        if (NotIdentifierOrLiteral())
                        {
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            The USING BY VALUE clause must contain at least one data item name or literal.
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());
                        }

                        if (CurrentEquals(TokenType.Identifier))
                        {
                            Identifier();
                        }
                        else if (CurrentEquals(TokenType.Numeric))
                        {
                            Number();
                        }
                        else
                        {
                            String();
                        }
                    }
                }
            }

            if (CurrentEquals("RETURNING"))
            {
                Expected("RETURNING");
                Identifier();
            }
        }

        void MERGE()
        {
            Expected("MERGE");
            Identifier();

            Optional("ON");
            if (CurrentEquals("ASCENDING"))
            {
                Expected("ASCENDING");
            }
            else
            {
                Expected("DESCENDING");
            }

            Optional("KEY");

            if (!CurrentEquals(TokenType.Identifier))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The ON ASCENDING / DESCENDING KEY clause must only contain key data item names.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            Identifier();
            while (Current().type == TokenType.Identifier)
                Identifier();


            while (CurrentEquals("ON", "ASCENDING", "DESCENDING"))
            {
                Optional("ON");
                if (CurrentEquals("ASCENDING"))
                {
                    Expected("ASCENDING");
                }
                else
                {
                    Expected("DESCENDING");
                }

                Optional("KEY");

                if (!CurrentEquals(TokenType.Identifier))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The ON ASCENDING / DESCENDING KEY clause must only contain key data item names.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                Identifier();
                while (Current().type == TokenType.Identifier)
                    Identifier();
            }

            if (CurrentEquals("COLLATING", "SEQUENCE"))
            {
                Optional("COLLATING");
                Expected("SEQUENCE");
                if (CurrentEquals("IS") && LookaheadEquals(1, TokenType.Identifier) || CurrentEquals(TokenType.Identifier))
                {
                    Optional("IS");
                    Identifier();

                    if (CurrentEquals(TokenType.Identifier)) Identifier();
                }
                else
                {
                    if (!CurrentEquals("FOR", "ALPHANUMERIC", "NATIONAL"))
                    {
                        ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                        The COLLATING SEQUENCE clause must contain at least 1 alphabet name (max of 2 alphabet names) or at least one FOR ALPHANUMERIC and FOR NATIONAL clauses.
                        """);
                        ErrorHandler.Parser.PrettyError(FileName, Current());

                        CombinedAnchorPoint(TokenContext.IsStatement, "USING");
                    }

                    ForAlphanumericForNational();
                }
            }

            Expected("USING");
            Identifier();
            Identifier();
            while (Current().type == TokenType.Identifier)
                Identifier();

            if (CurrentEquals("OUTPUT"))
            {
                Expected("OUTPUT");
                Expected("PROCEDURE");
                Optional("IS");
                Identifier();

                if (CurrentEquals("THROUGH", "THRU"))
                {
                    Choice("THROUGH", "THRU");
                    Identifier();
                }
            }
            else
            {
                Expected("GIVING");
                Identifier();
                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
        }

        void MULTIPLY()
        {
            bool isConditional = false;

            Expected("MULTIPLY");
            switch (Current().type)
            {
                case TokenType.Identifier:
                    Identifier();
                    break;

                case TokenType.Numeric:
                    Number();
                    break;

                default:
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                    break;
            }

            if (CurrentEquals("BY") && LookaheadEquals(2, "GIVING"))
            {
                Optional("BY");
                switch (Current().type)
                {
                    case TokenType.Identifier:
                        Identifier();
                        break;

                    case TokenType.Numeric:
                        Number();
                        break;

                    default:
                        ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                        ErrorHandler.Parser.PrettyError(FileName, Current());
                        break;
                }

                Expected("GIVING");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier");
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else if (CurrentEquals("BY"))
            {
                Expected("BY");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier");
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "BY");
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            SizeError(ref isConditional);

            if (isConditional)
                Expected("END-MULTIPLY");
        }

        void MOVE()
        {
            Expected("MOVE");
            if (CurrentEquals("CORRESPONDING") || CurrentEquals("CORR"))
            {
                Expected(Current().value);
                Identifier();
                Expected("TO");
                Identifier();
                return;
            }

            if (NotIdentifierOrLiteral())
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The MOVE statement must only contain a single data item identifier, datatype literal or an intrisic function which returns a data item before the "TO" reserved word.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            if (Current().type == TokenType.Identifier)
                Identifier();

            else if (Current().type == TokenType.Numeric)
                Number();

            else if (Current().type == TokenType.String)
                String();

            Expected("TO");
            if (Current().type != TokenType.Identifier)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The MOVE statement must only contain data item identifiers after the "TO" reserved word.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            while (Current().type == TokenType.Identifier)
                Identifier();

            if (!CurrentEquals(".") && !CurrentEquals(TokenType.ReservedKeyword) && !CurrentEquals(TokenContext.IsStatement))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The MOVE statement must only contain data item identifiers after the "TO" reserved word.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
        }

        void OPEN()
        {
            Expected("OPEN");
            Choice("INPUT", "OUTPUT", "I-O", "EXTEND");

            if (CurrentEquals("SHARING"))
            {
                Expected("SHARING");
                Optional("WITH");
                if (CurrentEquals("ALL"))
                {
                    Expected("ALL");
                    Optional("OTHER");
                }
                else if (CurrentEquals("NO"))
                {
                    Expected("NO");
                    Optional("OTHER");
                }
                else if (CurrentEquals("READ"))
                {
                    Expected("READ");
                    Expected("ONLY");
                }
                else
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    Expected ALL OTHER, NO OTHER or READ ONLY. One of them must be specified in the SHARING clause
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
            }

            if (CurrentEquals("RETRY"))
            {
                RetryPhrase();
            }

            Identifier();
            if (CurrentEquals("WITH", "NO"))
            {
                Optional("WITH");
                Expected("NO");
                Expected("REWIND");
            }

            while (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
                if (CurrentEquals("WITH", "NO"))
                {
                    Optional("WITH");
                    Expected("NO");
                    Expected("REWIND");
                }
            }

            while (CurrentEquals("INPUT", "OUTPUT", "I-O", "EXTEND"))
            {
                Choice("INPUT", "OUTPUT", "I-O", "EXTEND");

                if (CurrentEquals("SHARING"))
                {
                    Expected("SHARING");
                    Optional("WITH");
                    if (CurrentEquals("ALL"))
                    {
                        Expected("ALL");
                        Optional("OTHER");
                    }
                    else if (CurrentEquals("NO"))
                    {
                        Expected("NO");
                        Optional("OTHER");
                    }
                    else if (CurrentEquals("READ"))
                    {
                        Expected("READ");
                        Expected("ONLY");
                    }
                    else
                    {
                        ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                        Expected ALL OTHER, NO OTHER or READ ONLY. One of them must be specified in the SHARING clause
                        """);
                        ErrorHandler.Parser.PrettyError(FileName, Current());
                    }
                }

                if (CurrentEquals("RETRY"))
                {
                    RetryPhrase();
                }

                Identifier();
                if (CurrentEquals("WITH", "NO"))
                {
                    Optional("WITH");
                    Expected("NO");
                    Expected("REWIND");
                }

                while (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                    if (CurrentEquals("WITH", "NO"))
                    {
                        Optional("WITH");
                        Expected("NO");
                        Expected("REWIND");
                    }
                }

            }
        }

        void DIVIDE()
        {
            bool isConditional = false;

            Expected("DIVIDE");
            switch (Current().type)
            {
                case TokenType.Identifier:
                    Identifier();
                    break;

                case TokenType.Numeric:
                    Number();
                    break;

                default:
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                    break;
            }

            if ((CurrentEquals("BY") || CurrentEquals("INTO")) && LookaheadEquals(2, "GIVING") && !LookaheadEquals(4, "REMAINDER"))
            {
                Choice("BY", "INTO");
                switch (Current().type)
                {
                    case TokenType.Identifier:
                        Identifier();
                        break;

                    case TokenType.Numeric:
                        Number();
                        break;

                    default:
                        ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                        ErrorHandler.Parser.PrettyError(FileName, Current());
                        break;
                }

                Expected("GIVING");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier");
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else if ((CurrentEquals("BY") || CurrentEquals("INTO")) && LookaheadEquals(2, "GIVING") && LookaheadEquals(4, "REMAINDER"))
            {
                Choice("BY", "INTO");
                switch (Current().type)
                {
                    case TokenType.Identifier:
                        Identifier();
                        break;

                    case TokenType.Numeric:
                        Number();
                        break;

                    default:
                        ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                        ErrorHandler.Parser.PrettyError(FileName, Current());
                        break;
                }

                Expected("GIVING");
                Identifier();
                Expected("REMAINDER");
                Identifier();
            }
            else if (CurrentEquals("INTO"))
            {
                Expected("INTO");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "identifier");
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, "BY or INTO");
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            SizeError(ref isConditional);

            if (isConditional)
                Expected("END-MULTIPLY");
        }

        void DELETE()
        {
            bool isConditional = false;
            bool isFile = false;

            Expected("DELETE");
            if (CurrentEquals("FILE"))
            {
                isFile = true;
                Expected("FILE");
                Optional("OVERRIDE");
                Identifier();
                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else if (Current().type == TokenType.Identifier)
            {
                Identifier();
                Expected("RECORD");
            }

            if (CurrentEquals("RETRY"))
                RetryPhrase();

            if (!isFile)
                InvalidKey(ref isConditional);

            if (isFile)
                OnException(ref isConditional);

            if (isConditional)
                Expected("END-DELETE");
        }

        void EVALUATE()
        {
            var conditions = new List<EvaluateOperand>();
            var conditionsIndex = 0;
            Expected("EVALUATE");

            conditions.Add(SelectionSubject());
            while (CurrentEquals("ALSO"))
            {
                Expected("ALSO");
                conditions.Add(SelectionSubject());
            }

            Expected("WHEN");
            SelectionObject(conditions[conditionsIndex]);
            conditionsIndex++;

            while (CurrentEquals("ALSO"))
            {
                Expected("ALSO");
                SelectionObject(conditions[conditionsIndex]);
                conditionsIndex++;
            }
            conditionsIndex = 0;

            ParseStatements(true);

            while (CurrentEquals("WHEN") && !LookaheadEquals(1, "OTHER"))
            {
                Expected("WHEN");
                SelectionObject(conditions[conditionsIndex]);
                conditionsIndex++;

                while (CurrentEquals("ALSO"))
                {
                    Expected("ALSO");
                    SelectionObject(conditions[conditionsIndex]);
                    conditionsIndex++;
                }
                conditionsIndex = 0;

                ParseStatements(true);
            }

            if (CurrentEquals("WHEN") && LookaheadEquals(1, "OTHER"))
            {
                Expected("WHEN");
                Expected("OTHER");
                ParseStatements(true);
            }

            Expected("END-EVALUATE");
        }

        void EXIT()
        {
            Expected("EXIT");
            if (CurrentEquals("PERFORM"))
            {
                Expected("PERFORM");
                Optional("CYCLE");
            }
            else if (CurrentEquals("PARAGRAPH"))
                Expected("PARAGRAPH");

            else if (CurrentEquals("SECTION"))
                Expected("SECTION");

            else if (CurrentEquals("PROGRAM"))
            {
                Expected("PROGRAM");
                if (CurrentEquals("RAISING"))
                {
                    Expected("RAISING");
                    if (CurrentEquals("EXCEPTION"))
                    {
                        Expected("EXCEPTION");
                        Identifier();
                    }
                    else if (CurrentEquals("LAST"))
                    {
                        Expected("LAST");
                        Optional("EXCEPTION");
                    }
                    else
                        Identifier();
                }
            }
        }

        void FREE()
        {
            Expected("FREE");
            Identifier();
            while (CurrentEquals(TokenType.Identifier)) Identifier();

            if (!CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The FREE statement must only contain based data item identifiers.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

        }

        void GENERATE()
        {
            Expected("GENERATE");
            Identifier();
        }

        void GO()
        {
            Expected("GO");
            Optional("TO");
            Identifier();
            if (CurrentEquals("DEPENDING") || Current().type == TokenType.Identifier)
            {
                while (CurrentEquals(TokenType.Identifier)) Identifier();

                Expected("DEPENDING");
                Optional("ON");
                Identifier();
            }
        }

        void GOBACK()
        {
            Expected("GOBACK");
            RaisingStatus();
        }

        void COMMIT()
        {
            Expected("COMMIT");
        }

        void CLOSE()
        {
            Expected("CLOSE");
            if (Current().type == TokenType.Identifier)
            {
                Identifier();
                if (CurrentEquals("REEL") || CurrentEquals("UNIT"))
                {
                    Expected(Current().value);

                    if (CurrentEquals("FOR") || CurrentEquals("REMOVAL"))
                    {
                        Optional("FOR");
                        Expected("REMOVAL");
                    }
                }
                else if (CurrentEquals("WITH") || CurrentEquals("NO"))
                {
                    Optional("WITH");
                    Expected("NO");
                    Expected("REWIND");
                }
            }
            else
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The CLOSE statement only accepts file connector names. 
                NOTE: This statement must not specify more than one file connector when inside of an exception-checking phrase in a PERFORM statement.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            while (Current().type == TokenType.Identifier)
            {
                Identifier();
                if (CurrentEquals("REEL", "UNIT"))
                {
                    Expected(Current().value);

                    if (CurrentEquals("FOR", "REMOVAL"))
                    {
                        Optional("FOR");
                        Expected("REMOVAL");
                    }
                }
                else if (CurrentEquals("WITH", "NO"))
                {
                    Optional("WITH");
                    Expected("NO");
                    Expected("REWIND");
                }
            }

            if (!CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The CLOSE statement only accepts file connector names. 
                NOTE: This statement must not specify more than one file connector when inside of an exception-checking phrase in a PERFORM statement.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
        }

        void CANCEL()
        {
            Expected("CANCEL");
            if (Current().type == TokenType.Identifier)
                Identifier();

            else if (Current().type == TokenType.String)
                String();

            else
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The CANCEL statement only accepts Alphanumeric or National literals and data items, or a program prototype name specified in the REPOSITORY paragraph.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
                Continue();
            }

            while (Current().type == TokenType.Identifier || Current().type == TokenType.String)
            {
                if (Current().type == TokenType.Identifier)
                    Identifier();

                if (Current().type == TokenType.String)
                    String();
            }

            if (!CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The CANCEL statement only accepts Alphanumeric or National literals and data items, or a program prototype name specified in the REPOSITORY paragraph.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
        }

        void PERFORM()
        {
            bool isExceptionChecking = false;
            bool isInline = false;

            Expected("PERFORM");
            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
                if (CurrentEquals("THROUGH", "THRU"))
                {
                    Choice("THROUGH", "THRU");
                    Identifier();
                }

                if (CurrentEquals(TokenType.Identifier, TokenType.Numeric))
                {
                    TimesPhrase();
                }
                else if (CurrentEquals("WITH", "TEST", "VARYING", "UNTIL"))
                {
                    WithTest();
                    if (CurrentEquals("VARYING"))
                    {
                        VaryingPhrase();
                    }
                    else if (CurrentEquals("UNTIL"))
                    {
                        UntilPhrase();
                    }
                }
            }
            else
            {
                if (CurrentEquals("LOCATION") || CurrentEquals("WITH") && LookaheadEquals(1, "LOCATION"))
                {
                    isExceptionChecking = true;
                    Optional("WITH");
                    Expected("LOCATION");
                }
                else if (CurrentEquals(TokenType.Identifier, TokenType.Numeric))
                {
                    isInline = true;

                    TimesPhrase();
                }
                else if (CurrentEquals("TEST", "VARYING", "UNTIL") || CurrentEquals("WITH") && LookaheadEquals(1, "TEST"))
                {
                    isInline = true;

                    WithTest();
                    if (CurrentEquals("VARYING"))
                    {
                        VaryingPhrase();
                    }
                    else if (CurrentEquals("UNTIL"))
                    {
                        UntilPhrase();
                    }
                }

                ParseStatements(true);

                if (isInline && CurrentEquals("WHEN"))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Recovery, """
                    An inline PERFORM with a TIMES, VARYING or UNTIL phrase cannot contain an exception checking WHEN phrase 
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current(), ConsoleColor.Blue);

                    AnchorPoint("END-PERFORM");
                }
                
                if (isExceptionChecking || !isInline && CurrentEquals("WHEN"))
                {
                    isExceptionChecking = true;

                    Expected("WHEN");
                    if (CurrentEquals("EXCEPTION") && LookaheadEquals(1, TokenType.Identifier))
                    {
                        Expected("EXCEPTION");

                        Identifier();
                        while (CurrentEquals(TokenType.Identifier))
                            Identifier();

                        ParseStatements(true);
                    }
                    else if (CurrentEquals("EXCEPTION"))
                    {
                        Expected("EXCEPTION");
                        Choice("INPUT", "OUTPUT", "IO", "EXTEND");
                        ParseStatements(true);
                    }
                    else if (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "FILE"))
                    {
                        Identifier();
                        Expected("FILE");
                        Identifier();

                        while (CurrentEquals(TokenType.Identifier))
                            Identifier();
                    }
                    else if (CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, "FILE"))
                    {
                        Identifier();
                        while (CurrentEquals(TokenType.Identifier))
                            Identifier();
                    }

                    while (CurrentEquals("WHEN"))
                    {
                        Expected("WHEN");
                        if (CurrentEquals("EXCEPTION") && LookaheadEquals(1, TokenType.Identifier))
                        {
                            Expected("EXCEPTION");

                            Identifier();
                            while (CurrentEquals(TokenType.Identifier))
                                Identifier();

                            ParseStatements(true);
                        }
                        else if (CurrentEquals("EXCEPTION"))
                        {
                            Expected("EXCEPTION");
                            Choice("INPUT", "OUTPUT", "IO", "EXTEND");
                            ParseStatements(true);
                        }
                        else if (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "FILE"))
                        {
                            Identifier();
                            Expected("FILE");
                            Identifier();

                            while (CurrentEquals(TokenType.Identifier))
                                Identifier();
                        }
                        else if (CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, "FILE"))
                        {
                            Identifier();
                            while (CurrentEquals(TokenType.Identifier))
                                Identifier();
                        }
                    }
                }

                if (isExceptionChecking && CurrentEquals("WHEN") && LookaheadEquals(1, "OTHER"))
                {
                    Expected("WHEN");
                    Expected("OTHER");
                    Optional("EXCEPTION");

                    ParseStatements(true);
                }

                if (isExceptionChecking && CurrentEquals("WHEN", "COMMON"))
                {
                    Optional("WHEN");
                    Expected("COMMON");
                    Optional("EXCEPTION");

                    ParseStatements(true);
                }

                if (isExceptionChecking && CurrentEquals("FINALLY"))
                {
                    Expected("FINALLY");

                    ParseStatements(true);
                }

                Expected("END-PERFORM");
            }
        }

        void RAISE()
        {
            Expected("RAISE");
            if (CurrentEquals("EXCEPTION"))
            {
                Expected("EXCEPTION");
                Identifier();
            }
            else
                Identifier();
        }

        void READ()
        {
            bool isSequential = false;
            bool isConditional = false;

            Expected("READ");
            Identifier();
            if (CurrentEquals("NEXT", "PREVIOUS"))
            {
                Expected(Current().value);
                isSequential = true;
            }

            Expected("RECORD");
            if (CurrentEquals("INTO"))
            {
                Expected("INTO");
                Identifier();
            }

            if (CurrentEquals("ADVANCING"))
            {
                Expected("ADVANCING");
                Optional("ON");
                Expected("LOCK");
                isSequential = true;
            }
            else if (CurrentEquals("IGNORING"))
            {
                Expected("IGNORING");
                Expected("LOCK");
            }
            else if (CurrentEquals("RETRY"))
            {
                RetryPhrase();
            }

            if (CurrentEquals("WITH", "LOCK"))
            {
                Optional("WITH");
                Expected("LOCK");
            }
            else if (CurrentEquals("WITH", "NO"))
            {
                Optional("WITH");
                Expected("NO");
                Expected("LOCK");
            }

            if (!isSequential && CurrentEquals("KEY"))
            {
                Expected("KEY");
                Optional("IS");
                Identifier();
            }

            if (!isSequential && CurrentEquals("INVALID", "NOT"))
            {
                InvalidKey(ref isConditional);
            }
            else if (isSequential && CurrentEquals("AT", "END", "NOT"))
            {
                AtEnd(ref isConditional);
            }

            if (isConditional) Expected("END-READ");
        }

        void RECEIVE()
        {
            bool isConditional = false;

            Expected("RECEIVE");
            Optional("FROM");
            Identifier();
            Expected("GIVING");
            Identifier();
            Identifier();

            if (CurrentEquals("CONTINUE"))
            {
                Expected("CONTINUE");
                Optional("AFTER");
                if (CurrentEquals("MESSAGE"))
                {
                    Expected("MESSAGE");
                    Expected("RECEIVED");
                }
                else
                {
                    Arithmetic("SECONDS");
                    Optional("SECONDS");
                }
            }

            OnException(ref isConditional);

            if (isConditional) Expected("END-RECEIVE");

        }

        void RELEASE()
        {
            Expected("RELEASE");
            Identifier();

            if (CurrentEquals("FROM"))
            {
                Expected("FROM");
                if (Current().type == TokenType.String)
                    String();

                else if (Current().type == TokenType.Numeric)
                    Number();

                else
                    Identifier();
            }
        }

        void RETURN()
        {
            bool isConditional = false;

            Expected("RETURN");
            Identifier();
            Expected("RECORD");
            if (CurrentEquals("INTO"))
            {
                Expected("INTO");
                Identifier();
            }

            AtEnd(ref isConditional);

            if (isConditional)
                Expected("END-RETURN");
        }

        void REWRITE()
        {
            bool isConditional = false;
            bool isFile = false;

            Expected("REWRITE");
            if (CurrentEquals("FILE"))
            {
                isFile = true;
                Expected("FILE");
                Identifier();
            }
            else
                Identifier();

            Expected("RECORD");
            if (CurrentEquals("FROM") || isFile)
            {
                Expected("FROM");

                if (Current().type == TokenType.Identifier)
                    Identifier();

                else if (Current().type == TokenType.Numeric)
                    Number();

                else
                    String();
            }

            RetryPhrase();
            if (CurrentEquals("WITH") || CurrentEquals("LOCK") || CurrentEquals("NO"))
            {
                Optional("WITH");
                if (CurrentEquals("LOCK"))
                {
                    Expected("LOCK");
                }
                else
                {
                    Expected("NO");
                    Expected("LOCK");
                }
            }

            InvalidKey(ref isConditional);

            if (isConditional)
                Expected("END-REWRITE");
        }

        void RESUME()
        {
            Expected("RESUME");
            Optional("AT");
            if (CurrentEquals("NEXT"))
            {
                Expected("NEXT");
                Expected("STATEMENT");
            }
            else
            {
                Identifier();
            }
        }

        void ROLLBACK()
        {
            Expected("ROLLBACK");
        }

        void SEARCH()
        {
            Expected("SEARCH");
            if (!CurrentEquals("ALL"))
            {
                Identifier();
                if (CurrentEquals("VARYING"))
                {
                    Expected("VARYING");
                    Identifier();
                }

                if (CurrentEquals("AT", "END"))
                {
                    Optional("AT");
                    Expected("END");
                    ParseStatements(true);
                }

                if (!CurrentEquals("WHEN"))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The SEARCH statement must contain at least one WHEN condition
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                while (CurrentEquals("WHEN"))
                {
                    Expected("WHEN");
                    Condition();
                    if (CurrentEquals("NEXT") && LookaheadEquals(1, "SENTENCE"))
                    {
                        ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                        Unsupported phrase: NEXT SENTENCE is an archaic feature. This phrase can be confusing and is a common source of errors.
                        The CONTINUE statement can be used to accomplish the same functionality while being much clearer and less prone to error
                        """);
                        ErrorHandler.Parser.PrettyError(FileName, Current());

                        AnchorPoint("WHEN", "END-SEARCH");
                    }

                    ParseStatements(true);
                }

                Expected("END-SEARCH");
                return;
            }

            Expected("ALL");
            Identifier();
            if (CurrentEquals("AT", "END"))
            {
                Optional("AT");
                Expected("END");
                ParseStatements(true);
            }

            Expected("WHEN");
            Identifier();
            if (CurrentEquals("IS", "EQUAL", "="))
            {
                Optional("IS");
                if (CurrentEquals("EQUAL"))
                {
                    Expected("EQUAL");
                    Optional("TO");
                }
                else
                {
                    Expected("=");
                }
            }

            if (CurrentEquals(TokenType.Identifier, TokenType.String, TokenType.Numeric) && LookaheadEquals(1, TokenType.Symbol))
            {
                Arithmetic();
            }
            else if (CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, TokenType.Symbol))
            {
                Identifier();
            }
            else if (CurrentEquals(TokenType.Numeric))
            {
                Number();
            }
            else
            {
                String();
            }

            while (CurrentEquals("AND"))
            {
                Expected("AND");
                Identifier();
                if (CurrentEquals("IS", "EQUAL", "="))
                {
                    Optional("IS");
                    if (CurrentEquals("EQUAL"))
                    {
                        Expected("EQUAL");
                        Optional("TO");
                    }
                    else
                    {
                        Expected("=");
                    }
                }

                if (CurrentEquals(TokenType.Identifier, TokenType.String, TokenType.Numeric) && LookaheadEquals(1, TokenType.Symbol))
                {
                    Arithmetic();
                }
                else if (CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, TokenType.Symbol))
                {
                    Identifier();
                }
                else if (CurrentEquals(TokenType.Numeric))
                {
                    Number();
                }
                else
                {
                    String();
                }

            }

            if (CurrentEquals("NEXT") && LookaheadEquals(1, "SENTENCE"))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                Unsupported phrase: NEXT SENTENCE is an archaic feature. This phrase can be confusing and is a common source of errors.
                The CONTINUE statement can be used to accomplish the same functionality while being much clearer and less prone to error
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());

                AnchorPoint("END-SEARCH");
            }

            ParseStatements(true);
            Expected("END-SEARCH");
        }

        void SEND()
        {
            bool isConditional = false;
            Expected("SEND");
            Optional("TO");

            if (LookaheadEquals(3, "RETURNING"))
            {
                if (CurrentEquals(TokenType.String))
                {
                    String();
                }
                else
                {
                    Identifier();
                }

                Expected("FROM");
                Identifier();
                Expected("RETURNING");
                Identifier();
            }
            else
            {
                Identifier();
                Expected("FROM");
                Identifier();

                if (CurrentEquals("RAISING"))
                {
                    Expected("RAISING");
                    if (CurrentEquals("LAST"))
                    {
                        Expected("LAST");
                        Optional("EXCEPTION");
                    }
                    else
                    {
                        Expected("EXCEPTION");
                        Identifier();
                    }
                }
            }

            OnException(ref isConditional);

            if (isConditional) Expected("END-SEND");
        }

        void SET()
        {
            Expected("SET");

            if (CurrentEquals(TokenType.Identifier) || CurrentEquals("SIZE"))
            {
                string dataItemHash = $"{SourceId}#{Current().value}";
                DataItemInfo dataItem = Information.DataItems.GetValue(dataItemHash);

                if (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "UP", "DOWN", "TO"))
                {
                    Identifier();
                    if (CurrentEquals("UP"))
                    {
                        Expected("UP");
                        Expected("BY");
                    }
                    else if (CurrentEquals("DOWN"))
                    {
                        Expected("DOWN");
                        Expected("BY");
                    }
                    else
                    {
                        Expected("TO");
                    }

                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier(UsageType.Integer);
                    }
                    else
                    {
                        Arithmetic();
                    }
                }
                else if (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "TO") && LookaheadEquals(2, "LOCALE"))
                {
                    Identifier();
                    Expected("TO");
                    Expected("LOCALE");
                    Choice("LC_ALL", "LOCALE");
                }
                else if (dataItem.UsageType == UsageType.MessageTag)
                {
                    Identifier(UsageType.MessageTag);
                    Expected("TO");
                    if (CurrentEquals("NULL"))
                    {
                        Expected("NULL");
                    }
                    else
                    {
                        Identifier(UsageType.MessageTag);
                    }
                }
                else if (dataItem.IsDynamicLength || CurrentEquals("SIZE"))
                {
                    if (CurrentEquals("SIZE"))
                    {
                        Expected("SIZE");
                        Optional("OF");
                    }

                    Identifier();
                    Expected("TO");
                    if (CurrentEquals(TokenType.Numeric))
                    {
                        Number();
                    }
                    else
                    {
                        Arithmetic();
                    }
                }
                else if (dataItem.UsageType == UsageType.DataPointer)
                {
                    Identifier(UsageType.DataPointer);
                    while (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier(UsageType.DataPointer);
                    }

                    Choice("UP", "DOWN");
                    Expected("BY");
                    Arithmetic();
                }
                else if (dataItem.UsageType is UsageType.Integer or UsageType.Index)
                {

                    Identifier();
                    bool checkUsage = true;

                    while (CurrentEquals(TokenType.Identifier))
                        checkUsage = Identifier(true, UsageType.Index, UsageType.Integer);

                    if (CurrentEquals("TO"))
                    {
                        Expected("TO");
                        if (CurrentEquals(TokenType.Identifier))
                        {
                            Identifier(UsageType.Integer, UsageType.Index);
                        }
                        else
                        {
                            Arithmetic();
                        }
                    }
                    else if (checkUsage && CurrentEquals("UP", "DOWN"))
                    {
                        Choice("UP", "DOWN");
                        Expected("BY");
                        Arithmetic();
                    }

                }
            }
            else if (CurrentEquals("LAST"))
            {
                Expected("LAST");
                Expected("EXCEPTION");
                Expected("TO");
                Expected("OFF");
            }
            else if (CurrentEquals("LOCALE"))
            {
                Expected("LOCALE");
                if (CurrentEquals("USER-DEFAULT"))
                {
                    Expected("USER-DEFAULT");
                }
                else
                {
                    SetLocale();
                }

                Expected("TO");
                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    Choice("USER-DEFAULT", "SYSTEM-DEFAULT");
                }
            }

        }

        void SORT()
        {
            Expected("SORT");
            Identifier();

            Optional("ON");
            if (CurrentEquals("ASCENDING"))
            {
                Expected("ASCENDING");
            }
            else
            {
                Expected("DESCENDING");
            }

            Optional("KEY");

            if (!CurrentEquals(TokenType.Identifier))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The ON ASCENDING / DESCENDING KEY clause must only contain key data item names.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            Identifier();
            while (Current().type == TokenType.Identifier)
                Identifier();


            while (CurrentEquals("ON", "ASCENDING", "DESCENDING"))
            {
                Optional("ON");
                if (CurrentEquals("ASCENDING"))
                {
                    Expected("ASCENDING");
                }
                else
                {
                    Expected("DESCENDING");
                }

                Optional("KEY");

                if (!CurrentEquals(TokenType.Identifier))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The ON ASCENDING / DESCENDING KEY clause must only contain key data item names.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                Identifier();
                while (Current().type == TokenType.Identifier)
                    Identifier();
            }

            if (CurrentEquals("WITH", "DUPLICATES"))
            {
                Optional("WITH");
                Expected("DUPLICATES");
                Optional("IN");
                Optional("ORDER");
            }

            if (CurrentEquals("COLLATING", "SEQUENCE"))
            {
                Optional("COLLATING");
                Expected("SEQUENCE");
                if (CurrentEquals("IS") && LookaheadEquals(1, TokenType.Identifier) || CurrentEquals(TokenType.Identifier))
                {
                    Optional("IS");
                    Identifier();

                    if (CurrentEquals(TokenType.Identifier)) Identifier();
                }
                else
                {
                    if (!CurrentEquals("FOR", "ALPHANUMERIC", "NATIONAL"))
                    {
                        ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                        The COLLATING SEQUENCE clause must contain at least 1 alphabet name (max of 2 alphabet names) or at least one FOR ALPHANUMERIC and FOR NATIONAL clauses.
                        """);
                        ErrorHandler.Parser.PrettyError(FileName, Current());

                        CombinedAnchorPoint(TokenContext.IsStatement, "USING");
                    }

                    ForAlphanumericForNational();
                }
            }

            if (CurrentEquals("INPUT"))
            {
                Expected("INPUT");
                Expected("PROCEDURE");
                Optional("IS");
                Identifier();

                if (CurrentEquals("THROUGH", "THRU"))
                {
                    Choice("THROUGH", "THRU");
                    Identifier();
                }
            }
            else
            {
                Expected("USING");
                Identifier();
                while (Current().type == TokenType.Identifier)
                    Identifier();
            }

            if (CurrentEquals("OUTPUT"))
            {
                Expected("OUTPUT");
                Expected("PROCEDURE");
                Optional("IS");
                Identifier();

                if (CurrentEquals("THROUGH", "THRU"))
                {
                    Choice("THROUGH", "THRU");
                    Identifier();
                }
            }
            else
            {
                Expected("GIVING");
                Identifier();
                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
        }

        void START()
        {
            bool isConditional = false;

            Expected("START");
            Identifier();

            if (CurrentEquals("FIRST"))
            {
                Expected("FIRST");
            }
            else if (CurrentEquals("LAST"))
            {
                Expected("LAST");
            }
            else if (CurrentEquals("KEY"))
            {
                Expected("KEY");
                StartRelationalOperator();
                Identifier();

                if (CurrentEquals("WITH", "LENGTH"))
                {
                    Optional("WITH");
                    Expected("LENGTH");
                    Arithmetic(".");
                }

            }

            InvalidKey(ref isConditional);

            if (isConditional) Expected("END-START");
        }

        void STOP()
        {
            Expected("STOP");
            Expected("RUN");
            if (CurrentEquals("WITH") || CurrentEquals("NORMAL") || CurrentEquals("ERROR"))
            {
                Optional("WITH");
                Choice("NORMAL", "ERROR");
                Optional("STATUS");
                switch (Current().type)
                {
                    case TokenType.Identifier:
                        Identifier();
                        break;
                    case TokenType.Numeric:
                        Number();
                        break;
                    case TokenType.String:
                        String();
                        break;
                }
            }
        }

        void STRING()
        {
            bool isConditional = false;

            Expected("STRING");
            if (CurrentEquals(TokenType.Identifier)) Identifier();

            else String();

            while (CurrentEquals(TokenType.Identifier, TokenType.String))
            {
                if (CurrentEquals(TokenType.Identifier)) Identifier();

                else String();
            }

            Expected("DELIMITED");
            Optional("BY");
            if (CurrentEquals(TokenType.Identifier)) Identifier();

            else if (CurrentEquals("SIZE")) Expected("SIZE");

            else String();

            while (CurrentEquals(TokenType.Identifier, TokenType.String))
            {
                if (CurrentEquals(TokenType.Identifier)) Identifier();

                else String();

                while (CurrentEquals(TokenType.Identifier, TokenType.String))
                {
                    if (CurrentEquals(TokenType.Identifier)) Identifier();

                    else String();
                }

                Expected("DELIMITED");
                Optional("BY");
                if (CurrentEquals(TokenType.Identifier)) Identifier();

                else if (CurrentEquals("SIZE")) Expected("SIZE");

                else String();
            }

            Expected("INTO");
            Identifier();

            if (CurrentEquals("WITH", "POINTER"))
            {
                Optional("WITH");
                Expected("POINTER");
                Identifier();
            }

            OnOverflow(ref isConditional);

            if (isConditional) Expected("END-STRING");
        }

        void SUPPRESS()
        {
            Expected("SUPPRESS");
            Optional("PRINTING");
        }

        void TERMINATE()
        {
            Expected("TERMINATE");
            if (Current().type != TokenType.Identifier)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The TERMINATE statement must only contain report entry identifiers defined in the report section.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            Identifier();
            while (Current().type == TokenType.Identifier)
                Identifier();

            if (!CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The TERMINATE statement must only contain report entry identifiers defined in the report section.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
        }

        void UNLOCK()
        {
            Expected("UNLOCK");
            Identifier();
            Choice("RECORD", "RECORDS");
        }

        void UNSTRING()
        {
            bool isConditional = false;

            Expected("UNSTRING");
            Identifier();

            if (CurrentEquals("DELIMITED"))
            {
                Expected("DELIMITED");
                Optional("BY");
                if (CurrentEquals("ALL")) Expected("ALL");

                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    String();
                }

                while (CurrentEquals("OR"))
                {
                    Expected("OR");
                    if (CurrentEquals("ALL")) Expected("ALL");

                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else
                    {
                        String();
                    }
                }
            }

            Expected("INTO");
            Identifier();

            if (CurrentEquals("DELIMITER"))
            {
                Expected("DELIMITER");
                Optional("IN");
                Identifier();
            }
            if (CurrentEquals("COUNT"))
            {
                Expected("COUNT");
                Optional("IN");
                Identifier();
            }

            while (CurrentEquals(TokenType.Identifier))
            {
                Identifier();

                if (CurrentEquals("DELIMITER"))
                {
                    Expected("DELIMITER");
                    Optional("IN");
                    Identifier();
                }
                if (CurrentEquals("COUNT"))
                {
                    Expected("COUNT");
                    Optional("IN");
                    Identifier();
                }
            }

            if (CurrentEquals("WITH", "POINTER"))
            {
                Optional("WITH");
                Expected("POINTER");
                Identifier();
            }

            if (CurrentEquals("TALLYING"))
            {
                Expected("TALLYING");
                Optional("IN");
                Identifier();
            }

            OnOverflow(ref isConditional);

            if (isConditional) Expected("END-UNSTRING");
        }

        void VALIDATE()
        {
            Expected("VALIDATE");
            if (Current().type != TokenType.Identifier)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The VALIDATE statement must only contain data item identifiers.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            Identifier();
            while (Current().type == TokenType.Identifier)
                Identifier();

            if (!CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The VALIDATE statement must only contain data item identifiers.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
        }

        void WRITE()
        {
            bool isSequential = false;
            bool isConditional = false;

            Expected("WRITE");
            if (CurrentEquals("FILE"))
            {
                Expected("FILE");
                Identifier();
            }
            else
            {
                Identifier();
            }

            if (CurrentEquals("FROM"))
            {
                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    String();
                }
            }

            if (CurrentEquals("BEFORE", "AFTER"))
            {
                isSequential = true;

                WriteBeforeAfter();
                Optional("ADVANCING");
                if (CurrentEquals("PAGE"))
                {
                    Expected("PAGE");
                    // Missing mnemonic-name handling
                }
                else if (CurrentEquals(TokenType.Identifier, TokenType.Numeric))
                {
                    if (CurrentEquals(TokenType.Identifier)) Identifier();

                    else Number();

                    if (CurrentEquals("LINE", "LINES"))
                    {
                        Expected(Current().value);
                    }
                }
            }

            RetryPhrase();

            if (CurrentEquals("WITH", "LOCK"))
            {
                Optional("WITH");
                Expected("LOCK");
            }
            else if (CurrentEquals("WITH", "NO"))
            {
                Optional("WITH");
                Expected("NO");
                Expected("LOCK");
            }

            if (isSequential && CurrentEquals("AT", "NOT", "END-OF-PAGE", "EOP"))
            {
                AtEndOfPage(ref isConditional);
            }
            else if (!isSequential && CurrentEquals("INVALID", "NOT"))
            {
                InvalidKey(ref isConditional);
            }

            if (isConditional) Expected("END-WRITE");
        }

        void PARAGRAPH()
        {
            Identifier(Current().value);
        }

        // The following methods are responsible for parsing some commonly repeated pieces of COBOL statements.
        // The ON SIZE ERROR, ON EXCEPTION, INVALID KEY, AT END, and the RETRY phrase are examples of pieces of COBOL syntax
        // that appear on multiple statements. Reusing the same code in those cases keeps things much more modular and easier to maintain.
        //
        // The Arithmetic() and Condition() methods are responsible for parsing expressions and verifying if those expressions were
        // written correctly. This is using a combination of the Shunting Yard algorithm, and some methods to verify if the 
        // parentheses are balanced and if it can be evaluated correctly.
        void TimesPhrase()
        {
            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }
            else
            {
                Number();
            }

            Expected("TIMES");
        }

        void UntilPhrase()
        {
            Expected("UNTIL");
            if (CurrentEquals("EXIT"))
            {
                Expected("EXIT");
            }
            else
            {
                Condition();
            }
        }

        void VaryingPhrase()
        {
            Expected("VARYING");
            Identifier();
            Expected("FROM");
            if (CurrentEquals(TokenType.Numeric))
            {
                Number();
            }
            else
            {
                Identifier();
            }

            if (CurrentEquals("BY"))
            {
                Expected("BY");
                if (CurrentEquals(TokenType.Numeric))
                {
                    Number();
                }
                else
                {
                    Identifier();
                }
            }

            Expected("UNTIL");
            Condition("AFTER");

            while (CurrentEquals("AFTER"))
            {
                Expected("AFTER");
                Identifier();
                Expected("FROM");
                if (CurrentEquals(TokenType.Numeric))
                {
                    Number();
                }
                else
                {
                    Identifier();
                }

                if (CurrentEquals("BY"))
                {
                    Expected("BY");
                    if (CurrentEquals(TokenType.Numeric))
                    {
                        Number();
                    }
                    else
                    {
                        Identifier();
                    }
                }

                Expected("UNTIL");
                Condition("AFTER");
            }
        }

        void WithTest()
        {
            if (CurrentEquals("WITH", "TEST"))
            {
                Optional("WITH");
                Expected("TEST");
                Choice("BEFORE", "AFTER");
            }
        }

        void RetryPhrase()
        {
            var hasFor = false;

            Expected("RETRY");
            if (CurrentEquals("FOREVER"))
            {
                Expected("FOREVER");
                return;
            }

            if (CurrentEquals("FOR"))
            {
                Optional("FOR");
                hasFor = true;
            }

            Arithmetic("SECONDS", "TIMES");
            if (CurrentEquals("SECONDS") || hasFor)
            {
                Expected("SECONDS");
            }
            else
            {
                Expected("TIMES");
            }
        }

        void TallyingPhrase()
        {
            if (!CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, "FOR"))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The tallying phrase must start with a data item identifier, which must be followed by the FOR keyword
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            while (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "FOR"))
            {
                Identifier();
                Expected("FOR");

                if (!CurrentEquals("CHARACTERS", "ALL", "LEADING"))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The tallying phrase must contain at least one of the following clauses: CHARACTERS, ALL or LEADING
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                while (CurrentEquals("CHARACTERS", "ALL", "LEADING"))
                {
                    if (CurrentEquals("CHARACTERS"))
                    {
                        Expected("CHARACTERS");
                        if (CurrentEquals("AFTER", "BEFORE"))
                        {
                            AfterBeforePhrase();
                        }
                    }
                    else if (CurrentEquals("ALL"))
                    {
                        Expected("ALL");
                        if (CurrentEquals(TokenType.Identifier))
                        {
                            Identifier();
                        }
                        else
                        {
                            String();
                        }

                        if (CurrentEquals("AFTER", "BEFORE"))
                        {
                            AfterBeforePhrase();
                        }

                        while (CurrentEquals(TokenType.Identifier, TokenType.String))
                        {
                            if (CurrentEquals(TokenType.Identifier))
                            {
                                Identifier();
                            }
                            else
                            {
                                String();
                            }

                            if (CurrentEquals("AFTER", "BEFORE"))
                            {
                                AfterBeforePhrase();
                            }
                        }
                    }
                    else if (CurrentEquals("LEADING"))
                    {
                        Expected("LEADING");
                        if (CurrentEquals(TokenType.Identifier))
                        {
                            Identifier();
                        }
                        else
                        {
                            String();
                        }

                        if (CurrentEquals("AFTER", "BEFORE"))
                        {
                            AfterBeforePhrase();
                        }

                        while (CurrentEquals(TokenType.Identifier, TokenType.String))
                        {
                            if (CurrentEquals(TokenType.Identifier))
                            {
                                Identifier();
                            }
                            else
                            {
                                String();
                            }

                            if (CurrentEquals("AFTER", "BEFORE"))
                            {
                                AfterBeforePhrase();
                            }
                        }
                    }
                }
            }
        }

        void ReplacingPhrase()
        {
            if (!CurrentEquals("CHARACTERS", "ALL", "LEADING", "FIRST"))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The replacing phrase must contain at least one of the following clauses: CHARACTERS, ALL, LEADING or FIRST
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            while (CurrentEquals("CHARACTERS", "ALL", "LEADING", "FIRST"))
            {
                if (CurrentEquals("CHARACTERS"))
                {
                    Expected("CHARACTERS");
                    Expected("BY");

                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else
                    {
                        String();
                    }

                    if (CurrentEquals("AFTER", "BEFORE"))
                    {
                        AfterBeforePhrase();
                    }
                }
                else if (CurrentEquals("ALL"))
                {
                    Expected("ALL");
                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else
                    {
                        String();
                    }

                    Expected("BY");
                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else
                    {
                        String();
                    }

                    if (CurrentEquals("AFTER", "BEFORE"))
                    {
                        AfterBeforePhrase();
                    }

                    while (CurrentEquals(TokenType.Identifier, TokenType.String))
                    {
                        if (CurrentEquals(TokenType.Identifier))
                        {
                            Identifier();
                        }
                        else
                        {
                            String();
                        }

                        Expected("BY");
                        if (CurrentEquals(TokenType.Identifier))
                        {
                            Identifier();
                        }
                        else
                        {
                            String();
                        }

                        if (CurrentEquals("AFTER", "BEFORE"))
                        {
                            AfterBeforePhrase();
                        }

                        if (CurrentEquals("AFTER", "BEFORE"))
                        {
                            AfterBeforePhrase();
                        }
                    }
                }
                else if (CurrentEquals("LEADING"))
                {
                    Expected("LEADING");
                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else
                    {
                        String();
                    }

                    Expected("BY");
                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else
                    {
                        String();
                    }

                    if (CurrentEquals("AFTER", "BEFORE"))
                    {
                        AfterBeforePhrase();
                    }

                    if (CurrentEquals("AFTER", "BEFORE"))
                    {
                        AfterBeforePhrase();
                    }

                    while (CurrentEquals(TokenType.Identifier, TokenType.String))
                    {
                        if (CurrentEquals(TokenType.Identifier))
                        {
                            Identifier();
                        }
                        else
                        {
                            String();
                        }

                        Expected("BY");
                        if (CurrentEquals(TokenType.Identifier))
                        {
                            Identifier();
                        }
                        else
                        {
                            String();
                        }

                        if (CurrentEquals("AFTER", "BEFORE"))
                        {
                            AfterBeforePhrase();
                        }

                        if (CurrentEquals("AFTER", "BEFORE"))
                        {
                            AfterBeforePhrase();
                        }
                    }
                }
                else if (CurrentEquals("FIRST"))
                {
                    Expected("FIRST");
                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else
                    {
                        String();
                    }

                    Expected("BY");
                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else
                    {
                        String();
                    }

                    if (CurrentEquals("AFTER", "BEFORE"))
                    {
                        AfterBeforePhrase();
                    }

                    if (CurrentEquals("AFTER", "BEFORE"))
                    {
                        AfterBeforePhrase();
                    }

                    while (CurrentEquals(TokenType.Identifier, TokenType.String))
                    {
                        if (CurrentEquals(TokenType.Identifier))
                        {
                            Identifier();
                        }
                        else
                        {
                            String();
                        }

                        Expected("BY");
                        if (CurrentEquals(TokenType.Identifier))
                        {
                            Identifier();
                        }
                        else
                        {
                            String();
                        }

                        if (CurrentEquals("AFTER", "BEFORE"))
                        {
                            AfterBeforePhrase();
                        }

                        if (CurrentEquals("AFTER", "BEFORE"))
                        {
                            AfterBeforePhrase();
                        }
                    }
                }
            }
        }

        void AfterBeforePhrase(bool beforeExists = false, bool afterExists = false)
        {
            if (CurrentEquals("AFTER"))
            {
                if (afterExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    AFTER can only be specified once in this part of the statement. 
                    The same applies to BEFORE.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                afterExists = true;
                Expected("AFTER");
                Optional("INITIAL");

                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    String();
                }

                AfterBeforePhrase(beforeExists, afterExists);

            }

            if (CurrentEquals("BEFORE"))
            {
                if (beforeExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    BEFORE can only be specified once in this part of the statement. 
                    The same applies to AFTER.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                beforeExists = true;
                Expected("BEFORE");
                Optional("INITIAL");

                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    String();
                }

                AfterBeforePhrase(beforeExists, afterExists);
            }
        }

        void InvalidKey(ref bool isConditional, bool invalidKeyExists = false, bool notInvalidKeyExists = false)
        {
            if (CurrentEquals("INVALID"))
            {
                if (invalidKeyExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    INVALID KEY can only be specified once in this statement. 
                    The same applies to the NOT INVALID KEY.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                isConditional = true;
                invalidKeyExists = true;
                Expected("INVALID");
                Optional("KEY");
                ParseStatements(true);
                InvalidKey(ref isConditional, invalidKeyExists, notInvalidKeyExists);

            }

            if (CurrentEquals("NOT"))
            {
                if (notInvalidKeyExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    NOT INVALID KEY can only be specified once in this statement. 
                    The same applies to the INVALID KEY.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                isConditional = true;
                notInvalidKeyExists = true;
                Expected("NOT");
                Expected("INVALID");
                Optional("KEY");
                ParseStatements(true);
                InvalidKey(ref isConditional, invalidKeyExists, notInvalidKeyExists);
            }
        }

        void OnException(ref bool isConditional, bool onExceptionExists = false, bool notOnExceptionExists = false)
        {
            if (CurrentEquals("ON") || CurrentEquals("EXCEPTION"))
            {
                if (onExceptionExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    ON EXCEPTION can only be specified once in this statement. 
                    The same applies to the NOT ON EXCEPTION.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                isConditional = true;
                onExceptionExists = true;
                Optional("ON");
                Expected("EXCEPTION");
                ParseStatements(true);
                OnException(ref isConditional, onExceptionExists, notOnExceptionExists);

            }

            if (CurrentEquals("NOT"))
            {
                if (notOnExceptionExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    NOT ON EXCEPTION can only be specified once in this statement. 
                    The same applies to the ON EXCEPTION.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                isConditional = true;
                notOnExceptionExists = true;
                Expected("NOT");
                Optional("ON");
                Expected("EXCEPTION");
                ParseStatements(true);
                OnException(ref isConditional, onExceptionExists, notOnExceptionExists);
            }
        }

        void RaisingStatus(bool raisingExists = false, bool statusExists = false)
        {
            if (CurrentEquals("RAISING"))
            {
                if (raisingExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    RAISING can only be specified once in this statement. 
                    The same applies to the WITH NORMAL/ERROR STATUS.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                Expected("RAISING");
                if (CurrentEquals("EXCEPTION"))
                {
                    Expected("EXCEPTION");
                    Identifier();
                }
                else if (CurrentEquals("LAST"))
                {
                    Expected("LAST");
                    Optional("EXCEPTION");
                }
                else
                    Identifier();

                raisingExists = true;
                RaisingStatus(raisingExists, statusExists);

            }

            if (CurrentEquals("WITH") || CurrentEquals("NORMAL") || CurrentEquals("ERROR"))
            {
                if (statusExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    WITH NORMAL/ERROR STATUS can only be specified once in this statement. 
                    The same applies to the RAISING.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                Optional("WITH");
                Choice("NORMAL", "ERROR");
                Optional("STATUS");
                switch (Current().type)
                {
                    case TokenType.Identifier:
                        Identifier();
                        break;
                    case TokenType.Numeric:
                        Number();
                        break;
                    case TokenType.String:
                        String();
                        break;
                }

                statusExists = true;
                RaisingStatus(raisingExists, statusExists);
            }
        }

        void AtEnd(ref bool isConditional, bool atEndExists = false, bool notAtEndExists = false)
        {
            if (CurrentEquals("AT") || CurrentEquals("END"))
            {
                if (atEndExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    AT END can only be specified once in this statement. 
                    The same applies to the NOT AT END.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                isConditional = true;
                atEndExists = true;
                Optional("AT");
                Expected("END");
                ParseStatements(true);
                AtEnd(ref isConditional, atEndExists, notAtEndExists);

            }

            if (CurrentEquals("NOT"))
            {
                if (notAtEndExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    NOT AT END can only be specified once in this statement. 
                    The same applies to the AT END.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                isConditional = true;
                notAtEndExists = true;
                Expected("NOT");
                Optional("AT");
                Expected("END");
                ParseStatements(true);
                AtEnd(ref isConditional, atEndExists, notAtEndExists);
            }
        }

        void SizeError(ref bool isConditional, bool onErrorExists = false, bool notOnErrorExists = false)
        {
            if (CurrentEquals("ON") || CurrentEquals("SIZE"))
            {
                if (onErrorExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    ON SIZE ERROR can only be specified once in this statement. 
                    The same applies to NOT ON SIZE ERROR.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                isConditional = true;
                onErrorExists = true;
                Optional("ON");
                Expected("SIZE");
                Expected("ERROR");
                ParseStatements(true);
                SizeError(ref isConditional, onErrorExists, notOnErrorExists);

            }

            if (CurrentEquals("NOT"))
            {
                if (notOnErrorExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    NOT ON SIZE ERROR can only be specified once in this statement. 
                    The same applies to ON SIZE ERROR.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                isConditional = true;
                notOnErrorExists = true;
                Expected("NOT");
                Optional("ON");
                Expected("SIZE");
                Expected("ERROR");
                ParseStatements(true);
                SizeError(ref isConditional, onErrorExists, notOnErrorExists);
            }
        }

        void OnOverflow(ref bool isConditional, bool onOverflowExists = false, bool notOnOverflowExists = false)
        {
            if (CurrentEquals("ON") || CurrentEquals("OVERFLOW"))
            {
                if (onOverflowExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    ON OVERFLOW can only be specified once in this statement. 
                    The same applies to NOT ON OVERFLOW.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                isConditional = true;
                onOverflowExists = true;
                Optional("ON");
                Expected("OVERFLOW");
                ParseStatements(true);
                OnOverflow(ref isConditional, onOverflowExists, notOnOverflowExists);

            }

            if (CurrentEquals("NOT"))
            {
                if (notOnOverflowExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    NOT ON OVERFLOW can only be specified once in this statement. 
                    The same applies to ON OVERFLOW.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                isConditional = true;
                notOnOverflowExists = true;
                Expected("NOT");
                Optional("ON");
                Expected("OVERFLOW");
                ParseStatements(true);
                OnOverflow(ref isConditional, onOverflowExists, notOnOverflowExists);
            }
        }

        void WriteBeforeAfter(bool beforeExists = false, bool afterExists = false)
        {
            if (CurrentEquals("BEFORE"))
            {
                if (beforeExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    BEFORE can only be specified once in this statement. 
                    The same applies to AFTER.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                beforeExists = true;
                Expected("BEFORE");

                WriteBeforeAfter(beforeExists, afterExists);

            }

            if (CurrentEquals("AFTER"))
            {
                if (afterExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    AFTER can only be specified once in this statement. 
                    The same applies to BEFORE.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                afterExists = true;
                Expected("AFTER");

                WriteBeforeAfter(beforeExists, afterExists);
            }
        }

        void SetLocale(SetLcValues locales = new())
        {
            if (CurrentEquals("LC_ALL"))
            {
                if (locales.LC_ALL)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    LC_ALL can only be specified once in this statement. 
                    The same applies to each of the other locale names.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                locales.LC_ALL = true;
                Expected("LC_ALL");

                SetLocale(locales);

            }

            if (CurrentEquals("LC_COLLATE"))
            {
                if (locales.LC_COLLATE)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    LC_COLLATE can only be specified once in this statement. 
                    The same applies to each of the other locale names.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                locales.LC_COLLATE = true;
                Expected("LC_COLLATE");

                SetLocale(locales);

            }

            if (CurrentEquals("LC_CTYPE"))
            {
                if (locales.LC_CTYPE)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    LC_CTYPE can only be specified once in this statement. 
                    The same applies to each of the other locale names.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                locales.LC_CTYPE = true;
                Expected("LC_CTYPE");

                SetLocale(locales);

            }

            if (CurrentEquals("LC_MESSAGES"))
            {
                if (locales.LC_MESSAGES)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    LC_MESSAGES can only be specified once in this statement. 
                    The same applies to each of the other locale names.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                locales.LC_MESSAGES = true;
                Expected("LC_MESSAGES");

                SetLocale(locales);

            }

            if (CurrentEquals("LC_MONETARY"))
            {
                if (locales.LC_MONETARY)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    LC_MONETARY can only be specified once in this statement. 
                    The same applies to each of the other locale names.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                locales.LC_MONETARY = true;
                Expected("LC_MONETARY");

                SetLocale(locales);

            }

            if (CurrentEquals("LC_NUMERIC"))
            {
                if (locales.LC_NUMERIC)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    LC_NUMERIC can only be specified once in this statement. 
                    The same applies to each of the other locale names.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                locales.LC_NUMERIC = true;
                Expected("LC_NUMERIC");

                SetLocale(locales);

            }

            if (CurrentEquals("LC_TIME"))
            {
                if (locales.LC_TIME)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    LC_TIME can only be specified once in this statement. 
                    The same applies to each of the other locale names.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                locales.LC_TIME = true;
                Expected("LC_TIME");

                SetLocale(locales);

            }
        }

        void AtEndOfPage(ref bool isConditional, bool atEndOfPageExists = false, bool notAtEndOfPageExists = false)
        {
            if (CurrentEquals("AT", "END-OF-PAGE", "EOP"))
            {
                if (atEndOfPageExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    AT END-OF-PAGE can only be specified once in this statement. 
                    The same applies to NOT AT END-OF-PAGE.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                isConditional = true;
                atEndOfPageExists = true;
                Optional("AT");
                Choice("END-OF-PAGE", "EOP");
                ParseStatements(true);
                AtEndOfPage(ref isConditional, atEndOfPageExists, notAtEndOfPageExists);

            }

            if (CurrentEquals("NOT"))
            {
                if (notAtEndOfPageExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    NOT AT END-OF-PAGE can only be specified once in this statement. 
                    The same applies to AT END-OF-PAGE.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                isConditional = true;
                notAtEndOfPageExists = true;
                Expected("NOT");
                Optional("AT");
                Choice("END-OF-PAGE", "EOP");
                ParseStatements(true);
                AtEndOfPage(ref isConditional, atEndOfPageExists, notAtEndOfPageExists);
            }
        }

        void ForAlphanumericForNational(bool forAlphanumericExists = false, bool forNationalExists = false)
        {
            if (CurrentEquals("FOR") && LookaheadEquals(1, "ALPHANUMERIC") || CurrentEquals("ALPHANUMERIC"))
            {
                if (forAlphanumericExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    FOR ALPHANUMERIC can only be specified once in this statement. 
                    The same applies to FOR NATIONAL.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                forAlphanumericExists = true;
                Optional("FOR");
                Expected("ALPHANUMERIC");
                Optional("IS");
                Identifier();

                ForAlphanumericForNational(forAlphanumericExists, forNationalExists);

            }

            if (CurrentEquals("FOR") && LookaheadEquals(1, "NATIONAL") || CurrentEquals("NATIONAL"))
            {
                if (forNationalExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    FOR NATIONAL can only be specified once in this statement. 
                    The same applies to FOR ALPHANUMERIC.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                forNationalExists = true;
                Optional("FOR");
                Expected("NATIONAL");
                Optional("IS");
                Identifier();

                ForAlphanumericForNational(forAlphanumericExists, forNationalExists);
            }
        }

        void LineColumn(bool lineExists = false, bool columnExists = false)
        {
            if (CurrentEquals("LINE"))
            {
                if (lineExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    LINE NUMBER can only be specified once in this statement. 
                    The same applies to the COLUMN NUMBER.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                lineExists = true;
                Expected("LINE");
                Optional("NUMBER");
                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    Number();
                }


                LineColumn(lineExists, columnExists);

            }

            if (CurrentEquals("COLUMN", "COL"))
            {
                if (columnExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    COLUMN NUMBER can only be specified once in this statement. 
                    The same applies to the LINE NUMBER.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                columnExists = true;
                Expected(Current().value);
                Optional("NUMBER");
                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    Number();
                }

                LineColumn(lineExists, columnExists);
            }
        }

        void Arithmetic(params string[] delimiter)
        {
            bool IsArithmeticSymbol(Token current) => Helpers.ArithmeticPrecedence.ContainsKey(current.value);
            var expression = new List<Token>();

            while (!CurrentEquals(TokenType.ReservedKeyword) && !CurrentEquals(delimiter))
            {
                if (CurrentEquals(TokenType.Identifier, TokenType.Numeric))
                {
                    expression.Add(Current());
                    Continue();
                }

                if (IsArithmeticSymbol(Current()))
                {
                    expression.Add(Current());
                    Continue();
                }

                if (CurrentEquals(TokenType.Symbol) && !CurrentEquals(".") && !IsArithmeticSymbol(Current()))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    Invalid symbol in this arithmetic expression. Valid operators are: +, -, *, /, **, ( and )
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
            }

            if (!Helpers.IsBalanced(expression))
            {
                ErrorHandler.Parser.Report(FileName, expression[0], ErrorType.General, """
                This expression is not balanced, one or more parenthesis to not have their matching opening or closing pair, it is an invalid expression
                """);
                ErrorHandler.Parser.PrettyError(FileName, expression[0]);
            }

            var shuntingYard = Helpers.ShuntingYard(expression, Helpers.ArithmeticPrecedence);

            if (!Helpers.EvaluatePostfix(shuntingYard, Helpers.ArithmeticPrecedence, out Token error))
            {
                ErrorHandler.Parser.Report(FileName, error, ErrorType.General, """
                This expression cannot be correctly evaluated. Please make sure that all operators have their matching operands.
                """);
                ErrorHandler.Parser.PrettyError(FileName, error);
            }
        }

        void Condition(params string[] delimiter)
        {
            var expression = new List<Token>();

            while (!CurrentEquals(TokenContext.IsStatement) && !CurrentEquals(delimiter))
            {
                if (CurrentEquals("IS") && (Lookahead(1).value is "GREATER" or "LESS" or "EQUAL" or "NOT" || Lookahead(1).type is TokenType.Symbol))
                {
                    Continue();
                }
                else if (CurrentEquals("NOT") && (LookaheadEquals(1, ">") || LookaheadEquals(1, "<")))
                {
                    var combined = new Token($"NOT {Lookahead(1).value}", TokenType.Symbol, Current().line, Current().column);
                    expression.Add(combined);
                    Continue(2);
                }
                else if (CurrentEquals("NOT") && (LookaheadEquals(1, "GREATER") || LookaheadEquals(1, "LESS") || LookaheadEquals(1, "EQUAL")))
                {
                    var combined = new Token();
                    if (LookaheadEquals(1, "GREATER"))
                        combined = new Token($"NOT >", TokenType.Symbol, Current().line, Current().column);

                    if (LookaheadEquals(1, "LESS"))
                        combined = new Token($"NOT <", TokenType.Symbol, Current().line, Current().column);

                    if (LookaheadEquals(1, "EQUAL"))
                        combined = new Token($"<>", TokenType.Symbol, Current().line, Current().column);

                    expression.Add(combined);
                    Continue(2);

                    if (Current().value is "THAN" or "TO") Continue();
                }
                else if (CurrentEquals("GREATER") || CurrentEquals("LESS") || CurrentEquals("EQUAL"))
                {
                    var converted = new Token();
                    if (CurrentEquals("GREATER"))
                        converted = new Token($">", TokenType.Symbol, Current().line, Current().column);

                    if (CurrentEquals("LESS"))
                        converted = new Token($"<", TokenType.Symbol, Current().line, Current().column);

                    if (CurrentEquals("EQUAL"))
                        converted = new Token($"=", TokenType.Symbol, Current().line, Current().column);

                    if (CurrentEquals("GREATER") && (LookaheadEquals(1, "OR") || LookaheadEquals(2, "OR")))
                    {
                        if (!LookaheadEquals(1, "THAN")) Continue(2);

                        if (LookaheadEquals(1, "THAN")) Continue(3);

                        converted = new Token($">=", TokenType.Symbol, Current().line, Current().column);
                    }

                    if (CurrentEquals("LESS") && (LookaheadEquals(1, "OR") || LookaheadEquals(2, "OR")))
                    {
                        if (LookaheadEquals(1, "THAN")) Continue(3);

                        if (!LookaheadEquals(1, "THAN")) Continue(2);

                        converted = new Token($"<=", TokenType.Symbol, Current().line, Current().column);
                    }

                    expression.Add(converted);
                    Continue();

                    if (Current().value is "THAN" or "TO") Continue();
                }
                else
                {
                    expression.Add(Current());
                    Expected(Current().value);
                }
            }

            if (!Helpers.IsBalanced(expression))
            {
                ErrorHandler.Parser.Report(FileName, expression[0], ErrorType.General, """
                This expression is not balanced, one or more parenthesis to not have their matching opening or closing pair, it is an invalid expression
                """);
                ErrorHandler.Parser.PrettyError(FileName, expression[0]);
            }

            var shuntingYard = Helpers.ShuntingYard(expression, Helpers.BooleanPrecedence);

            if (!Helpers.EvaluatePostfix(shuntingYard, Helpers.BooleanPrecedence, out Token error))
            {
                ErrorHandler.Parser.Report(FileName, error, ErrorType.General, """
                This expression cannot be correctly evaluated. Please make sure that all operators have their matching operands.
                """);
                ErrorHandler.Parser.PrettyError(FileName, error);
            }
        }

        void StartRelationalOperator()
        {
            string[] operators =
            {
                "<",
                ">",
                "<=",
                ">=",
                "="
            };

            if (CurrentEquals("IS") && (LookaheadEquals(1, "GREATER", "LESS", "EQUAL", "NOT") || LookaheadEquals(1, TokenType.Symbol)))
            {
                Continue();
            }

            if (CurrentEquals("NOT") && LookaheadEquals(1, ">", "<"))
            {
                Continue(2);
            }
            else if (CurrentEquals("NOT") && LookaheadEquals(1, "GREATER", "LESS"))
            {
                Continue(2);

                if (CurrentEquals("THAN", "TO")) Continue();
            }
            else if (CurrentEquals("GREATER", "LESS", "EQUAL"))
            {
                if (CurrentEquals("GREATER") && (LookaheadEquals(1, "OR") || LookaheadEquals(2, "OR")))
                {
                    if (!LookaheadEquals(1, "THAN")) Continue(2);

                    if (LookaheadEquals(1, "THAN")) Continue(3);
                }

                if (CurrentEquals("LESS") && (LookaheadEquals(1, "OR") || LookaheadEquals(2, "OR")))
                {
                    if (LookaheadEquals(1, "THAN")) Continue(3);

                    if (!LookaheadEquals(1, "THAN")) Continue(2);

                }

                Continue();

                if (Current().value is "THAN" or "TO") Continue();
            }
            else if (CurrentEquals(operators))
            {
                Continue();
            }
            else
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
                Expected a relational operator. With the exceptions being the "IS NOT EQUAL TO" and "IS NOT =" operators 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());

                Continue();
            }
        }

        void UsageClause(string dataItemHash)
        {
            Expected("USAGE");
            Optional("IS");
            switch (Current().value)
            {
                case "BINARY":
                    Expected("BINARY");
                    Information.DataItems
                        .AddUsage(dataItemHash, UsageType.Binary);
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
                    Information.DataItems
                        .AddUsage(dataItemHash, UsageType.Bit);
                    break;

                case "COMP":
                case "COMPUTATIONAL":
                    Expected(Current().value);
                    Information.DataItems
                        .AddUsage(dataItemHash, UsageType.Computational);
                    break;

                case "DISPLAY":
                    Expected("DISPLAY");
                    Information.DataItems
                        .AddUsage(dataItemHash, UsageType.Display);
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
                    Information.DataItems
                        .AddUsage(dataItemHash, UsageType.Index);
                    break;

                case "MESSAGE-TAG":
                    Expected("MESSAGE-TAG");
                    Information.DataItems
                        .AddUsage(dataItemHash, UsageType.MessageTag);
                    break;

                case "NATIONAL":
                    Expected("NATIONAL");
                    break;

                case "OBJECT":
                    Expected("OBJECT");
                    Expected("REFERENCE");
                    // Need implement identifier resolution first
                    // To parse the rest of this using clause
                    Information.DataItems
                        .AddUsage(dataItemHash, UsageType.ObjectReference);
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
                        Information.DataItems
                            .AddUsage(dataItemHash, UsageType.DataPointer, Current().value);
                        Identifier();
                    }
                    else
                    {
                        Information.DataItems
                            .AddUsage(dataItemHash, UsageType.DataPointer);
                    }
                    break;

                case "FUNCTION-POINTER":
                    Expected("FUNCTION-POINTER");
                    Optional("TO");
                    Information.DataItems
                        .AddUsage(dataItemHash, UsageType.FunctionPointer, Current().value);
                    Identifier();
                    break;

                case "PROGRAM-POINTER":
                    Expected("PROGRAM-POINTER");
                    if (CurrentEquals("TO") || CurrentEquals(TokenType.Identifier))
                    {
                        Optional("TO");
                        Information.DataItems
                            .AddUsage(dataItemHash, UsageType.ProgramPointer, Current().value);
                        Identifier();
                    }
                    else
                    {
                        Information.DataItems
                            .AddUsage(dataItemHash, UsageType.ProgramPointer);
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

        void EncodingEndianness(bool encodingExists = false, bool endiannessExists = false)
        {
            if (CurrentEquals("BINARY-ENCODING", "DECIMAL-ENCODING"))
            {
                if (encodingExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The encoding phrase can only be specified once in this clause. 
                    The same applies to the endianness phrase.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                encodingExists = true;
                Expected(Current().value);

                WriteBeforeAfter(encodingExists, endiannessExists);

            }

            if (CurrentEquals("HIGH-ORDER-LEFT", "HIGH-ORDER-RIGHT"))
            {
                if (endiannessExists)
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The endianness phrase can only be specified once in this clause. 
                    The same applies to the encoding phrase.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }
                endiannessExists = true;
                Expected(Current().value);

                WriteBeforeAfter(encodingExists, endiannessExists);
            }
        }

        EvaluateOperand SelectionSubject()
        {
            if (CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String) && !LookaheadEquals(1, TokenType.Symbol))
            {
                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                    return EvaluateOperand.Identifier;
                }

                ParseLiteral(true, true);
                return EvaluateOperand.Literal;
            }
            else if (CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String) && LookaheadEquals(1, TokenType.Symbol))
            {
                if (Helpers.ArithmeticPrecedence.ContainsKey(Lookahead(1).value))
                {
                    Arithmetic("ALSO", "WHEN");
                    return EvaluateOperand.Arithmetic;
                }
                else
                {
                    Condition("ALSO", "WHEN");
                    return EvaluateOperand.Condition;
                }
            }
            else if (CurrentEquals("TRUE", "FALSE"))
            {
                Choice("TRUE", "FALSE");
                return EvaluateOperand.TrueOrFalse;
            }

            return EvaluateOperand.Invalid;
        }

        void SelectionObject(EvaluateOperand operand)
        {
            bool identifier = operand is
                EvaluateOperand.Identifier or EvaluateOperand.Literal or
                EvaluateOperand.Arithmetic or EvaluateOperand.Boolean;

            bool literal = operand is
                EvaluateOperand.Identifier or EvaluateOperand.Arithmetic or 
                EvaluateOperand.Boolean;

            bool arithmetic = operand is
                EvaluateOperand.Identifier or EvaluateOperand.Literal or 
                EvaluateOperand.Arithmetic;

            bool boolean = operand is
                EvaluateOperand.Identifier or EvaluateOperand.Literal or 
                EvaluateOperand.Boolean;

            bool range = operand is
                EvaluateOperand.Identifier or EvaluateOperand.Literal or 
                EvaluateOperand.Arithmetic;

            bool condition = operand is 
                EvaluateOperand.Condition or EvaluateOperand.TrueOrFalse;
                
            bool truefalse = operand is
                EvaluateOperand.Condition or EvaluateOperand.TrueOrFalse;

            if (identifier || literal && CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String) && !LookaheadEquals(1, TokenType.Symbol))
            {
                if (identifier && CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                    RangeExpression(range, EvaluateOperand.Identifier);
                }
                else
                {
                    ParseLiteral(true, true);
                    RangeExpression(range, EvaluateOperand.Literal);
                }
            }
            else if (arithmetic || condition && CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String) && LookaheadEquals(1, TokenType.Symbol))
            {
                if (arithmetic && Helpers.ArithmeticPrecedence.ContainsKey(Lookahead(1).value))
                {
                    Arithmetic("ALSO", "WHEN");
                    RangeExpression(range, EvaluateOperand.Arithmetic);
                }
                else
                {
                    Condition("ALSO", "WHEN");
                }
            }
            else if (truefalse && CurrentEquals("TRUE", "FALSE"))
            {
                Choice("TRUE", "FALSE");
            }
            else if (CurrentEquals("ANY"))
            {
                Expected("ANY");
            }
        }

        void RangeExpression(bool canHaveRange, EvaluateOperand rangeType)
        {
            if (canHaveRange && CurrentEquals("THROUGH", "THRU"))
            {
                Choice("THROUGH", "THRU");
                if (rangeType is EvaluateOperand.Identifier)
                {
                    Identifier();
                }
                else if (rangeType is EvaluateOperand.Literal)
                {
                    ParseLiteral(true, true);
                }
                else if (rangeType is EvaluateOperand.Arithmetic)
                {
                    Arithmetic("ALSO", "WHEN");
                }

                if (CurrentEquals("IS", "UTF-8"))
                {
                    Optional("IS");
                    // Need to implement other alphabet support
                    Expected("UTF-8");
                }
            }
        }

        void ParseLiteral(bool numeric, bool @string)
        {
            if (!CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                Expected an identifier or a literal
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            if (numeric && CurrentEquals(TokenType.Numeric))
            {
                Number();
            }
            else if (@string && CurrentEquals(TokenType.String))
            {
                String();
            }
        }

        bool NotIdentifierOrLiteral()
        {
            return !CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String);
        }

        bool IdentifierOrLiteral()
        {
            return CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String);
        }

    }


    // Parser Helper methods.
    // These are the main methods used to interact with and iterate through the List of Tokens.
    // All other methods inside of the parser depend on these to parse through the tokens.

    private static void AnchorPoint(params string[] anchors)
    {
        ErrorHandler.Parser.AttemptRecovery(anchors);

        while (!CurrentEquals(TokenType.EOF))
        {
            if (CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Recovery, """
                Parser recovered at the following anchor point: 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current(), ConsoleColor.Blue);
                Continue();
                return;
            }

            if (CurrentEquals(anchors))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Recovery, """
                Parser recovered at the following anchor point: 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current(), ConsoleColor.Blue);
                return;
            }

            Continue();
        }
    }

    private static void AnchorPoint(TokenContext anchor)
    {
        ErrorHandler.Parser.AttemptRecovery(anchor);

        while (!CurrentEquals(TokenType.EOF))
        {
            if (CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Recovery, """
                Parser recovered at the following anchor point: 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current(), ConsoleColor.Blue);
                Continue();
                return;
            }

            if (CurrentEquals(anchor))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Recovery, """
                Parser recovered at the following anchor point: 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current(), ConsoleColor.Blue);
                return;
            }

            Continue();
        }
    }

    private static void CombinedAnchorPoint(TokenContext anchor, params string[] anchors)
    {
        ErrorHandler.Parser.AttemptRecovery(anchors);

        while (!CurrentEquals(TokenType.EOF))
        {
            if (CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Recovery, """
                Parser recovered at the following anchor point: 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current(), ConsoleColor.Blue);
                Continue();
                return;
            }

            if (CurrentEquals(anchors) || CurrentEquals(anchor))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Recovery, """
                Parser recovered at the following anchor point: 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current(), ConsoleColor.Blue);
                return;
            }

            Continue();
        }
    }


    /// <summary>
    /// Token <c>Lookahead</c>: This method returns a Token from an index of Current Index + the amount parameter
    /// <para>When passed a positive amount it will act as a lookahead method, and when passed a negative amount it will act as a lookbehind method</para>
    /// <para>Technically this method allows for infinite lookahead and lookbehind, as long as the Index + amount is not bigger than the
    /// number of items on the list of Tokens or smaller than 0.</para>
    /// </summary>
    private static Token Lookahead(int amount)
    {
        if (Index + amount >= TokenList.Count) return TokenList[^1];

        return Index + amount < 0 ? TokenList[0] : TokenList[Index + amount];
    }

    /// <summary>
    /// Boolean <c>LookaheadEquals</c> string[]: This method returns true or false depending on if the Token value from an index of Current Index + the first parameter is equal to the values of the second paramenter
    /// <para>When passed a positive amount it will act as a lookahead comparison method, and when passed a negative amount it will act as a lookbehind comparison method</para>
    /// <para>Technically this method allows for infinite lookahead and lookbehind comparisons, as long as the Index + amount is not bigger than the
    /// number of items on the list of Tokens or smaller than 0.</para>
    /// </summary>
    private static bool LookaheadEquals(int lookahead, params string[] valuesToCompare)
    {
        foreach (var value in valuesToCompare)
        {
            if (Lookahead(lookahead).value.Equals(value)) return true;
        }

        return false;
    }

    /// <summary>
    /// Boolean <c>LookaheadEquals</c> TokenType[]: This method returns true or false depending on if the Token type from an index of Current Index + the first parameter is equal to the types of the second paramenter
    /// <para>When passed a positive amount it will act as a lookahead comparison method, and when passed a negative amount it will act as a lookbehind comparison method</para>
    /// <para>Technically this method allows for infinite lookahead and lookbehind comparisons, as long as the Index + amount is not bigger than the
    /// number of items on the list of Tokens or smaller than 0.</para>
    /// </summary>
    private static bool LookaheadEquals(int lookahead, params TokenType[] tokenTypesToCompare)
    {
        foreach (var type in tokenTypesToCompare)
        {
            if (Lookahead(lookahead).type.Equals(type)) return true;
        }

        return false;
    }

    /// <summary>
    /// Boolean <c>LookaheadEquals</c> TokenScope[]: This method returns true or false depending on if the Token scope from an index of Current Index + the first parameter is equal to the scopes of the second paramenter
    /// <para>When passed a positive amount it will act as a lookahead comparison method, and when passed a negative amount it will act as a lookbehind comparison method</para>
    /// <para>Technically this method allows for infinite lookahead and lookbehind comparisons, as long as the Index + amount is not bigger than the
    /// number of items on the list of Tokens or smaller than 0.</para>
    /// </summary>
    private static bool LookaheadEquals(int lookahead, params TokenScope[] tokenScopesToCompare)
    {
        foreach (var scope in tokenScopesToCompare)
        {
            if (Lookahead(lookahead).scope.Equals(scope)) return true;
        }

        return false;
    }

    /// <summary>
    /// Boolean <c>LookaheadEquals</c> TokenContext[]: This method returns true or false depending on if the Token context from an index of Current Index + the first parameter is equal to the contexts of the second paramenter
    /// <para>When passed a positive amount it will act as a lookahead comparison method, and when passed a negative amount it will act as a lookbehind comparison method</para>
    /// <para>Technically this method allows for infinite lookahead and lookbehind comparisons, as long as the Index + amount is not bigger than the
    /// number of items on the list of Tokens or smaller than 0.</para>
    /// </summary>
    private static bool LookaheadEquals(int lookahead, params TokenContext[] tokenContextsToCompare)
    {
        foreach (var context in tokenContextsToCompare)
        {
            if (Lookahead(lookahead).context.Equals(context)) return true;
        }

        return false;
    }

    /// <summary>
    /// Token <c>Current</c>: This method returns the current token from the current Index.
    /// <para>The returned token in encapsulated inside of the Token type, which holds not only the value of the token but also some additional context
    /// that might be required by the parser in certain situations</para>
    /// </summary>
    private static Token Current()
    {
        return TokenList[Index];
    }

    /// <summary>
    /// Boolean <c>CurrentEquals</c> string[]: This method returns true or false depending on if the current token from the current Index has the same value as the parameter.
    /// <para>This helper method is an alternative to the <c>"Current().value.Equals()"</c> syntax, which could become verbose and harder to read when it's used frequently</para>
    /// </summary>
    private static bool CurrentEquals(params string[] valuesToCompare)
    {
        foreach (var value in valuesToCompare)
        {
            if (Current().value.Equals(value)) return true;
        }

        return false;
    }

    /// <summary>
    /// Boolean <c>CurrentEquals</c> TokenType[]: This method returns true or false depending on if the current token from the current Index has the same type as the parameter.
    /// <para>This helper method is an alternative to the <c>"Current().value.Equals()"</c> syntax, which could become verbose and harder to read when it's used frequently</para>
    /// </summary>
    private static bool CurrentEquals(params TokenType[] tokenTypesToCompare)
    {
        foreach (var type in tokenTypesToCompare)
        {
            if (Current().type.Equals(type)) return true;
        }

        return false;
    }

    /// <summary>
    /// Boolean <c>CurrentEquals</c> TokenScope[]: This method returns true or false depending on if the current token from the current Index has the same scope as the parameter.
    /// <para>This helper method is an alternative to the <c>"Current().value.Equals()"</c> syntax, which could become verbose and harder to read when it's used frequently</para>
    /// </summary>
    private static bool CurrentEquals(params TokenScope[] tokenScopesToCompare)
    {
        foreach (var scope in tokenScopesToCompare)
        {
            if (Current().scope.Equals(scope)) return true;
        }

        return false;
    }

    /// <summary>
    /// Boolean <c>CurrentEquals</c> TokenContext[]: This method returns true or false depending on if the current token from the current Index has the same context as the parameter.
    /// <para>This helper method is an alternative to the <c>"Current().value.Equals()"</c> syntax, which could become verbose and harder to read when it's used frequently</para>
    /// </summary>
    private static bool CurrentEquals(params TokenContext[] tokenContextsToCompare)
    {
        foreach (var context in tokenContextsToCompare)
        {
            if (Current().context.Equals(context)) return true;
        }

        return false;
    }

    /// <summary>
    /// Void <c>Continue</c>: This method adds +1 to the current index, moving the index to the next token.
    /// <para>This method will be called in the Expected(), Optional(), Choice(), Identifier(), Number() and String() methods,
    /// so there's no need to call Continue after calling those methods.</para>
    /// </summary>
    private static void Continue(int amount = 1)
    {
        if (CurrentEquals(TokenType.EOF) && Index >= TokenList.Count - 1) return;

        Index += amount;
    }

    /// <summary>
    /// Void <c>Choice</c>: This method checks if the current token matches one of the values passed in its parameters.
    /// <para>If the current token matches one the values, it moves to the next token,
    /// if the current token doesn't match any of the values it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void Choice(params string[] choices)
    {
        Token token = Current();
        foreach (string choice in choices)
        {
            if (CurrentEquals(choice))
            {
                Continue();
                return;
            }
        }

        ErrorHandler.Parser.Report(FileName, token, ErrorType.Choice, choices);
        ErrorHandler.Parser.PrettyError(FileName, token);
        Continue();
    }

    /// <summary>
    /// Void <c>Optional</c>: This method checks if the current token is equal to it's first parameter.
    /// <para>If the current token matches the value, it moves to the next token,
    /// if the current token doesn't match the value it ignores the token and returns without moving to the next token</para>
    /// </summary>
    private static void Optional(string optional)
    {
        if (CurrentEquals(optional)) Continue();
    }

    /// <summary>
    /// Void <c>Optional</c>: This method checks if the current token is equal to it's first parameter.
    /// <para>If the current token matches the type, it moves to the next token,
    /// if the current token doesn't match the type it ignores the token and returns without moving to the next token</para>
    /// </summary>
    private static void Optional(TokenType optional)
    {
        if (CurrentEquals(optional)) Continue();
    }

    /// <summary>
    /// Void <c>Expected</c>: This method checks if the current token is equal to it's first parameter.
    /// <para>If the current token matches the value, it moves to the next token,
    /// if the current token doesn't match the value it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void Expected(string expected, string custom = "default", int position = 0, params string[] wordAnchors)
    {
        var errorMessage = expected;
        var errorType = ErrorType.Expected;
        if (!custom.Equals("default"))
        {
            errorMessage = custom;
            errorType = ErrorType.General;
        }

        if (CurrentEquals(TokenType.EOF))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
            Unexpected End Of File. Expected {expected} instead.
            """);

            return;
        }

        if (!CurrentEquals(expected))
        {
            var lookahead = Lookahead(position);

            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);

            if (wordAnchors.Length != 0) AnchorPoint(wordAnchors);

            if (wordAnchors.Length == 0) Continue();
        }
        else
        {
            Continue();
        }
    }

    private static void Expected(string expected, string custom = "default", int position = 0, TokenContext? typeAnchor = null)
    {
        var errorMessage = expected;
        var errorType = ErrorType.Expected;
        if (!custom.Equals("default"))
        {
            errorMessage = custom;
            errorType = ErrorType.General;
        }

        if (CurrentEquals(TokenType.EOF))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
            Unexpected End Of File. Expected {expected} instead.
            """);

            return;
        }

        if (!CurrentEquals(expected))
        {
            var lookahead = Lookahead(position);

            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);

            if (typeAnchor is not null) AnchorPoint((TokenContext)typeAnchor);

            if (typeAnchor is null) Continue();
        }
        else
        {
            Continue();
        }
    }

    /// <summary>
    /// Void <c>Identifier</c>: This method checks if the current token is an identifier.
    /// <para>If the current token's type is TokenType.Identifier, it moves to the next token,
    /// if the current token's type is TokenType.Identifier it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void Identifier(params UsageType[] allowedUsage)
    {
        if (CurrentEquals(TokenType.EOF))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
            Unexpected End Of File. Expected identifier instead.
            """);

            return;
        }

        if (!CurrentEquals(TokenType.Identifier))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, """
            a user-defined name or word (an identifier)
            """);
            ErrorHandler.Parser.PrettyError(FileName, Current());
            Continue();
            return;
        }

        string dataItemHash = $"{SourceId}#{Current().value}";

        if (CurrentSection is CurrentScope.ProcedureDivision)
        {
            if (!Information.DataItems.ValueExists(dataItemHash))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
                The name "{Current().value}" does not exist in the context of the current source unit
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
        }

        if (allowedUsage.Length >= 1)
        {
            bool hasAllowedUsage = false;

            foreach (var usage in allowedUsage)
            {
                if (Information.DataItems.GetValue(dataItemHash).UsageType == usage) 
                    hasAllowedUsage = true;
            }

            if (!hasAllowedUsage)
            {
                StringBuilder errorBuilder = new();
                string separator = string.Empty;

                foreach (var usage in allowedUsage)
                {
                    errorBuilder.Append(separator);
                    errorBuilder.Append(usage.Display());
                    separator = ",";
                }

                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
                Expected a data item defined with the following: {errorBuilder.ToString()} USAGE clauses or PICTURE types
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
        }

        Continue();
    }

    private static bool Identifier(bool checkFirstUsage, params UsageType[] allowedUsage)
    {
        if (CurrentEquals(TokenType.EOF))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
            Unexpected End Of File. Expected identifier instead.
            """);

            return false;
        }

        if (!CurrentEquals(TokenType.Identifier))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, """
            a user-defined name or word (an identifier)
            """);
            ErrorHandler.Parser.PrettyError(FileName, Current());
            Continue();
            return false;
        }

        string dataItemHash = $"{SourceId}#{Current().value}";

        if (CurrentSection is CurrentScope.ProcedureDivision)
        {
            if (!Information.DataItems.ValueExists(dataItemHash))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
                The name "{Current().value}" does not exist in the context of the current source unit
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
        }

        if (allowedUsage.Length >= 1)
        {
            bool hasAllowedUsage = false;
            DataItemInfo dataItem = Information.DataItems.GetValue(dataItemHash);
            foreach (var usage in allowedUsage)
            {
                if (dataItem.UsageType == usage) 
                    hasAllowedUsage = true;

                if (usage != allowedUsage[0])
                    checkFirstUsage = false;
            }

            if (!hasAllowedUsage)
            {
                StringBuilder errorBuilder = new();
                string separator = string.Empty;

                foreach (var usage in allowedUsage)
                {
                    errorBuilder.Append(separator);
                    errorBuilder.Append(usage.Display());
                    separator = ",";
                }

                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
                Expected a data item defined with the following: {errorBuilder.ToString()} USAGE clauses or PICTURE types
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
        }

        Continue();
        return checkFirstUsage;
    }

    private static void Identifier(string identifierString)
    {
        if (CurrentEquals(TokenType.EOF))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
            Unexpected End Of File. Expected identifier instead.
            """);

            return;
        }

        if (!CurrentEquals(TokenType.Identifier))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, """
            a user-defined name or word (an identifier)
            """);
            ErrorHandler.Parser.PrettyError(FileName, Current());
            Continue();
            return;
        }

        if (!CurrentEquals(identifierString))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
            Expected a user-defined name (an identifier) with value: "{identifierString}"
            """);
            ErrorHandler.Parser.PrettyError(FileName, Current());
        }

        Continue();
    }

    /// <summary>
    /// Void <c>Number</c>: This method checks if the current token is a Number.
    /// <para>If the current token's type is TokenType.Numeric, it moves to the next token,
    /// if the current token's type is TokenType.Numeric it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void Number(string custom = "default", int position = 0)
    {
        var errorMessage = "Numeric literal";
        var errorType = ErrorType.Expected;
        if (!custom.Equals("default"))
        {
            errorMessage = custom;
            errorType = ErrorType.General;
        }

        if (!CurrentEquals(TokenType.Numeric))
        {
            var lookahead = Lookahead(position);

            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);
            Continue();
        }
        else
        {
            Continue();
        }
    }

    /// <summary>
    /// Void <c>String</c>: This method checks if the current token is a National, Alphanumeric, Alphabetic or Boolean.
    /// <para>If the current token's type is TokenType.String, it moves to the next token,
    /// if the current token's type is TokenType.String it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void String(string custom = "default", int position = 0)
    {
        var errorMessage = "String literal";
        var errorType = ErrorType.Expected;
        if (!custom.Equals("default"))
        {
            errorMessage = custom;
            errorType = ErrorType.General;
        }

        if (!CurrentEquals(TokenType.String))
        {
            var lookahead = Lookahead(position);

            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);
            Continue();
        }
        else
        {
            Continue();
        }
    }

    /// <summary>
    /// Void <c>FigurativeLiteral</c>: This method checks if the current token is a Figurative Literal.
    /// <para>If the current token's type is TokenType.FigurativeLiteral, it moves to the next token,
    /// if the current token's type is TokenType.FigurativeLiteral it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void FigurativeLiteral(string custom = "default", int position = 0)
    {
        var errorMessage = "Numeric literal";
        var errorType = ErrorType.Expected;
        if (!custom.Equals("default"))
        {
            errorMessage = custom;
            errorType = ErrorType.General;
        }

        if (!CurrentEquals(TokenType.FigurativeLiteral))
        {
            var lookahead = Lookahead(position);

            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);
            Continue();
        }
        else
        {
            Continue();
        }
    }

    /// <summary>
    /// Void <c>Symbol</c>: This method checks if the current token is a valid COBOl Symbol.
    /// <para>If the current token's type is TokenType.Symbol, it moves to the next token,
    /// if the current token's type is TokenType.Symbol it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void Symbol(string custom = "default", int position = 0)
    {
        var errorMessage = "a COBOL symbol";
        var errorType = ErrorType.Expected;
        if (!custom.Equals("default"))
        {
            errorMessage = custom;
            errorType = ErrorType.General;
        }

        if (!CurrentEquals(TokenType.Symbol))
        {
            var lookahead = Lookahead(position);

            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);
            Continue();
        }
        else
        {
            Continue();
        }
    }

}
