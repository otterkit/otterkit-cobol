namespace Otterkit;

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
    private static string FileName = string.Empty;

    /// <summary>
    /// String <c>SourceId</c> is used in the parser whenever it needs to know the name of the current source unit (The identifier after PROGRAM-ID).
    /// <para>This is used when checking if a variable already exists in the current source unit, and when adding them to the DataItemInformation class's variable table.
    /// The DataItemInformation class is then used to simplify the codegen process of generating data items.</para>
    /// </summary>
    private static string SourceId = string.Empty;

    /// <summary>
    /// String <c>SourceType</c> is used in the parser whenever it needs to know which <c>-ID</c> it is currently parsing.
    /// <para>This is used when handling certain syntax rules for different <c>-ID</c>s, like the <c>"RETURNING data-name"</c> being required for every <c>FUNCTION-ID</c> source unit.</para>
    /// </summary>
    private static string SourceType = string.Empty;

    /// <summary>
    /// String <c>CurrentSection</c> is used in the parser whenever it needs to know which section it is currently parsing (WORKING-STORAGE and LOCAL-STORAGE for example).
    /// <para>This is used when handling certain syntax rules for different sections and to add extra context needed for the DataItemInformation class's variable table.
    /// This will also be used by the DataItemInformation class during codegen to simplify the process to figuring out if a variable is static or not.</para>
    /// </summary>
    private static string CurrentSection = string.Empty;

    /// <summary>
    /// List of Tokens <c>TokenList</c>: This is the main data structure that the parser will be iterating through.
    /// <para>The parser expects a list of already preprocessed and classified COBOL tokens in the form of full COBOL words (CALL, END-IF, COMPUTE for example)</para>
    /// </summary>
    private static List<Token> TokenList = new();

    /// <summary>
    /// Int <c>Index</c>: This is the index of the current token, used by most helper methods including Continue, Current and Lookahead.
    /// <para>The index should only move forwards, but if the previous token is needed you can use the Lookahead and LookaheadEquals methods with a negative integer parameter</para>
    /// </summary>
    private static int Index;

    /// <summary>
    /// List of Tokens <c>Analyzed</c>: This list will contain the parsed Tokens after the parser is finished with the TokenList.
    /// <para>This parser does not generate an AST, instead it returns this list only if the COBOL source code was written correctly and passed all parsing checks.
    /// Generating a full AST and the code needed to handle it would add extra complexity to the compiler, the AST is also not needed for the codegen to work</para>
    /// </summary>
    private static List<Token> Analyzed = new();

    /// <summary>
    /// Otterkit COBOL Syntax Analyzer
    /// <para>This parser was built to be easily extensible, with some reusable COBOL parts.</para>
    /// <para>It requires a List of Tokens generated from the Lexer and the Token Classifier.</para>
    /// </summary>
    public static List<Token> Analyze(List<Token> tokenList, string fileName)
    {
        FileName = fileName;
        TokenList = tokenList;
        Analyzed = new();
        Index = 0;

        // Call the parser's main method
        // This should only return when the parser reaches the EOF token
        Source();

        // If a parsing error has occured, terminate the compilation process.
        // We do not want the compiler to continue when the source code is not valid.
        if (ErrorHandler.Error) ErrorHandler.Terminate("parsing");

        // Return parsed list of tokens.
        return Analyzed;

        // Source() is the main method of the parser.
        // It's responsible for parsing COBOL divisions until the EOF token.
        // If EOF was not returned as the last Token in the list then,
        // the parser has not finished reading through the list of tokens correctly.
        void Source()
        {
            IDENTIFICATION();
            if (CurrentEquals("ENVIRONMENT"))
                ENVIRONMENT();

            if (CurrentEquals("DATA"))
                DATA();

            PROCEDURE();

            if (CurrentEquals("IDENTIFICATION") || CurrentEquals("PROGRAM-ID") || CurrentEquals("FUNCTION-ID"))
                Source();
            
            if (CurrentEquals("EOF"))
                Analyzed.Add(Current());

        }


        // Method responsible for parsing the IDENTIFICATION DIVISION.
        // That includes PROGRAM-ID, FUNCTION-ID, CLASS-ID, METHOD-ID, INTERFACE-ID, OBJECT, FACTORY and OPTIONS paragraphs.
        // It is also responsible for showing appropriate error messages when an error occurs in the IDENTIFICATION DIVISION.
        void IDENTIFICATION()
        {
            const string headerPeriodError = """
            Missing separator period at the end of this IDENTIFICATION DIVISION header, every division header must end with a separator period
            """;

            if (CurrentEquals("IDENTIFICATION"))
            {
                Expected("IDENTIFICATION");
                Expected("DIVISION");
                Expected(".", headerPeriodError, -1, "separator period");
            }

            if (!CurrentEquals("PROGRAM-ID") && !CurrentEquals("FUNCTION-ID"))
            {
                const string missingIdentificationError = """
                Missing source unit ID name (PROGRAM-ID, FUNCTION-ID, CLASS-ID...), the identification division header is optional but every source unit must still have an ID.
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", missingIdentificationError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }

            if (CurrentEquals("PROGRAM-ID"))
                ProgramId();

            if (CurrentEquals("FUNCTION-ID"))
                FunctionId();
        }


        // The following methods are responsible for parsing the -ID paragraph.
        // That includes the program, user-defined function, method, class, interface, factory or object identifier that should be specified right after.
        // This is where SourceId and SourceType get their values for a COBOL source unit.
        void ProgramId()
        {
            Expected("PROGRAM-ID");
            Expected(".");
            SourceId = Current().value;
            SourceType = "PROGRAM";
            Identifier();
            Expected(".");
        }

        void FunctionId()
        {
            Expected("FUNCTION-ID");
            Expected(".");
            SourceId = Current().value;
            SourceType = "FUNCTION";
            Identifier();
            Expected(".");
        }


        // Method responsible for parsing the ENVIRONMENT DIVISION.
        // That includes the CONFIGURATION and the INPUT-OUTPUT sections.
        // It is also responsible for showing appropriate error messages when an error occurs in the ENVIRONMENT DIVISION.
        void ENVIRONMENT()
        {
            const string headerPeriodError = """
            Missing separator period at the end of this ENVIRONMENT DIVISION header, every division header must end with a separator period
            """;

            Expected("ENVIRONMENT", "environment division");
            Expected("DIVISION");
            Expected(".", headerPeriodError, -1, "separator period");
        }


        // Method responsible for parsing the DATA DIVISION.
        // That includes the FILE, WORKING-STORAGE, LOCAL-STORAGE, LINKAGE, REPORT and SCREEN sections.
        // It is also responsible for showing appropriate error messages when an error occurs in the DATA DIVISION.
        void DATA()
        {
            const string headerPeriodError = """
            Missing separator period at the end of this DATA DIVISION header, every division header must end with a separator period
            """;

            Expected("DATA", "data division");
            Expected("DIVISION");
            Expected(".", headerPeriodError, -1, "separator period");

            if (CurrentEquals("WORKING-STORAGE"))
                WorkingStorage();

            if (CurrentEquals("LOCAL-STORAGE"))
                LocalStorage();

            if (CurrentEquals("LINKAGE"))
                LinkageSection();

            if (!CurrentEquals("PROCEDURE"))
            {
                ErrorHandler.Parser.Report(fileName, Current(), "expected", "Data Division data items and sections");
                ErrorHandler.Parser.PrettyError(fileName, Current());
                Continue();
            }
        }
       

        // The following methods are responsible for parsing the DATA DIVISION sections
        // They are technically only responsible for parsing the section header, 
        // the Entries() method handles parsing the actual data items in their correct sections.
        void WorkingStorage()
        {
            CurrentSection = Current().value;
            Expected("WORKING-STORAGE");
            Expected("SECTION");
            Expected(".");
            while (Current().type == TokenType.Numeric)
                Entries();
        }

        void LocalStorage()
        {
            CurrentSection = Current().value;
            Expected("LOCAL-STORAGE");
            Expected("SECTION");
            Expected(".");
            while (Current().type is TokenType.Numeric)
                Entries();
        }

        void LinkageSection()
        {
            CurrentSection = Current().value;
            Expected("LINKAGE");
            Expected("SECTION");
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
            int OutInt;
            bool isNum = int.TryParse(Current().value, out OutInt);
            while (OutInt > 1 && OutInt < 50)
            {
                BaseEntry();
                isNum = int.TryParse(Current().value, out OutInt);
            }
        }

        void BaseEntry()
        {
            string dataType = string.Empty;
            int LevelNumber = int.Parse(Current().value);
            Number();

            string DataName = Current().value;
            Identifier();

            string DataItemHash = $"{SourceId}#{DataName}";
            if (!DataItemInformation.AddDataItem(DataItemHash, DataName, LevelNumber, Current()))
            {
                DataItemInfo originalItem = DataItemInformation.GetValue(DataItemHash);
                string duplicateDataItemError = $"""
                A data item with this name already exists in this program, data items in a program must have a unique name.
                The original {originalItem.Identifier} data item can be found at line {originalItem.Line}. 
                """;

                ErrorHandler.Parser.Report(fileName, Lookahead(-1), "general", duplicateDataItemError);
                ErrorHandler.Parser.PrettyError(fileName, Lookahead(-1));
            }

            DataItemInformation.AddSection(DataItemHash, CurrentSection);

            if (Current().context is not TokenContext.IsClause && !CurrentEquals("."))
            {
                string notAClauseError = $"""
                Expected data division clauses or a separator period after this data item's identifier.
                Token found ("{Current().value}") was not a data division clause reserved word.
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", notAClauseError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
                Continue();
            }

            while (Current().context is TokenContext.IsClause)
            {
                if (CurrentEquals("IS") && !(LookaheadEquals(1, "EXTERNAL") || LookaheadEquals(1, "GLOBAL") || LookaheadEquals(1, "TYPEDEF")))
                {
                    const string Externalerror = """
                    Missing clause or possible clause mismatch, in this context the "IS" word must be followed by the EXTERNAL, GLOBAL or TYPEDEF clauses only (IS TYPEDEF), or must be in the middle of the PICTURE clause (PIC IS ...) 
                    """;

                    ErrorHandler.Parser.Report(fileName, Current(), "general", Externalerror);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }

                if ((CurrentEquals("IS") && LookaheadEquals(1, "EXTERNAL")) || CurrentEquals("EXTERNAL"))
                {
                    Optional("IS");
                    Expected("EXTERNAL");
                    if (CurrentEquals("AS"))
                    {
                        const string externalizedNameError = """
                        Missing externalized name, the "AS" word on the EXTERNAL clause must be followed by an alphanumeric or national literal
                        """;
                        Expected("AS");
                        DataItemInformation.IsExternal(DataItemHash, true, Current().value);
                        String(externalizedNameError, -1);
                    }

                    if (!CurrentEquals("AS"))
                        DataItemInformation.IsExternal(DataItemHash, true, DataName);
                }

                if (CurrentEquals("PIC") || CurrentEquals("PICTURE"))
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
                        const string dataTypeError = """
                        Unrecognized type, PICTURE type must be S9, 9, X, A, N or 1. These are Signed Numeric, Unsigned Numeric, Alphanumeric, Alphabetic, National and Boolean respectively
                        """;

                        ErrorHandler.Parser.Report(fileName, Current(), "general", dataTypeError);
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                    }

                    DataItemInformation.AddType(DataItemHash, dataType);
                    DataItemInformation.IsElementary(DataItemHash, true);
                    Choice("S9", "9", "X", "A", "N", "1");

                    string DataLength = string.Empty;
                    Expected("(");
                    DataLength = Current().value;
                    Number();
                    Expected(")");
                    if (CurrentEquals("V9") && (dataType != "S9" && (dataType != "9")))
                    {
                        ErrorHandler.Parser.Report(fileName, Current(), " ", "V9 cannot be used with non-numeric types");
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                    }

                    if (CurrentEquals("V9"))
                    {
                        Expected("V9");
                        Expected("(");
                        DataLength += $"V{Current().value}";
                        Number();
                        Expected(")");
                    }

                    DataItemInformation.AddPicture(DataItemHash, DataLength);
                }

                if (CurrentEquals("VALUE"))
                {
                    Expected("VALUE");

                    if (Current().type is not TokenType.String and not TokenType.Numeric)
                    {
                        const string valueError = """
                        The only tokens allowed after a VALUE clause are type literals, like an Alphanumeric literal ("Hello, World!") or a Numeric literal (123.456).
                        """;

                        ErrorHandler.Parser.Report(fileName, Current(), "general", valueError);
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                    }

                    if (Current().type is TokenType.String)
                    {
                        DataItemInformation.AddDefault(DataItemHash, Current().value);
                        String();
                    }

                    if (Current().type is TokenType.Numeric)
                    {
                        DataItemInformation.AddDefault(DataItemHash, Current().value);
                        Number();
                    }
                }

            }

            if (!DataItemInformation.GetValue(DataItemHash).IsElementary)
                DataItemInformation.IsGroup(DataItemHash, true);

            const string separatorPeriodError = """
            Missing separator period at the end of this data item definition, each data item must end with a separator period
            """;
            Expected(".", separatorPeriodError, -1);
        }

        void ConstantEntry()
        {
            if (!CurrentEquals("01") && !CurrentEquals("1"))
            {
                const string levelNumberError = """
                Invalid level number for this data item, CONSTANT data items must have a level number of 1 or 01
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", levelNumberError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }

            var LevelNumber = int.Parse(Current().value);
            Number();

            var DataName = Current().value;
            Identifier();

            var DataItemHash = $"{SourceId}#{DataName}";
            if (!DataItemInformation.AddDataItem(DataItemHash, DataName, LevelNumber, Current()))
            {
                var originalItem = DataItemInformation.GetValue(DataItemHash);
                var duplicateDataItemError = $"""
                A data item with this name already exists in this program, data items in a program must have a unique name.
                The original {originalItem.Identifier} data item can be found at line {originalItem.Line}. 
                """;

                ErrorHandler.Parser.Report(fileName, Lookahead(-1), "general", duplicateDataItemError);
                ErrorHandler.Parser.PrettyError(fileName, Lookahead(-1));
            }
            DataItemInformation.IsConstant(DataItemHash, true);
            DataItemInformation.AddSection(DataItemHash, CurrentSection);

            Expected("CONSTANT");
            if (CurrentEquals("IS") || CurrentEquals("GLOBAL"))
            {
                Optional("IS");
                Expected("GLOBAL");
                DataItemInformation.IsGlobal(DataItemHash, true);
            }

            if (CurrentEquals("FROM"))
            {
                Expected("FROM");
                Identifier();
                Expected(".");
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

                Expected(".");
            }
        }


        // Method responsible for parsing the PROCEDURE DIVISION.
        // That includes the user-defined paragraphs, sections and declaratives
        // or when parsing OOP COBOL code, it's responsible for parsing COBOL methods, objects and factories. 
        // It is also responsible for showing appropriate error messages when an error occurs in the PROCEDURE DIVISION.
        void PROCEDURE()
        {
            const string headerPeriodError = """
            Missing separator period at the end of this PROCEDURE DIVISION header, every division header must end with a separator period
            """;

            Expected("PROCEDURE");
            Expected("DIVISION");
            if (SourceType.Equals("FUNCTION"))
            {
                Expected("RETURNING");
                ReturningDataName();
            }
            else if (!SourceType.Equals("FUNCTION") && CurrentEquals("RETURNING"))
            {
                Expected("RETURNING");
                ReturningDataName(); 
            }

            Expected(".", headerPeriodError, -1);
            Statement();

            if (CurrentEquals("IDENTIFICATION") || CurrentEquals("PROGRAM-ID") || CurrentEquals("FUNCTION-ID"))
            {
                string missingEndMarkerError = $"""
                Missing END {SourceType} marker. If another source unit is present after the end of the current source unit, the current unit must contain an END marker.
                """;

                const string missingEndFunctionMarkerError = $"""
                Missing END FUNCTION marker. User-defined functions must always end with an END FUNCTION marker.
                """;

                string errorMessageChoice = SourceType.Equals("FUNCTION") ? missingEndFunctionMarkerError : missingEndMarkerError;

                ErrorHandler.Parser.Report(fileName, Lookahead(-1), "general", errorMessageChoice);
                ErrorHandler.Parser.PrettyError(fileName, Lookahead(-1));
                return;
            }

            if (SourceType.Equals("PROGRAM") && CurrentEquals("END") && LookaheadEquals(1, "PROGRAM"))
            {
                const string endProgramPeriodError = """
                Missing separator period at the end of this END PROGRAM definition
                """;

                Expected("END");
                Expected("PROGRAM");
                Identifier();
                Expected(".", endProgramPeriodError, -1);
            }

            if (SourceType.Equals("FUNCTION"))
            {
                const string endFunctionPeriodError = """
                Missing separator period at the end of this END FUNCTION definition
                """;

                Expected("END");
                Expected("FUNCTION");
                Identifier();
                Expected(".", endFunctionPeriodError, -1);
            }
        }

        // This method is part of the PROCEDURE DIVISION parsing. It's used to parse the "RETURNING" data item specified in
        // the PROCEDURE DIVISION header. It's separate from the previous method because its code is needed more than once.
        // COBOL user-defined functions should always return a data item.
        void ReturningDataName()
        {
            if (Current().type != TokenType.Identifier)
            {
                const string missingDataItemError = $"""
                Missing returning data item after this RETURNING definition.
                """;

                ErrorHandler.Parser.Report(fileName, Lookahead(-1), "general", missingDataItemError);
                ErrorHandler.Parser.PrettyError(fileName, Lookahead(-1));
                return;
            }

            var DataName = Current().value;
            Identifier();

            var DataItemHash = $"{SourceId}#{DataName}";
            if (!DataItemInformation.ValueExists(DataItemHash))
            {
                const string undefinedDataItemError = $"""
                No data item found with this name in this source unit's data division. 
                Please define a new returning data item in this unit's linkage section.
                """;

                ErrorHandler.Parser.Report(fileName, Lookahead(-1), "general", undefinedDataItemError);
                ErrorHandler.Parser.PrettyError(fileName, Lookahead(-1));
                return;
            }
        }


        // Recursive method responsible for parsing ALL COBOL statements.
        // The while loop continues parsing statements while the current token is a statement reserved word,
        // This might look like it could cause issues by parsing two statements in a single loop, but that
        // can only happen on nested statements, and in those cases it still parses every statement correctly.
        // For other statements, the separator period is required, which is then handled by the ScopeTerminator()
        void Statement(bool isNested = false)
        {
            while (Current().context == TokenContext.IsStatement)
            {
                if (CurrentEquals("ACCEPT"))
                    ACCEPT();

                if (CurrentEquals("ADD"))
                    ADD();

                if (CurrentEquals("ALLOCATE"))
                    ALLOCATE();

                if (CurrentEquals("CALL"))
                    CALL();

                if (CurrentEquals("CANCEL"))
                    CANCEL();

                if (CurrentEquals("CLOSE"))
                    CLOSE();

                if (CurrentEquals("COMMIT"))
                    COMMIT();

                if (CurrentEquals("CONTINUE"))
                    CONTINUE();

                if (CurrentEquals("COMPUTE"))
                    COMPUTE();

                if (CurrentEquals("DISPLAY"))
                    DISPLAY();

                if (CurrentEquals("DIVIDE"))
                    DIVIDE();

                if (CurrentEquals("DELETE"))
                    DELETE();

                if (CurrentEquals("IF"))
                    IF();

                if (CurrentEquals("INITIATE"))
                    INITIATE();

                if (CurrentEquals("MULTIPLY"))
                    MULTIPLY();

                if (CurrentEquals("MOVE"))
                    MOVE();

                if (CurrentEquals("EXIT"))
                    EXIT();

                if (CurrentEquals("FREE"))
                    FREE();

                if (CurrentEquals("GENERATE"))
                    GENERATE();

                if (CurrentEquals("GO"))
                    GO();

                if (CurrentEquals("GOBACK"))
                    GOBACK();

                if (CurrentEquals("SUBTRACT"))
                    SUBTRACT();

                if (CurrentEquals("RELEASE"))
                    RELEASE();

                if (CurrentEquals("RAISE"))
                    RAISE();

                if (CurrentEquals("RESUME"))
                    RESUME();

                if (CurrentEquals("RETURN"))
                    RETURN();

                if (CurrentEquals("REWRITE"))
                    REWRITE();

                if (CurrentEquals("ROLLBACK"))
                    ROLLBACK();

                if (CurrentEquals("STOP"))
                    STOP();

                if (CurrentEquals("SUPPRESS"))
                    SUPPRESS();

                if (CurrentEquals("TERMINATE"))
                    TERMINATE();

                if (CurrentEquals("UNLOCK"))
                    UNLOCK();

                if (CurrentEquals("VALIDATE"))
                    VALIDATE();

                ScopeTerminator(isNested);
                Statement(isNested);
            }
        }


        // This method handles COBOL's slightly inconsistent separator period rules.
        // Statements that are nested inside another statement cannot end with a separator period,
        // since that separator period would mean the end of the containing statement and not the contained statement.
        void ScopeTerminator(bool isNested)
        {
            if (isNested)
                return;

            Expected(".");
        }


        // Statement parsing methods
        // All the following uppercased methods are responsible for parsing a single COBOL statement
        // When a new method is added here to parse a new statement, we need to add it to the Statement() method as well.
        // Adding extra statements to the parser only requires a new method here, and an if statement added to the Statement() method
        void DISPLAY()
        {
            Expected("DISPLAY");
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
                default:
                    ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier or literal");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                    break;
            }

            while (Current().type == TokenType.Identifier
                || Current().type == TokenType.Numeric
                || Current().type == TokenType.String
                )
            {
                if (Current().type == TokenType.Identifier)
                    Identifier();

                if (Current().type == TokenType.Numeric)
                    Number();

                if (Current().type == TokenType.String)
                    String();
            }

            if (CurrentEquals("UPON"))
            {
                Expected("UPON");
                Choice("STANDARD-OUTPUT", "STANDARD-ERROR");
            }

            if (CurrentEquals("WITH") || CurrentEquals("NO"))
            {
                Optional("WITH");
                Expected("NO");
                Expected("ADVANCING");
            }

            Optional("END-DISPLAY");
        }

        void ACCEPT()
        {
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

            Optional("END-ACCEPT");
        }

        void ALLOCATE()
        {
            Expected("ALLOCATE");
            if (Current().type == TokenType.Identifier && !LookaheadEquals(1, "CHARACTERS") && Lookahead(1).type != TokenType.Symbol)
                Identifier();

            if (Current().type == TokenType.Identifier || Current().type == TokenType.Numeric)
            {
                Arithmetic();
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
            if (Current().type != TokenType.Identifier)
            {
                ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier");
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }

            while (Current().type == TokenType.Identifier)
            {
                Identifier();
            }

            Expected("=");
            if (Current().type != TokenType.Identifier && Current().type != TokenType.Numeric && Current().type != TokenType.Symbol)
            {
                ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier, numeric literal or valid arithmetic symbol");
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }

            Arithmetic();

            if (CurrentEquals("."))
            {
                Expected(".");
                return;
            }

            SizeError(ref isConditional);

            if (isConditional)
                Expected("END-COMPUTE");
        }

        void CALL()
        {
            Expected("CALL");
            String();
            Optional("END-CALL");
        }

        void CONTINUE()
        {
            Expected("CONTINUE");
            if (CurrentEquals("AFTER"))
            {
                Expected("AFTER");
                Arithmetic();
                Expected("SECONDS");
            }
        }

        void ADD()
        {
            bool isConditional = false;

            Expected("ADD");
            if (Current().type != TokenType.Identifier && Current().type != TokenType.Numeric)
                ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier or numeric literal");

            while (Current().type == TokenType.Identifier
                || Current().type == TokenType.Numeric
            )
            {
                if (Current().type == TokenType.Identifier)
                    Identifier();

                if (Current().type == TokenType.Numeric)
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
                        ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier or numeric literal");
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                        break;
                }

                Expected("GIVING");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else if (CurrentEquals("GIVING"))
            {
                Expected("GIVING");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else if (CurrentEquals("TO"))
            {
                Expected("TO");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else
            {
                ErrorHandler.Parser.Report(fileName, Current(), "expected", "TO or GIVING");
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }

            SizeError(ref isConditional);

            if (isConditional)
                Expected("END-ADD");
        }

        void SUBTRACT()
        {
            bool isConditional = false;

            Expected("SUBTRACT");
            if (Current().type != TokenType.Identifier && Current().type != TokenType.Numeric)
            {
                ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier or numeric literal");
                ErrorHandler.Parser.PrettyError(fileName, Current());
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
                        ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier or numeric literal");
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                        break;
                }

                Expected("GIVING");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else if (CurrentEquals("FROM"))
            {
                Expected("FROM");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else
            {
                ErrorHandler.Parser.Report(fileName, Current(), "expected", "FROM");
                ErrorHandler.Parser.PrettyError(fileName, Current());
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
                string archaicFeatureError = """
                Unsupported phrase: NEXT SENTENCE is an archaic feature. This phrase can be confusing and is a common source of errors.
                The CONTINUE statement can be used to accomplish the same functionality while being much clearer and less prone to error
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", archaicFeatureError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }
            Statement(true);
            if (CurrentEquals("ELSE"))
            {
                Expected("ELSE");
                Statement(true);
            }

            Expected("END-IF");
        }

        void INITIATE()
        {
            Expected("INITIATE");
            if (Current().type != TokenType.Identifier)
            {
                string notIdentifierError = """
                The INITIATE statement must only contain report entry identifiers defined in the report section.
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", notIdentifierError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }
            Identifier();
            while (Current().type == TokenType.Identifier)
                Identifier();

            if (!CurrentEquals("."))
            {
                string notIdentifierError = """
                The INITIATE statement must only contain report entry identifiers defined in the report section.
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", notIdentifierError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
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
                    ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier or numeric literal");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
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
                        ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier or numeric literal");
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                        break;
                }

                Expected("GIVING");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else if (CurrentEquals("BY"))
            {
                Expected("BY");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else
            {
                ErrorHandler.Parser.Report(fileName, Current(), "expected", "BY");
                ErrorHandler.Parser.PrettyError(fileName, Current());
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
                string notIdentifierOrLiteralError = """
                The MOVE statement must only contain a single data item identifier, datatype literal or an intrisic function which returns a data item before the "TO" reserved word.
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", notIdentifierOrLiteralError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
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
                string notIdentifierError = """
                The MOVE statement must only contain data item identifiers after the "TO" reserved word.
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", notIdentifierError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }

            while (Current().type == TokenType.Identifier)
                Identifier();

            if (!CurrentEquals("."))
            {
                string notIdentifierError = """
                The MOVE statement must only contain data item identifiers after the "TO" reserved word.
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", notIdentifierError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
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
                    ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier or numeric literal");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
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
                        ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier or numeric literal");
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                        break;
                }

                Expected("GIVING");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
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
                        ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier or numeric literal");
                        ErrorHandler.Parser.PrettyError(fileName, Current());
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
                    ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else
            {
                ErrorHandler.Parser.Report(fileName, Current(), "expected", "BY or INTO");
                ErrorHandler.Parser.PrettyError(fileName, Current());
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
            while (Current().type == TokenType.Identifier)
                Identifier();

            if (!CurrentEquals("."))
            {
                string notIdentifierError = """
                    The FREE statement must only contain based data item identifiers.
                    """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", notIdentifierError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
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
                while (Current().type == TokenType.Identifier)
                    Identifier();

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
                string notProgramNameError = """
                The CLOSE statement only accepts file connector names. 
                NOTE: This statement must not specify more than one file connector when inside of an exception-checking phrase in a PERFORM statement.
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", notProgramNameError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }

            while (Current().type == TokenType.Identifier)
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

            if (!CurrentEquals("."))
            {
                string notProgramNameError = """
                The CLOSE statement only accepts file connector names. 
                NOTE: This statement must not specify more than one file connector when inside of an exception-checking phrase in a PERFORM statement.
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", notProgramNameError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
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
                string notProgramNameError = """
                The CANCEL statement only accepts Alphanumeric or National literals and data items, or a program prototype name specified in the REPOSITORY paragraph.
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", notProgramNameError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
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
                string notProgramNameError = """
                The CANCEL statement only accepts Alphanumeric or National literals and data items, or a program prototype name specified in the REPOSITORY paragraph.
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", notProgramNameError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
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
                string notIdentifierError = """
                The TERMINATE statement must only contain report entry identifiers defined in the report section.
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", notIdentifierError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }
            Identifier();
            while (Current().type == TokenType.Identifier)
                Identifier();

            if (!CurrentEquals("."))
            {
                string notIdentifierError = """
                The TERMINATE statement must only contain report entry identifiers defined in the report section.
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", notIdentifierError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }
        }

        void UNLOCK()
        {
            Expected("UNLOCK");
            Identifier();
            Choice("RECORD", "RECORDS");
        }

        void VALIDATE()
        {
            Expected("VALIDATE");
            if (Current().type != TokenType.Identifier)
            {
                string notIdentifierError = """
                The VALIDATE statement must only contain data item identifiers.
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", notIdentifierError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }
            Identifier();
            while (Current().type == TokenType.Identifier)
                Identifier();

            if (!CurrentEquals("."))
            {
                string notIdentifierError = """
                The VALIDATE statement must only contain data item identifiers.
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", notIdentifierError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }
        }


        // The following methods are responsible for parsing some commonly repeated pieces of COBOL statements.
        // The ON SIZE ERROR, ON EXCEPTION, INVALID KEY, AT END, and the RETRY phrase are examples of pieces of COBOL syntax
        // that appear on multiple statements. Reusing the same code in those cases keeps things much more modular and easier to maintain.
        //
        // The Arithmetic() and Condition() methods are responsible for parsing expressions and verifying if those expressions were
        // written correctly. This is using a combination of the Shunting Yard algorithm, and some methods to verify if the 
        // parentheses are balanced and if it can be evaluated correctly.
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

            Arithmetic();
            if (CurrentEquals("SECONDS") || hasFor)
                Expected("SECONDS");

            else
                Expected("TIMES");
        }

        void InvalidKey(ref bool isConditional, bool invalidKeyExists = false, bool notInvalidKeyExists = false)
        {
            if (CurrentEquals("INVALID"))
            {
                if (invalidKeyExists)
                {
                    const string onInvalidExistsError = """
                    INVALID KEY can only be specified once in this statement. 
                    The same applies to the NOT INVALID KEY.
                    """;
                    ErrorHandler.Parser.Report(fileName, Current(), "general", onInvalidExistsError);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }
                isConditional = true;
                invalidKeyExists = true;
                Expected("INVALID");
                Optional("KEY");
                Statement(true);
                InvalidKey(ref isConditional, invalidKeyExists, notInvalidKeyExists);

            }

            if (CurrentEquals("NOT"))
            {
                if (notInvalidKeyExists)
                {
                    const string notOnInvalidExistsError = """
                    NOT INVALID KEY can only be specified once in this statement. 
                    The same applies to the INVALID KEY.
                    """;
                    ErrorHandler.Parser.Report(fileName, Current(), "general", notOnInvalidExistsError);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }
                isConditional = true;
                notInvalidKeyExists = true;
                Expected("NOT");
                Expected("INVALID");
                Optional("KEY");
                Statement(true);
                InvalidKey(ref isConditional, invalidKeyExists, notInvalidKeyExists);
            }
        }

        void OnException(ref bool isConditional, bool onExceptionExists = false, bool notOnExceptionExists = false)
        {
            if (CurrentEquals("ON") || CurrentEquals("EXCEPTION"))
            {
                if (onExceptionExists)
                {
                    const string onExceptionExistsError = """
                    ON EXCEPTION can only be specified once in this statement. 
                    The same applies to the NOT ON EXCEPTION.
                    """;
                    ErrorHandler.Parser.Report(fileName, Current(), "general", onExceptionExistsError);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }
                isConditional = true;
                onExceptionExists = true;
                Optional("ON");
                Expected("EXCEPTION");
                Statement(true);
                OnException(ref isConditional, onExceptionExists, notOnExceptionExists);

            }

            if (CurrentEquals("NOT"))
            {
                if (notOnExceptionExists)
                {
                    const string notOnExceptionExistsError = """
                    NOT ON EXCEPTION can only be specified once in this statement. 
                    The same applies to the ON EXCEPTION.
                    """;
                    ErrorHandler.Parser.Report(fileName, Current(), "general", notOnExceptionExistsError);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }
                isConditional = true;
                notOnExceptionExists = true;
                Expected("NOT");
                Optional("ON");
                Expected("EXCEPTION");
                Statement(true);
                OnException(ref isConditional, onExceptionExists, notOnExceptionExists);
            }
        }

        void RaisingStatus(bool raisingExists = false, bool statusExists = false)
        {
            if (CurrentEquals("RAISING"))
            {
                if (raisingExists)
                {
                    const string raisingExistsError = """
                    RAISING can only be specified once in this statement. 
                    The same applies to the WITH NORMAL/ERROR STATUS.
                    """;
                    ErrorHandler.Parser.Report(fileName, Current(), "general", raisingExistsError);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
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
                    const string statusExistsError = """
                    WITH NORMAL/ERROR STATUS can only be specified once in this statement. 
                    The same applies to the RAISING.
                    """;
                    ErrorHandler.Parser.Report(fileName, Current(), "general", statusExistsError);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
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
                    const string atEndExistsError = """
                    AT END can only be specified once in this statement. 
                    The same applies to the NOT AT END.
                    """;
                    ErrorHandler.Parser.Report(fileName, Current(), "general", atEndExistsError);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }
                isConditional = true;
                atEndExists = true;
                Optional("AT");
                Expected("END");
                Statement(true);
                AtEnd(ref isConditional, atEndExists, notAtEndExists);

            }

            if (CurrentEquals("NOT"))
            {
                if (notAtEndExists)
                {
                    const string notAtEndExistsError = """
                    NOT AT END can only be specified once in this statement. 
                    The same applies to the AT END.
                    """;
                    ErrorHandler.Parser.Report(fileName, Current(), "general", notAtEndExistsError);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }
                isConditional = true;
                notAtEndExists = true;
                Expected("NOT");
                Optional("AT");
                Expected("END");
                Statement(true);
                AtEnd(ref isConditional, atEndExists, notAtEndExists);
            }
        }

        void SizeError(ref bool isConditional, bool onErrorExists = false, bool notOnErrorExists = false)
        {
            if (CurrentEquals("ON") || CurrentEquals("SIZE"))
            {
                if (onErrorExists)
                {
                    const string onErrorExistsError = """
                    ON SIZE ERROR can only be specified once in this statement. 
                    The same applies to NOT ON SIZE ERROR.
                    """;
                    ErrorHandler.Parser.Report(fileName, Current(), "general", onErrorExistsError);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }
                isConditional = true;
                onErrorExists = true;
                Optional("ON");
                Expected("SIZE");
                Expected("ERROR");
                Statement(true);
                SizeError(ref isConditional, onErrorExists, notOnErrorExists);

            }

            if (CurrentEquals("NOT"))
            {
                if (notOnErrorExists)
                {
                    const string notOnErrorExistsError = """
                    NOT ON SIZE ERROR can only be specified once in this statement. 
                    The same applies to ON SIZE ERROR.
                    """;
                    ErrorHandler.Parser.Report(fileName, Current(), "general", notOnErrorExistsError);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }
                isConditional = true;
                notOnErrorExists = true;
                Expected("NOT");
                Optional("ON");
                Expected("SIZE");
                Expected("ERROR");
                Statement(true);
                SizeError(ref isConditional, onErrorExists, notOnErrorExists);
            }
        }

        void Arithmetic()
        {
            bool IsArithmeticSymbol(Token current) => current.value switch
            {
                "+" => true,
                "-" => true,
                "*" => true,
                "/" => true,
                "**" => true,
                "(" => true,
                ")" => true,
                _ => false
            };

            while (Current().type is TokenType.Identifier or TokenType.Numeric or TokenType.Symbol)
            {
                if (Current().type == TokenType.Identifier)
                    Identifier();

                if (Current().type == TokenType.Numeric)
                    Number();

                if (IsArithmeticSymbol(Current()))
                {
                    if (IsArithmeticSymbol(Lookahead(-1)))
                    {
                        string invalidArithmeticSymbol = """
                        Invalid token after an arithmetic operator, expected a numeric literal or identifier instead of another arithmetic operator
                        """;

                        ErrorHandler.Parser.Report(fileName, Current(), "general", invalidArithmeticSymbol);
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                    }

                    if (Lookahead(1).type != TokenType.Numeric && Lookahead(1).type != TokenType.Identifier)
                    {
                        string invalidArithmeticSymbol = """
                        Invalid arithmetic expression, expected a numeric literal or identifier after this operator.
                        Arithmetic expressions cannot end with an operator
                        """;

                        ErrorHandler.Parser.Report(fileName, Current(), "general", invalidArithmeticSymbol);
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                    }

                    Symbol();
                }

                if (CurrentEquals("."))
                    return;

                if (Current().type == TokenType.Symbol && !IsArithmeticSymbol(Current()))
                {
                    const string invalidArithmeticSymbol = """
                    Invalid symbol in this arithmetic expression. Valid operators are: +, -, *, /, **, ( and )
                    """;

                    ErrorHandler.Parser.Report(fileName, Current(), "general", invalidArithmeticSymbol);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }
            }
        }

        void Condition(string delimiter)
        {
            var current = Current();
            var expression = new List<Token>();
            while (Current().context != TokenContext.IsStatement && !CurrentEquals(delimiter))
            {
                if (CurrentEquals("NOT") && (LookaheadEquals(1, ">") || LookaheadEquals(1, "<")))
                {
                    var combined = new Token($"{Current().value} {Lookahead(1).value}", TokenType.Symbol, Current().line, Current().column);
                    expression.Add(combined);
                    Analyzed.Add(combined);
                    Continue();
                    Continue();
                }
                else
                {
                    expression.Add(Current());
                    Expected(Current().value);
                }
            }

            if(!Helpers.IsBalanced(expression))
            {
                const string expressionNotBalancedError = """
                This expression is not balanced, one or more parenthesis to not have their matching opening or closing pair, it is an invalid expression
                """;

                ErrorHandler.Parser.Report(fileName, expression[0], "general", expressionNotBalancedError);
                ErrorHandler.Parser.PrettyError(fileName, expression[0]);
            }

            var shuntingYard = Helpers.ShuntingYard(expression, Helpers.BooleanPrecedence);
            
            if (!Helpers.EvaluatePostfix(shuntingYard, Helpers.BooleanPrecedence, out Token error))
            {
                const string expressionNotValidError = """
                This expression cannot be correctly evaluated. Please make sure that all operators have their matching operands.
                """;

                ErrorHandler.Parser.Report(fileName, error, "general", expressionNotValidError);
                ErrorHandler.Parser.PrettyError(fileName, error);
            }
        }

        bool NotIdentifierOrLiteral()
        {
            return Current().type is not TokenType.Identifier and not TokenType.Numeric and not TokenType.String;
        }

    }
    

    // Parser Helper methods.
    // These are the main methods used to interact with and iterate through the List of Tokens.
    // All other methods inside of the parser depend on these to parse through the tokens.

    /// <summary>
    /// Token <c>Lookahead</c>: This method returns a Token from an index of Current Index + the amount parameter
    /// <para>When passed a positive amount it will act as a lookahead method, and when passed a negative amount it will act as a lookbehind method</para>
    /// <para>Technically this method allows for infinite lookahead and lookbehind, as long as the Index + amount is not bigger than the
    /// number of items on the list of Tokens or smaller than 0.</para>
    /// </summary>
    private static Token Lookahead(int amount)
    {
        if (Index + amount >= TokenList.Count)
        {
            return TokenList[TokenList.Count - 1];
        }

        return Index + amount < 0 ? TokenList[0] : TokenList[Index + amount];
    }

    /// <summary>
    /// Boolean <c>LookaheadEquals</c>: This method returns true or false depending on if the Token from an index of Current Index + the first parameter is equal to the second paramenter
    /// <para>When passed a positive amount it will act as a lookahead comparison method, and when passed a negative amount it will act as a lookbehind comparison method</para>
    /// <para>Technically this method allows for infinite lookahead and lookbehind comparisons, as long as the Index + amount is not bigger than the
    /// number of items on the list of Tokens or smaller than 0.</para>
    /// </summary>
    private static bool LookaheadEquals(int lookahead, string stringToCompare)
    {
        return Lookahead(lookahead).value.Equals(stringToCompare);
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
    /// Boolean <c>CurrentEquals</c>: This method returns true or false depending on if the current token from the current Index has the same value as the parameter.
    /// <para>This helper method is an alternative to the <c>"Current().value.Equals()"</c> syntax, which could become verbose and harder to read when it's used frequently</para>
    /// </summary>
    private static bool CurrentEquals(string stringToCompare)
    {
        return Current().value.Equals(stringToCompare);
    }

    /// <summary>
    /// Void <c>Continue</c>: This method adds +1 to the current index, moving the index to the next token.
    /// <para>This method will be called in the Expected(), Optional(), Choice(), Identifier(), Number() and String() methods,
    /// so there's no need to call Continue after calling those methods.</para>
    /// </summary>
    private static void Continue()
    {
        Index += 1;
    }

    /// <summary>
    /// Void <c>Choice</c>: This method checks if the current token matches one of the values passed in its parameters.
    /// <para>If the current token matches one the values, it adds the correct value to the parsed list,
    /// if the current token doesn't match any of the values it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void Choice(params string[] choices)
    {
        Token token = Current();
        foreach (string choice in choices)
        {
            if (token.value.Equals(choice))
            {
                Analyzed.Add(token);
                Continue();
                return;
            }
        }

        ErrorHandler.Parser.Report(FileName, token, "choice", choices);
        ErrorHandler.Parser.PrettyError(FileName, token);
        Analyzed.Add(token);
        Continue();
    }
    
    /// <summary>
    /// Void <c>Optional</c>: This method checks if the current token is equal to it's first parameter.
    /// <para>If the current token matches the value, it adds the token to the parsed list,
    /// if the current token doesn't match the value it ignores the token and returns without moving to the next token</para>
    /// </summary>
    private static void Optional(string optional, string scope = "")
    {
        var token = Current();
        if (!token.value.Equals(optional))
            return;

        Analyzed.Add(token);
        Continue();
    }

    /// <summary>
    /// Void <c>Expected</c>: This method checks if the current token is equal to it's first parameter.
    /// <para>If the current token matches the value, it adds the token to the parsed list,
    /// if the current token doesn't match the value it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void Expected(string expected, string custom = "default", int position = 0, string scope = "")
    {
        var errorMessage = expected;
        var errorType = "expected";
        var token = Current();
        var lookahead = Lookahead(position);
        if (!custom.Equals("default"))
        {
            errorMessage = custom;
            errorType = "general";
        }

        if (!token.value.Equals(expected))
        {
            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);
            Analyzed.Add(token);
            Continue();
            return;
        }

        Analyzed.Add(token);
        Continue();
    }

    /// <summary>
    /// Void <c>Identifier</c>: This method checks if the current token is an identifier.
    /// <para>If the current token's type is TokenType.Identifier, it adds the token to the parsed list,
    /// if the current token's type is TokenType.Identifier it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void Identifier()
    {
        Token token = Current();
        if (token.type is not TokenType.Identifier)
        {
            ErrorHandler.Parser.Report(FileName, token, "expected", "identifier");
            ErrorHandler.Parser.PrettyError(FileName, token);
            Continue();
            return;
        }
        Analyzed.Add(token);
        Continue();
        return;
    }

    /// <summary>
    /// Void <c>Number</c>: This method checks if the current token is a Number.
    /// <para>If the current token's type is TokenType.Numeric, it adds the token to the parsed list,
    /// if the current token's type is TokenType.Numeric it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void Number(string custom = "expected", int position = 0)
    {
        var errorMessage = "string literal";
        var errorType = "expected";
        var token = Current();
        var lookahead = Lookahead(position);
        if (!custom.Equals("expected"))
        {
            errorMessage = custom;
            errorType = "general";
        }

        if (token.type is not TokenType.Numeric)
        {
            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);
            Continue();
            return;
        }
        Analyzed.Add(token);
        Continue();
        return;
    }

    /// <summary>
    /// Void <c>String</c>: This method checks if the current token is a National, Alphanumeric, Alphabetic or Boolean.
    /// <para>If the current token's type is TokenType.String, it adds the token to the parsed list,
    /// if the current token's type is TokenType.String it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void String(string custom = "expected", int position = 0)
    {
        var errorMessage = "string literal";
        var errorType = "expected";
        var token = Current();
        var lookahead = Lookahead(position);
        if (!custom.Equals("expected"))
        {
            errorMessage = custom;
            errorType = "general";
        }

        if (token.type is not TokenType.String)
        {
            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);
            Continue();
            return;
        }
        Analyzed.Add(token);
        Continue();
        return;
    }

    /// <summary>
    /// Void <c>FigurativeLiteral</c>: This method checks if the current token is a Figurative Literal.
    /// <para>If the current token's type is TokenType.FigurativeLiteral, it adds the token to the parsed list,
    /// if the current token's type is TokenType.FigurativeLiteral it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void FigurativeLiteral()
    {
        var current = Current();
        if (current.type is not TokenType.FigurativeLiteral)
        {
            ErrorHandler.Parser.Report(FileName, Current(), "expected", "figurative literal");
            ErrorHandler.Parser.PrettyError(FileName, Current());
            Continue();
            return;
        }
        Analyzed.Add(current);
        Continue();
        return;
    }

    /// <summary>
    /// Void <c>Symbol</c>: This method checks if the current token is a valid COBOl Symbol.
    /// <para>If the current token's type is TokenType.Symbol, it adds the token to the parsed list,
    /// if the current token's type is TokenType.Symbol it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void Symbol(string custom = "expected", int position = 0)
    {
        var errorMessage = "string literal";
        var errorType = "expected";
        var token = Current();
        var lookahead = Lookahead(position);
        if (!custom.Equals("expected"))
        {
            errorMessage = custom;
            errorType = "general";
        }

        if (position != 0)
            token = Lookahead(position);

        if (token.type is not TokenType.Symbol)
        {
            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);
            Continue();
            return;
        }
        Analyzed.Add(token);
        Continue();
        return;
    }

}
