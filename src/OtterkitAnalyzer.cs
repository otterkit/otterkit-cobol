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
    /// Otterkit COBOL Syntax Analyzer
    /// <para>This parser was built to be easily extensible, with some reusable COBOL parts.</para>
    /// <para>It requires a List of Tokens generated from the Lexer and the Token Classifier.</para>
    /// </summary>
    public static List<Token> Analyze(List<Token> tokenList, string fileName)
    {
        FileName = fileName;
        TokenList = tokenList;
        Index = 0;

        // Call the parser's main method
        // This should only return when the parser reaches the EOF token
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

            PROCEDURE();

            if (CurrentEquals("IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID"))
            {
                Source();
            }
            
            if (CurrentEquals("EOF")) return;

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

            if (!CurrentEquals("PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "METHOD-ID", "INTERFACE-ID"))
            {
                Expected("PROGRAM-ID", """
                Missing source unit ID name (PROGRAM-ID, FUNCTION-ID, CLASS-ID...), the identification division header is optional but every source unit must still have an ID.
                """, 0, "OPTIONS", "ENVIRONMENT", "DATA", "PROCEDURE");
            }

            if (CurrentEquals("PROGRAM-ID"))
                ProgramId();

            if (CurrentEquals("FUNCTION-ID"))
                FunctionId();

            if (CurrentEquals("CLASS-ID"))
                ClassId();

            if (CurrentEquals("INTERFACE-ID"))
                InterfaceId();

            if (CurrentEquals("METHOD-ID"))
                MethodId();
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
            SourceId = ProgramIdentifier.value;
            SourceType = "PROGRAM";
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
                        isPrototype = true;
                    }
                }

                if (isPrototype && (isCommon || isInitial || isRecursive))
                {
                    ErrorHandler.Parser.Report(fileName, ProgramIdentifier, ErrorType.General, """
                    Invalid prototype. Program prototypes cannot be defined as common, initial or recursive.
                    """);
                    ErrorHandler.Parser.PrettyError(fileName, ProgramIdentifier);
                }

                if (isInitial && isRecursive)
                {
                    ErrorHandler.Parser.Report(fileName, ProgramIdentifier, ErrorType.General, """
                    Invalid program definition. Initial programs cannot be defined as recursive.
                    """);
                    ErrorHandler.Parser.PrettyError(fileName, ProgramIdentifier);
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
            SourceId = Current().value;
            SourceType = "FUNCTION";
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
            }

            Expected(".", """
            Missing separator period at the end of this function definition
            """, -1, "OPTION", "ENVIRONMENT", "DATA", "PROCEDURE");
        }

        void ClassId()
        {
            Expected("CLASS-ID");
            Expected(".");
            SourceId = Current().value;
            SourceType = "CLASS";
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                    The INHERITS FROM clause must contain at least one class or object name.
                    """);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }

                Identifier();
                while (CurrentEquals(TokenType.Identifier)) Identifier();
            }

            if (CurrentEquals("USING"))
            {
                Expected("USING");
                if (!CurrentEquals(TokenType.Identifier))
                {
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                    The USING clause must contain at least one parameter.
                    """);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
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
            SourceId = Current().value;
            SourceType = "INTERFACE";
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                    The INHERITS FROM clause must contain at least one class or object name.
                    """);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }

                Identifier();
                while (CurrentEquals(TokenType.Identifier)) Identifier();
            }

            if (CurrentEquals("USING"))
            {
                Expected("USING");
                if (!CurrentEquals(TokenType.Identifier))
                {
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                    The USING clause must contain at least one parameter.
                    """);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
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
            SourceType = "METHOD";

            if (CurrentEquals(TokenType.Identifier))
            {
                SourceId = Current().value;
                Identifier();
                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    String();
                }
            }
            else if (CurrentEquals("GET"))
            {
                Expected("GET");
                Expected("PROPERTY");
                SourceId = $"GET {Current().value}";
                Identifier();
            }
            else if (CurrentEquals("SET"))
            {
                Expected("SET");
                Expected("PROPERTY");
                SourceId = $"SET {Current().value}";
                Identifier();
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
            Expected(".", headerPeriodError, -1);
        }


        // Method responsible for parsing the DATA DIVISION.
        // That includes the FILE, WORKING-STORAGE, LOCAL-STORAGE, LINKAGE, REPORT and SCREEN sections.
        // It is also responsible for showing appropriate error messages when an error occurs in the DATA DIVISION.
        void DATA()
        {
            Expected("DATA", "data division");
            Expected("DIVISION");
            Expected(".", """
            Missing separator period at the end of this DATA DIVISION header, every division header must end with a separator period
            """, -1, "WORKING-STORAGE", "LOCAL-STORAGE" , "LINKAGE", "PROCEDURE");

            if (CurrentEquals("WORKING-STORAGE"))
                WorkingStorage();

            if (CurrentEquals("LOCAL-STORAGE"))
                LocalStorage();

            if (CurrentEquals("LINKAGE"))
                LinkageSection();

            if (!CurrentEquals("PROCEDURE"))
            {
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "Data Division data items and sections");
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
            _ = int.TryParse(Current().value, out int OutInt);
            while (OutInt > 1 && OutInt < 50)
            {
                BaseEntry();
                _ = int.TryParse(Current().value, out OutInt);
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

                ErrorHandler.Parser.Report(fileName, Lookahead(-1), ErrorType.General, $"""
                A data item with this name already exists in this program, data items in a program must have a unique name.
                The original {originalItem.Identifier} data item can be found at line {originalItem.Line}. 
                """);
                ErrorHandler.Parser.PrettyError(fileName, Lookahead(-1));
            }

            DataItemInformation.AddSection(DataItemHash, CurrentSection);

            if (!CurrentEquals(TokenContext.IsClause) && !CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, $"""
                Expected data division clauses or a separator period after this data item's identifier.
                Token found ("{Current().value}") was not a data division clause reserved word.
                """);
                ErrorHandler.Parser.PrettyError(fileName, Current());
                Continue();
            }

            while (CurrentEquals(TokenContext.IsClause))
            {
                if (CurrentEquals("IS") && !LookaheadEquals(1, "EXTERNAL", "GLOBAL", "TYPEDEF"))
                {
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                    Missing clause or possible clause mismatch, in this context the "IS" word must be followed by the EXTERNAL, GLOBAL or TYPEDEF clauses only (IS TYPEDEF), or must be in the middle of the PICTURE clause (PIC IS ...) 
                    """);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }

                if ((CurrentEquals("IS") && LookaheadEquals(1, "EXTERNAL")) || CurrentEquals("EXTERNAL"))
                {
                    Optional("IS");
                    Expected("EXTERNAL");
                    if (CurrentEquals("AS"))
                    {
                        Expected("AS");
                        DataItemInformation.IsExternal(DataItemHash, true, Current().value);

                        String("""
                        Missing externalized name, the "AS" word on the EXTERNAL clause must be followed by an alphanumeric or national literal
                        """, -1);
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
                        ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                        Unrecognized type, PICTURE type must be S9, 9, X, A, N or 1. These are Signed Numeric, Unsigned Numeric, Alphanumeric, Alphabetic, National and Boolean respectively
                        """);
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                    }

                    DataItemInformation.AddType(DataItemHash, dataType);
                    DataItemInformation.IsElementary(DataItemHash, true);
                    Choice("S9", "9", "X", "A", "N", "1");

                    Expected("(");
                    string DataLength = Current().value;
                    Number();
                    Expected(")");
                    if (CurrentEquals("V9") && (dataType != "S9" && (dataType != "9")))
                    {
                        ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, "V9 cannot be used with non-numeric types");
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
                        ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                        The only tokens allowed after a VALUE clause are type literals, like an Alphanumeric literal ("Hello, World!") or a Numeric literal (123.456).
                        """);
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
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                Invalid level number for this data item, CONSTANT data items must have a level number of 1 or 01
                """);
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

                ErrorHandler.Parser.Report(fileName, Lookahead(-1), ErrorType.General, $"""
                A data item with this name already exists in this program, data items in a program must have a unique name.
                The original {originalItem.Identifier} data item can be found at line {originalItem.Line}. 
                """);
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
            Expected("PROCEDURE");
            Expected("DIVISION");
            if (SourceType.Equals("FUNCTION"))
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

            Statement();

            if (CurrentEquals("IDENTIFICATION") || CurrentEquals("PROGRAM-ID") || CurrentEquals("FUNCTION-ID"))
            {
                var missingEndMarkerError = $"""
                Missing END {SourceType} marker. If another source unit is present after the end of the current source unit, the current unit must contain an END marker.
                """;

                const string missingEndFunctionMarkerError = $"""
                Missing END FUNCTION marker. User-defined functions must always end with an END FUNCTION marker.
                """;

                string errorMessageChoice = SourceType.Equals("FUNCTION") ? missingEndFunctionMarkerError : missingEndMarkerError;

                ErrorHandler.Parser.Report(fileName, Lookahead(-1), ErrorType.General, errorMessageChoice);
                ErrorHandler.Parser.PrettyError(fileName, Lookahead(-1));
                return;
            }

            if (SourceType.Equals("PROGRAM") && CurrentEquals("END") && LookaheadEquals(1, "PROGRAM"))
            {
                Expected("END");
                Expected("PROGRAM");
                Identifier();
                Expected(".", """
                Missing separator period at the end of this END PROGRAM definition
                """, -1, "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID");
            }

            if (SourceType.Equals("FUNCTION"))
            {
                Expected("END");
                Expected("FUNCTION");
                Identifier();
                Expected(".", """
                Missing separator period at the end of this END FUNCTION definition
                """, -1, "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID");
            }
        }

        // This method is part of the PROCEDURE DIVISION parsing. It's used to parse the "RETURNING" data item specified in
        // the PROCEDURE DIVISION header. It's separate from the previous method because its code is needed more than once.
        // COBOL user-defined functions should always return a data item.
        void ReturningDataName()
        {
            if (!CurrentEquals(TokenType.Identifier))
            {
                ErrorHandler.Parser.Report(fileName, Lookahead(-1), ErrorType.General, """
                Missing returning data item after this RETURNING definition.
                """);
                ErrorHandler.Parser.PrettyError(fileName, Lookahead(-1));
                return;
            }

            var DataName = Current().value;
            Identifier();

            var DataItemHash = $"{SourceId}#{DataName}";
            if (!DataItemInformation.ValueExists(DataItemHash))
            {
                ErrorHandler.Parser.Report(fileName, Lookahead(-1), ErrorType.General, """
                No data item found with this name in this source unit's data division. 
                Please define a new returning data item in this unit's linkage section.
                """);
                ErrorHandler.Parser.PrettyError(fileName, Lookahead(-1));
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier or literal");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                    break;
            }

            while (CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String))
            {
                if (CurrentEquals(TokenType.Identifier))
                    Identifier();

                if (CurrentEquals(TokenType.Numeric))
                    Number();

                if (CurrentEquals(TokenType.String))
                    String();
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
            if (CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, "CHARACTERS") && !LookaheadEquals(1, TokenType.Symbol))
                Identifier();

            if (CurrentEquals(TokenType.Identifier, TokenType.Numeric))
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
            if (!CurrentEquals(TokenType.Identifier))
            {
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier");
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }

            while (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }

            Expected("=");
            if (!CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String))
            {
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier, numeric literal or valid arithmetic symbol");
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
            if (!CurrentEquals(TokenType.Identifier, TokenType.Numeric))
            {
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                ErrorHandler.Parser.PrettyError(fileName, Current());
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
                        ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                        break;
                }

                Expected("GIVING");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier");
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier");
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else
            {
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "TO or GIVING");
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
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier or numeric literal");
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
                        ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                        break;
                }

                Expected("GIVING");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier");
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else
            {
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "FROM");
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
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                Unsupported phrase: NEXT SENTENCE is an archaic feature. This phrase can be confusing and is a common source of errors.
                The CONTINUE statement can be used to accomplish the same functionality while being much clearer and less prone to error
                """);
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
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                The INITIATE statement must only contain report entry identifiers defined in the report section.
                """);
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }
            Identifier();
            while (Current().type == TokenType.Identifier)
                Identifier();

            if (!CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                The INITIATE statement must only contain report entry identifiers defined in the report section.
                """);
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier or numeric literal");
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
                        ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                        break;
                }

                Expected("GIVING");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier");
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else
            {
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "BY");
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
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                The MOVE statement must only contain a single data item identifier, datatype literal or an intrisic function which returns a data item before the "TO" reserved word.
                """);
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
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                The MOVE statement must only contain data item identifiers after the "TO" reserved word.
                """);
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }

            while (Current().type == TokenType.Identifier)
                Identifier();

            if (!CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                The MOVE statement must only contain data item identifiers after the "TO" reserved word.
                """);
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier or numeric literal");
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
                        ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                        break;
                }

                Expected("GIVING");
                if (Current().type != TokenType.Identifier)
                {
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier");
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
                        ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier or numeric literal");
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "identifier");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }

                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else
            {
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.Expected, "BY or INTO");
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
            while (CurrentEquals(TokenType.Identifier)) Identifier();

            if (!CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                The FREE statement must only contain based data item identifiers.
                """);
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
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                The CLOSE statement only accepts file connector names. 
                NOTE: This statement must not specify more than one file connector when inside of an exception-checking phrase in a PERFORM statement.
                """);
                ErrorHandler.Parser.PrettyError(fileName, Current());
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
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                The CLOSE statement only accepts file connector names. 
                NOTE: This statement must not specify more than one file connector when inside of an exception-checking phrase in a PERFORM statement.
                """);
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
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                The CANCEL statement only accepts Alphanumeric or National literals and data items, or a program prototype name specified in the REPOSITORY paragraph.
                """);
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
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                The CANCEL statement only accepts Alphanumeric or National literals and data items, or a program prototype name specified in the REPOSITORY paragraph.
                """);
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
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                The TERMINATE statement must only contain report entry identifiers defined in the report section.
                """);
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }
            Identifier();
            while (Current().type == TokenType.Identifier)
                Identifier();

            if (!CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                The TERMINATE statement must only contain report entry identifiers defined in the report section.
                """);
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
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                The VALIDATE statement must only contain data item identifiers.
                """);
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }
            Identifier();
            while (Current().type == TokenType.Identifier)
                Identifier();

            if (!CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                The VALIDATE statement must only contain data item identifiers.
                """);
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                    INVALID KEY can only be specified once in this statement. 
                    The same applies to the NOT INVALID KEY.
                    """);
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                    NOT INVALID KEY can only be specified once in this statement. 
                    The same applies to the INVALID KEY.
                    """);
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                    ON EXCEPTION can only be specified once in this statement. 
                    The same applies to the NOT ON EXCEPTION.
                    """);
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                    NOT ON EXCEPTION can only be specified once in this statement. 
                    The same applies to the ON EXCEPTION.
                    """);
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                    RAISING can only be specified once in this statement. 
                    The same applies to the WITH NORMAL/ERROR STATUS.
                    """);
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                    WITH NORMAL/ERROR STATUS can only be specified once in this statement. 
                    The same applies to the RAISING.
                    """);
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                    AT END can only be specified once in this statement. 
                    The same applies to the NOT AT END.
                    """);
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                    NOT AT END can only be specified once in this statement. 
                    The same applies to the AT END.
                    """);
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                    ON SIZE ERROR can only be specified once in this statement. 
                    The same applies to NOT ON SIZE ERROR.
                    """);
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
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                    NOT ON SIZE ERROR can only be specified once in this statement. 
                    The same applies to ON SIZE ERROR.
                    """);
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
                        ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                        Invalid token after an arithmetic operator, expected a numeric literal or identifier instead of another arithmetic operator
                        """);
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                    }

                    if (Lookahead(1).type != TokenType.Numeric && Lookahead(1).type != TokenType.Identifier)
                    {
                        ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                        Invalid arithmetic expression, expected a numeric literal or identifier after this operator.
                        Arithmetic expressions cannot end with an operator
                        """);
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                    }

                    Symbol();
                }

                if (CurrentEquals("."))
                    return;

                if (Current().type == TokenType.Symbol && !IsArithmeticSymbol(Current()))
                {
                    ErrorHandler.Parser.Report(fileName, Current(), ErrorType.General, """
                    Invalid symbol in this arithmetic expression. Valid operators are: +, -, *, /, **, ( and )
                    """);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }
            }
        }

        void Condition(string delimiter)
        {
            var expression = new List<Token>();

            while (Current().context != TokenContext.IsStatement && !CurrentEquals(delimiter))
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

            if(!Helpers.IsBalanced(expression))
            {
                ErrorHandler.Parser.Report(fileName, expression[0], ErrorType.General, """
                This expression is not balanced, one or more parenthesis to not have their matching opening or closing pair, it is an invalid expression
                """);
                ErrorHandler.Parser.PrettyError(fileName, expression[0]);
            }

            var shuntingYard = Helpers.ShuntingYard(expression, Helpers.BooleanPrecedence);

            if (!Helpers.EvaluatePostfix(shuntingYard, Helpers.BooleanPrecedence, out Token error))
            {
                ErrorHandler.Parser.Report(fileName, error, ErrorType.General, """
                This expression cannot be correctly evaluated. Please make sure that all operators have their matching operands.
                """);
                ErrorHandler.Parser.PrettyError(fileName, error);
            }
        }

        bool NotIdentifierOrLiteral()
        {
            return !CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String);
        }

    }
    

    // Parser Helper methods.
    // These are the main methods used to interact with and iterate through the List of Tokens.
    // All other methods inside of the parser depend on these to parse through the tokens.

    private static void AnchorPoint(params string[] anchors)
    {
        ErrorHandler.Parser.AttemptRecovery(anchors);

        while(!CurrentEquals(TokenType.EOF))
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

        while(!CurrentEquals(TokenType.EOF))
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
        if (CurrentEquals(TokenType.EOF)) return;

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

        if (!CurrentEquals(expected))
        {
            var lookahead = Lookahead(position);

            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);

            if (wordAnchors.Length != 0) AnchorPoint(wordAnchors);
        }
        else
        {
            Continue();
        }
    }

    private static void Expected(string expected, string custom = "default", int position = 0, TokenContext typeAnchor = TokenContext.IsStatement)
    {
        var errorMessage = expected;
        var errorType = ErrorType.Expected;
        if (!custom.Equals("default"))
        {
            errorMessage = custom;
            errorType = ErrorType.General;
        }

        if (!CurrentEquals(expected))
        {
            var lookahead = Lookahead(position);

            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);

            AnchorPoint(typeAnchor);
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
    private static void Identifier(string custom = "default", int position = 0)
    {
        var errorMessage = "Identifier";
        var errorType = ErrorType.Expected;
        if (!custom.Equals("default"))
        {
            errorMessage = custom;
            errorType = ErrorType.General;
        }

        if (!CurrentEquals(TokenType.Identifier))
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
