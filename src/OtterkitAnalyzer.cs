namespace Otterkit;

public static class Analyzer
{
    public static List<Token> Analyze(List<Token> tokenList, string fileName)
    {
        List<Token> analyzed = new();
        string FileName = fileName;
        string SourceId = string.Empty;
        string CurrentSection = string.Empty;

        int index = 0;

        Source();
        return analyzed;

        void Source()
        {
            if (CurrentEquals("EOF"))
            {
                analyzed.Add(Current());
                return;
            }

            switch (Current().value)
            {
                case "IDENTIFICATION":
                    IDENTIFICATION();
                    Source();
                    break;

                case "ENVIRONMENT":
                    ENVIRONMENT();
                    Source();
                    break;

                case "DATA":
                    DATA();
                    Source();
                    break;

                case "PROCEDURE":
                    PROCEDURE();
                    Source();
                    break;

                default:
                    ErrorHandler.Parser.Report(fileName, Current(), "expected", "IDENTIFICATION, ENVIRONMENT, DATA or PROCEDURE");
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                    break;
            }
        }

        void IDENTIFICATION()
        {
            string headerPeriodError = """
            Missing separator period at the end of this IDENTIFICATION DIVISION header, every division header must end with a separator period
            """;

            Expected("IDENTIFICATION", "identification division");
            Expected("DIVISION");
            Expected(".", headerPeriodError, -1, "separator period");
            ProgramId();
        }

        void ProgramId()
        {
            Expected("PROGRAM-ID", "program definition");
            Expected(".", "separator period");
            SourceId = Current().value;
            Identifier();
            Expected(".", "separator period");
        }

        void ENVIRONMENT()
        {
            string headerPeriodError = """
            Missing separator period at the end of this ENVIRONMENT DIVISION header, every division header must end with a separator period
            """;

            Expected("ENVIRONMENT", "environment division");
            Expected("DIVISION");
            Expected(".", headerPeriodError, -1, "separator period");
        }

        void DATA()
        {
            string headerPeriodError = """
            Missing separator period at the end of this DATA DIVISION header, every division header must end with a separator period
            """;

            Expected("DATA", "data division");
            Expected("DIVISION");
            Expected(".", headerPeriodError, -1, "separator period");
            DataSections();
        }

        void DataSections()
        {
            while (!CurrentEquals("PROCEDURE"))
            {
                if (CurrentEquals("WORKING-STORAGE"))
                    WorkingStorage();

                if (CurrentEquals("LOCAL-STORAGE"))
                    LocalStorage();

                switch (Current().value)
                {
                    case "WORKING-STORAGE":
                    case "LOCAL-STORAGE":
                    case "PROCEDURE":
                        break;

                    default:
                        ErrorHandler.Parser.Report(fileName, Current(), "expected", "Data Division data items and sections");
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                        break;
                }
            }
        }

        void WorkingStorage()
        {
            CurrentSection = Current().value;
            Expected("WORKING-STORAGE", "working-storage section");
            Expected("SECTION");
            Expected(".", "separator period");
            while (Current().type == TokenType.Numeric)
                Entries();
        }

        void LocalStorage()
        {
            CurrentSection = Current().value;
            Expected("LOCAL-STORAGE", "local-storage section");
            Expected("SECTION");
            Expected(".", "separator period");
            while (Current().type == TokenType.Numeric)
                Entries();
        }

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

            if (Current().context != TokenContext.IsClause && !CurrentEquals("."))
            {
                string notAClauseError = $"""
                Expected data division clauses or a separator period after this data item's identifier.
                Token found ("{Current().value}") was not a data division clause reserved word.
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", notAClauseError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }

            while (Current().context == TokenContext.IsClause)
            {
                if (CurrentEquals("IS") && !(LookaheadEquals(1, "EXTERNAL") || LookaheadEquals(1, "GLOBAL") || LookaheadEquals(1, "TYPEDEF")))
                {
                    string Externalerror = """
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
                        string externalizedNameError = """
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
                    Choice(null, "PIC", "PICTURE");
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
                        string dataTypeError = """
                        Unrecognized type, PICTURE type must be S9, 9, X, A, N or 1. These are Signed Numeric, Unsigned Numeric, Alphanumeric, Alphabetic, National and Boolean respectively
                        """;

                        ErrorHandler.Parser.Report(fileName, Current(), "general", dataTypeError);
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                    }

                    DataItemInformation.AddType(DataItemHash, dataType);
                    DataItemInformation.IsElementary(DataItemHash, true);
                    Choice(null, "S9", "9", "X", "A", "N", "1");

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

                    if (!Current().type.Equals(TokenType.String) && !Current().type.Equals(TokenType.Numeric))
                    {
                        string valueError = """
                        The only tokens allowed after a VALUE clause are type literals, like an Alphanumeric literal ("Hello, World!") or a Numeric literal (123.456).
                        """;

                        ErrorHandler.Parser.Report(fileName, Current(), "general", valueError);
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                    }
                    
                    if (Current().type.Equals(TokenType.String))
                    {
                        DataItemInformation.AddDefault(DataItemHash, Current().value);
                        String();
                    }

                    if (Current().type.Equals(TokenType.Numeric))
                    {
                        DataItemInformation.AddDefault(DataItemHash, Current().value);
                        Number();
                    }
                }

            }

            if (!DataItemInformation.GetValue(DataItemHash).IsElementary)
                DataItemInformation.IsGroup(DataItemHash, true);

            string separatorPeriodError = """
            Missing separator period at the end of this data item definition, each data item must end with a separator period
            """;
            Expected(".", separatorPeriodError, -1);
        }

        void ConstantEntry()
        {
            if (!CurrentEquals("01") && !CurrentEquals("1"))
            {
                string levelNumberError = """
                Invalid level number for this data item, CONSTANT data items must have a level number of 1 or 01
                """;

                ErrorHandler.Parser.Report(fileName, Current(), "general", levelNumberError);
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }

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

        void PROCEDURE()
        {
            string headerPeriodError = """
            Missing separator period at the end of this PROCEDURE DIVISION header, every division header must end with a separator period
            """;

            Expected("PROCEDURE");
            Expected("DIVISION");
            Expected(".", headerPeriodError, -1);
            Statement();

            if(CurrentEquals("END") && LookaheadEquals(1, "PROGRAM"))
            {
                string endProgramPeriodError = """
                Missing separator period at the end of this END PROGRAM definition
                """;

                Expected("END");
                Expected("PROGRAM");
                Identifier();
                Expected(".", endProgramPeriodError, -1);
                if (Current().value.Equals("IDENTIFICATION"))
                {
                    Source();
                }
            }
        }

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

                if (CurrentEquals("MULTIPLY"))
                    MULTIPLY();

                if (CurrentEquals("MOVE"))
                    MOVE();

                if (CurrentEquals("FREE"))
                    FREE();

                if (CurrentEquals("SUBTRACT"))
                    SUBTRACT();

                if (CurrentEquals("RAISE"))
                    RAISE();               

                if (CurrentEquals("STOP"))
                    STOP();

                if (CurrentEquals("SUPPRESS"))
                    SUPPRESS();

                if (CurrentEquals("TERMINATE"))
                    TERMINATE();

                if (CurrentEquals("VALIDATE"))
                    VALIDATE();

                ScopeTerminator(isNested);
                Statement(isNested);
            }
        }

        void ScopeTerminator(bool isNested)
        {
            if (isNested)
                return;

            Expected(".", "expected", 0);
        }

        // Statement parsing section:
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
                Choice(TokenType.Device, "STANDARD-OUTPUT", "STANDARD-ERROR");
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
                        Choice(TokenType.Device, "STANDARD-INPUT", "COMMAND-LINE");
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
                Choice(null, "BY", "INTO");
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
                Choice(null, "BY", "INTO");
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
            if(CurrentEquals("EXCEPTION"))
            {
                Expected("EXCEPTION");
                Identifier();
            }
            else
                Identifier();
        }

        void STOP()
        {
            Expected("STOP");
            Expected("RUN");
            if (CurrentEquals("WITH") || CurrentEquals("NORMAL") || CurrentEquals("ERROR"))
            {
                Optional("WITH");
                Choice(null, "NORMAL", "ERROR");
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

        // Parser helper methods.
        Token Lookahead(int amount)
        {
            return tokenList[index + amount];
        }

        bool LookaheadEquals(int lookahead, string stringToCompare)
        {
            return Lookahead(lookahead).value.Equals(stringToCompare);
        }

        Token Current()
        {
            return tokenList[index];
        }

        bool CurrentEquals(string stringToCompare)
        {
            return Current().value.Equals(stringToCompare);
        }

        void Continue()
        {
            index += 1;
            return;
        }

        void Choice(TokenType? type, params string[] choices)
        {
            Token current = Current();
            foreach (string choice in choices)
            {
                if (current.value.Equals(choice))
                {
                    if (type != null)
                        current.type = type;
                    analyzed.Add(current);
                    Continue();
                    return;
                }
            }

            ErrorHandler.Parser.Report(fileName, Current(), "choice", choices);
            ErrorHandler.Parser.PrettyError(fileName, Current());
            Continue();
            return;
        }

        void Optional(string optional, string scope = "")
        {
            Token current = Current();
            if (!current.value.Equals(optional))
                return;

            analyzed.Add(current);
            Continue();
            return;
        }

        void Expected(string expected, string custom = "expected", int position = 0, string scope = "")
        {
            string errorMessage = expected;
            string errorType = "expected";
            Token token = Current();
            if (!custom.Equals("expected"))
            {
                errorMessage = custom;
                errorType = "general";
            }

            if (position != 0)
                token = Lookahead(position);

            Token current = Current();
            if (!current.value.Equals(expected))
            {
                ErrorHandler.Parser.Report(fileName, token, errorType, errorMessage);
                ErrorHandler.Parser.PrettyError(fileName, token);
                Continue();
                return;
            }

            analyzed.Add(current);
            Continue();
            return;
        }

        void SizeError(ref bool isConditional, bool onErrorExists = false, bool notOnErrorExists = false)
        {
            if (CurrentEquals("ON") || CurrentEquals("SIZE"))
            {
                if (onErrorExists)
                {
                    string onErrorExistsError = """
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
                    string notOnErrorExistsError = """
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
            bool isArithmeticSymbol(Token current) => current.value switch
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

            while (Current().type == TokenType.Identifier || Current().type == TokenType.Numeric || Current().type == TokenType.Symbol)
            {
                if (Current().type == TokenType.Identifier)
                    Identifier();

                if (Current().type == TokenType.Numeric)
                    Number();

                if (isArithmeticSymbol(Current()))
                {
                    if (isArithmeticSymbol(Lookahead(-1)))
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

                if (Current().type == TokenType.Symbol && !isArithmeticSymbol(Current()))
                {
                    string invalidArithmeticSymbol = """
                    Invalid symbol in this arithmetic expression. Valid operators are: +, -, *, /, **, ( and )
                    """;

                    ErrorHandler.Parser.Report(fileName, Current(), "general", invalidArithmeticSymbol);
                    ErrorHandler.Parser.PrettyError(fileName, Current());
                }
            }
        }

        void Identifier()
        {
            Token current = Current();
            if (current.type != TokenType.Identifier)
            {
                ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier");
                ErrorHandler.Parser.PrettyError(fileName, Current());
                Continue();
                return;
            }
            analyzed.Add(current);
            Continue();
            return;
        }

        void Number(string custom = "expected", int position = 0)
        {
            string errorMessage = "string literal";
            string errorType = "expected";
            Token token = Current();
            if (!custom.Equals("expected"))
            {
                errorMessage = custom;
                errorType = "general";
            }

            if (position != 0)
                token = Lookahead(position);

            Token current = Current();
            if (current.type != TokenType.Numeric)
            {
                ErrorHandler.Parser.Report(fileName, token, errorType, errorMessage);
                ErrorHandler.Parser.PrettyError(fileName, token);
                Continue();
                return;
            }
            analyzed.Add(current);
            Continue();
            return;
        }

        void String(string custom = "expected", int position = 0)
        {
            string errorMessage = "string literal";
            string errorType = "expected";
            Token token = Current();
            if (!custom.Equals("expected"))
            {
                errorMessage = custom;
                errorType = "general";
            }

            if (position != 0)
                token = Lookahead(position);

            Token current = Current();
            if (current.type != TokenType.String)
            {
                ErrorHandler.Parser.Report(fileName, token, errorType, errorMessage);
                ErrorHandler.Parser.PrettyError(fileName, token);
                Continue();
                return;
            }
            analyzed.Add(current);
            Continue();
            return;
        }

        void FigurativeLiteral()
        {
            Token current = Current();
            if (current.type != TokenType.FigurativeLiteral)
            {
                ErrorHandler.Parser.Report(fileName, Current(), "expected", "figurative literal");
                ErrorHandler.Parser.PrettyError(fileName, Current());
                Continue();
                return;
            }
            analyzed.Add(current);
            Continue();
            return;
        }

        void Symbol(string custom = "expected", int position = 0)
        {
            string errorMessage = "string literal";
            string errorType = "expected";
            Token token = Current();
            if (!custom.Equals("expected"))
            {
                errorMessage = custom;
                errorType = "general";
            }

            if (position != 0)
                token = Lookahead(position);

            Token current = Current();
            if (current.type != TokenType.Symbol)
            {
                ErrorHandler.Parser.Report(fileName, token, errorType, errorMessage);
                ErrorHandler.Parser.PrettyError(fileName, token);
                Continue();
                return;
            }
            analyzed.Add(current);
            Continue();
            return;
        }

        bool NotIdentifierOrLiteral()
        {
            return Current().type != TokenType.Identifier 
                && Current().type != TokenType.Numeric 
                && Current().type != TokenType.String;
        }

    }

}
