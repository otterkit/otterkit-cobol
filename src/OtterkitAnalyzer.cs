using System.Diagnostics;

namespace Otterkit;

public static class OtterkitAnalyzer
{
    public static List<Token> Analyze(List<Token> tokenList, string fileName)
    {
        string FileName = fileName;
        List<Token> analyzed = new();
        int index = 0;

        Source();
        return analyzed;

        void Source()
        {
            if (Current().value == "EOF")
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
                    Environment.Exit(1);
                    break;
            }
        }

        void IDENTIFICATION()
        {
            Expected("IDENTIFICATION", "identification division");
            Expected("DIVISION");
            Expected(".", "separator period");
            ProgramId();
        }

        void ProgramId()
        {
            Expected("PROGRAM-ID", "program definition");
            Expected(".", "separator period");
            Identifier();
            Expected(".", "separator period");
        }

        void ENVIRONMENT()
        {
            Expected("ENVIRONMENT", "environment division");
            Expected("DIVISION");
            Expected(".", "separator period");
        }

        void DATA()
        {
            Expected("DATA", "data division");
            Expected("DIVISION");
            Expected(".", "separator period");
            DataSections();
        }

        void DataSections()
        {
            while(Current().value != "PROCEDURE")
            {
                if (Current().value.Equals("WORKING-STORAGE"))
                    WorkingStorage();

                if(Current().value.Equals("LOCAL-STORAGE"))
                    LocalStorage();

                switch(Current().value)
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
            Expected("WORKING-STORAGE", "working-storage section");
            Expected("SECTION");
            Expected(".", "separator period");
            while (Current().type == TokenType.Numeric)
                Entries();
        }

        void LocalStorage()
        {
            Expected("LOCAL-STORAGE", "local-storage section");
            Expected("SECTION");
            Expected(".", "separator period");
            while (Current().type == TokenType.Numeric)
                Entries();
        }

        void Entries()
        {
            if(Current().value == "77")
                SevenSevenEntry();

            if (LookAhead(2).value == "CONSTANT")
                ConstantEntry();
        }

        void SevenSevenEntry()
        {
            string datatype = string.Empty;
            Number();
            Identifier();
            Choice(null, "PIC", "PICTURE");
            Optional("IS");
            datatype = Current().value switch
            {
                "S9" => "Signed Numeric",
                "9" => "Numeric",
                "X" => "Alphanumeric",
                "A" => "Alphabetic",
                "N" => "National",
                "1" => "Boolean",
                _ => "Error"
            };
            if (datatype == "Error")
            {
                ErrorHandler.Parser.Report(fileName, Current(), " ", "Unrecognized type, expected S9, 9, X, A, N or 1");
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }

            Choice(null, "S9", "9", "X", "A", "N", "1");
            Expected("(");
            Number();
            Expected(")");
            if(Current().value == "V9" && (datatype != "Signed Numeric" && (datatype != "Numeric")))
            {
                ErrorHandler.Parser.Report(fileName, Current(), " ", "V9 cannot be used with non-numeric types");
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }
        
            if(Current().value == "V9")
            {
                Expected("V9");
                Expected("(");
                Number();
                Expected(")");
            }
            
            if(Current().value == "VALUE")
            {
                Expected("VALUE");
                switch (datatype)
                {
                    case "Signed Numeric":
                    case "Numeric":
                        Number();
                        break;
                    
                    case "Alphanumeric":
                    case "Alphabetic":
                    case "National":
                    case "Boolean":
                        String();
                        break;

                    case "Error":
                        ErrorHandler.Parser.Report(fileName, Current(), " ", "Unable to determine the correct literal type due to the previous type error");
                        ErrorHandler.Parser.PrettyError(fileName, Current());
                        break;

                    default:
                        throw new UnreachableException("Unrecognized type has already been checked above. This should be unreachable.");
                }
            }

            Expected(".", "separator period");
        }

        void ConstantEntry()
        {
            if (Current().value != "01" && Current().value != "1")
            {
                ErrorHandler.Parser.Report(fileName, Current(), " ", "Constant entry must have a level number of 1 or 01");
                ErrorHandler.Parser.PrettyError(fileName, Current());
            }

            Number();
            Identifier();
            Expected("CONSTANT");
            if (Current().value == "IS" || Current().value == "GLOBAL")
            {
                Optional("IS");
                Expected("GLOBAL");
            }

            if (Current().value == "FROM")
            {
                Expected("FROM");
                Identifier();
                Expected(".", "separator period");
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

                if (Current().value == "LENGTH")
                {
                    Expected("LENGTH");
                    Optional("OF");
                    Identifier();
                }

                if (Current().value == "BYTE-LENGTH")
                {
                    Expected("BYTE-LENGTH");
                    Optional("OF");
                    Identifier();
                }

                Expected(".", "separator period");
            }
        }

        void PROCEDURE()
        {
            Expected("PROCEDURE", "procedure division");
            Expected("DIVISION");
            Expected(".", "separator period");
            Statement();
        }

        void Statement(bool isNested = false)
        {
            switch (Current().value)
            {
                case "DISPLAY":
                    DISPLAY();
                    ScopeTerminator(isNested);
                    Statement(isNested);
                    break;

                case "ACCEPT":
                    ACCEPT();
                    ScopeTerminator(isNested);
                    Statement(isNested);
                    break;

                case "COMPUTE":
                    COMPUTE();
                    ScopeTerminator(isNested);
                    Statement(isNested);
                    break;

                case "ADD":
                    ADD();
                    ScopeTerminator(isNested);
                    Statement(isNested);
                    break;

                case "SUBTRACT":
                    SUBTRACT();
                    ScopeTerminator(isNested);
                    Statement(isNested);
                    break;

                case "DIVIDE":
                    DIVIDE();
                    ScopeTerminator(isNested);
                    Statement(isNested);
                    break;

                case "MULTIPLY":
                    MULTIPLY();
                    ScopeTerminator(isNested);
                    Statement(isNested);
                    break;

                case "STOP":
                    STOP();
                    ScopeTerminator(isNested);
                    Statement(isNested);
                    break;
            }
        }

        void ScopeTerminator(bool isNested)
        {
            if (isNested)
                return;

            if (!isNested)
            {
                Expected(".", "separator period");
                return;
            }
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

            if (Current().value == "UPON")
            {
                Expected("UPON");
                Choice(TokenType.Device, "STANDARD-OUTPUT", "STANDARD-ERROR");
            }

            if (Current().value == "WITH" || Current().value == "NO")
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
            if (Current().value == "FROM")
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
            if (Current().type != TokenType.Identifier 
             && Current().type != TokenType.Numeric
             && Current().type != TokenType.Symbol)
                ErrorHandler.Parser.Report(fileName, Current(), "expected", "identifier, numeric literal or arithmetic symbol");

            while (Current().type == TokenType.Identifier
               || Current().type == TokenType.Numeric
               || Current().type == TokenType.Symbol
            )
            {
                if (Current().type == TokenType.Identifier)
                    Identifier();

                if (Current().type == TokenType.Numeric)
                    Number();

                if (Current().type == TokenType.Symbol)
                {
                    switch (Current().value)
                    {
                        case "+":
                        case "-":
                        case "*":
                        case "/":
                        case "**":
                        case "(":
                        case ")":
                            Symbol();
                            break;

                        case ".":
                            return;

                        default:
                            ErrorHandler.Parser.Report(fileName, Current(), "expected", "+, -, *, /, **, ( or )");
                            ErrorHandler.Parser.PrettyError(fileName, Current());
                            Continue();
                            break;
                    }
                }
            }

            SizeError(ref isConditional);

            if (isConditional)
                Expected("END-COMPUTE");
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

            if(Current().value == "TO" && LookAhead(2).value == "GIVING")
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
            else if (Current().value == "GIVING")
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
            else if (Current().value == "TO")
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

            if(Current().value == "FROM" && LookAhead(2).value == "GIVING")
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
            else if (Current().value == "FROM")
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

            if(Current().value == "BY" && LookAhead(2).value == "GIVING")
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
            else if (Current().value == "BY")
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

            if((Current().value == "BY" || Current().value == "INTO") 
                && LookAhead(2).value == "GIVING" && LookAhead(4).value != "REMAINDER"
            )
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
            else if((Current().value == "BY" || Current().value == "INTO") 
                && LookAhead(2).value == "GIVING" && LookAhead(4).value == "REMAINDER"
            )
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
            else if (Current().value == "INTO")
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

        void STOP()
        {
            Expected("STOP");
            Expected("RUN");
            if (Current().value == "WITH"
             || Current().value == "NORMAL"
             || Current().value == "ERROR"
            )
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

        // Parser helper methods.
        Token LookAhead(int amount)
        {
            return tokenList[index + amount];
        }

        void Continue()
        {
            index += 1;
            return;
        }

        Token Current()
        {
            return tokenList[index];
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

            current.scope = scope;
            analyzed.Add(current);
            Continue();
            return;
        }

        void Expected(string expected, string scope = "")
        {
            Token current = Current();
            if (!current.value.Equals(expected))
            {
                ErrorHandler.Parser.Report(fileName, Current(), "expected", expected);
                ErrorHandler.Parser.PrettyError(fileName, Current());
                Continue();
                return;
            }
            current.scope = scope;
            analyzed.Add(current);
            Continue();
            return;
        }

        void SizeError(ref bool isConditional)
        {
            if (Current().value.Equals("ON") || Current().value.Equals("SIZE"))
            {
                isConditional = true;
                Optional("ON");
                Expected("SIZE");
                Expected("ERROR");
                Statement(true);
            }

            if (Current().value.Equals("NOT"))
            {
                isConditional = true;
                Expected("NOT");
                Optional("ON");
                Expected("SIZE");
                Expected("ERROR");
                Statement(true);
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

        void Number()
        {
            Token current = Current();
            if (current.type != TokenType.Numeric)
            {
                ErrorHandler.Parser.Report(fileName, Current(), "expected", "numberic literal");
                ErrorHandler.Parser.PrettyError(fileName, Current());
                Continue();
                return;
            }
            analyzed.Add(current);
            Continue();
            return;
        }

        void String()
        {
            Token current = Current();
            if (current.type != TokenType.String)
            {
                ErrorHandler.Parser.Report(fileName, Current(), "expected", "string literal");
                ErrorHandler.Parser.PrettyError(fileName, Current());
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

        void Symbol()
        {
            Token current = Current();
            if (current.type != TokenType.Symbol)
            {
                ErrorHandler.Parser.Report(fileName, Current(), "expected", "symbol");
                ErrorHandler.Parser.PrettyError(fileName, Current());
                Continue();
                return;
            }
            analyzed.Add(current);
            Continue();
            return;
        }

    }

}