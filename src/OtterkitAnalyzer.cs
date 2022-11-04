namespace Otterkit;

public static class OtterkitAnalyzer
{
    public static List<Token> Analyze(List<Token> tokenList)
    {
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
                    ErrorHandler.Parser.Report(Current(), "expected", "IDENTIFICATION, ENVIRONMENT, DATA or PROCEDURE");
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
            WorkingStorage();
        }

        void WorkingStorage()
        {
            Expected("WORKING-STORAGE", "working-storage section");
            Expected("SECTION");
            Expected(".", "separator period");
            ConstantEntry();
        }

        void ConstantEntry()
        {
            Number();
            Identifier();
            Expected("CONSTANT");
            Expected("AS");
            Number();
            Expected(".", "separator period");
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
                    DISPLAY();
                    ScopeTerminator(isNested);
                    Statement(isNested);
                    break;

                case "DIVIDE":
                    DISPLAY();
                    ScopeTerminator(isNested);
                    Statement(isNested);
                    break;

                case "MULTIPLY":
                    DISPLAY();
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
                    ErrorHandler.Parser.Report(Current(), "expected", "identifier or literal");
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
            while (Current().type == TokenType.Identifier)
            {
                Identifier();
            }

            Expected("=");
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

                        default:
                            ErrorHandler.Parser.Report(Current(), "expected", "+, -, *, /, **, ( or )");
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
                        ErrorHandler.Parser.Report(Current(), "expected", "identifier or numeric literal");
                        break;
                }

                Expected("GIVING");
                while (Current().type == TokenType.Identifier)
                    Identifier();
            } 
            else if (Current().value == "GIVING")
            {
                Expected("GIVING");
                while (Current().type == TokenType.Identifier)
                    Identifier();
            }
            else if (Current().value == "TO")
            {
                Expected("TO");
                while (Current().type == TokenType.Identifier)
                    Identifier();
            }

            SizeError(ref isConditional);

            if (isConditional)
                Expected("END-ADD");
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
                if (current.value == choice)
                {
                    if (type != null)
                        current.type = type;
                    analyzed.Add(current);
                    Continue();
                    return;
                }
            }

            ErrorHandler.Parser.Report(Current(), "choice", choices);
            Continue();
            return;
        }

        void Optional(string optional, string scope = "")
        {
            Token current = Current();
            if (current.value != optional)
                return;

            current.scope = scope;
            analyzed.Add(current);
            Continue();
            return;
        }

        void Expected(string expected, string scope = "")
        {
            Token current = Current();
            if (current.value != expected)
            {
                ErrorHandler.Parser.Report(Current(), "expected", expected);
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
            if (Current().value == "ON" || Current().value == "SIZE")
            {
                isConditional = true;
                Optional("ON");
                Expected("SIZE");
                Expected("ERROR");
                Statement(true);
            }

            if (Current().value == "NOT")
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
                ErrorHandler.Parser.Report(Current(), "expected", "identifier");
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
                ErrorHandler.Parser.Report(Current(), "expected", "numberic literal");
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
                ErrorHandler.Parser.Report(Current(), "expected", "string literal");
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
                ErrorHandler.Parser.Report(Current(), "expected", "symbol");
                Continue();
                return;
            }
            analyzed.Add(current);
            Continue();
            return;
        }

    }

}