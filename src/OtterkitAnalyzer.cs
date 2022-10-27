namespace Otterkit;

public static class analyzerScopes
{
    public static readonly List<string> statements = new()
    {
        "ACCEPT", "ADD", "ALLOCATE", "CALL", "CANCEL", "CLOSE", "COMMIT", "COMPUTE", "CONTINUE", "DELETE",
        "DISPLAY", "DIVIDE", "EVALUATE", "EXIT", "FREE", "GENERATE", "GO TO", "GOBACK", "IF", "INITIALIZE",
        "INITIATE", "INSPECT", "INVOKE", "MERGE", "MOVE", "MULTIPLY", "OPEN", "PERFORM", "RAISE", "READ",
        "RECEIVE", "RELEASE", "RESUME", "RETURN", "REWRITE", "ROLLBACK", "SEARCH", "SEND", "SET", "SORT",
        "START", "STOP", "STRING", "SUBTRACT", "SUPPRESS", "TERMINATE", "UNLOCK", "UNSTRING", "USE", "VALIDATE",
        "WRITE"
    };

    public static readonly List<string> allowScopeChange = new()
    {
        "FALSE", "ON-ERROR", "FALSE", "ON-EXCEPTION", "FALSE", "FALSE", "FALSE", "ON-ERROR", "FALSE", "ON-INVALID-OR-ON-EXCEPTION",
        "FALSE", "ON-ERROR", "TRUE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "TRUE", "FALSE",
        "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "ON-ERROR", "FALSE", "TRUE", "FALSE", "AT-END-OR-ON-INVALID",
        "ON-EXCEPTION", "FALSE", "FALSE", "AT-END", "ON-INVALID", "FALSE", "AT-END-OR-WHEN-CONDITION", "ON-EXCEPTION", "FALSE", "FALSE",
        "ON-INVALID", "FALSE", "ON-OVERFLOW", "ON-ERROR", "FALSE", "FALSE", "FALSE", "ON-OVERFLOW", "FALSE", "FALSE",
        "ON-INVALID-OR-AT-END-OF-PAGE"
    };
}


public static class OtterkitAnalyzer
{

    public static List<Token> Analyze(List<Token> tokenList)
    {
        List<Token> analyzed = new();
        int index = 0;

        SOURCE();
        return analyzed;

        void SOURCE()
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
                    SOURCE();
                    break;

                case "ENVIRONMENT": 
                    ENVIRONMENT();
                    SOURCE();
                    break;

                case "DATA":
                    DATA();
                    SOURCE();
                    break;

                case "PROCEDURE":
                    PROCEDURE();
                    SOURCE();
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
        }

        void PROCEDURE()
        {
            Expected("PROCEDURE", "procedure division");
            Expected("DIVISION");
            Expected(".", "separator period");
        }

        string LookAhead(int amount)
        {
            return tokenList[index + amount].value;
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

        void Choice(params string[] choices)
        {
            Token current = Current();
            foreach(string choice in choices)
            {
                if (current.value == choice)
                {
                    analyzed.Add(current);
                    Continue();
                    return;
                }
            }

            ErrorHandler.Parser.Report(Current(), "choice", choices);
            Continue();
            return;
        }

        void Optional(string optional)
        {
            Token current = Current();
            if (current.value != optional)
                return;
            
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

    }

}