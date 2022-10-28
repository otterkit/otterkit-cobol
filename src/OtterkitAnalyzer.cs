namespace Otterkit;

public static class analyzerStatements
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
            PROGRAM_ID();
        }

        void PROGRAM_ID()
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
            WORKING_STORAGE();
        }

        void WORKING_STORAGE()
        {
            Expected("WORKING-STORAGE", "working-storage section");
            Expected("SECTION");
            Expected(".", "separator period");
            CONSTANT_ENTRY();
        }

        void CONSTANT_ENTRY()
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
            STATEMENT();
        }

        void STATEMENT()
        {
            switch (Current().value)
            {
                case "DISPLAY":
                    DISPLAY();
                    break;

            }
        }

        // Statement parsing section:
        void DISPLAY()
        {
            Expected("DISPLAY");
            switch (Current().type)
            {
                case "identifier":
                    Identifier();
                    break;
                case "number literal":
                    Number();
                    break;
                default:
                    ErrorHandler.Parser.Report(Current(), "expected", "identifier or literal");
                    break;
            }
            Expected(".", "separator period");
        }


        // Parser helper methods.
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

        void Identifier()
        {
            Token current = Current();
            if (current.type != "identifier")
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
            if (current.type != "number literal")
            {
                ErrorHandler.Parser.Report(Current(), "expected", "number literal");
                Continue();
                return;
            }
            analyzed.Add(current);
            Continue();
            return;
        }

    }

}