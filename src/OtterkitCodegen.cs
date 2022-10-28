namespace Otterkit;

public static class Codegen
{
    public static List<string> Generate(List<Token> tokenList)
    {
        List<string> compiled = new();
        int index = 0;

        SOURCE();
        return compiled;

        void SOURCE()
        {
            if (Current().value == "EOF")
            {
                compiled.Add(Environment.NewLine + "// Otterkit EOF");
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
            Compile("IDENTIFICATION", "identification division");
            Compile("DIVISION");
            Compile(".", "separator period");
            PROGRAM_ID();
        }

        void PROGRAM_ID()
        {
            Compile("PROGRAM-ID", "program definition");
            Compile(".", "separator period");
            Identifier();
            Compile(".", "separator period");

        }

        void ENVIRONMENT()
        {
            Compile("ENVIRONMENT", "environment division");
            Compile("DIVISION");
            Compile(".", "separator period");
        }

        void DATA()
        {
            Compile("DATA", "data division");
            Compile("DIVISION");
            Compile(".", "separator period");
            WORKING_STORAGE();
        }

        void WORKING_STORAGE()
        {
            Compile("WORKING-STORAGE", "working-storage section");
            Compile("SECTION");
            Compile(".", "separator period");
            CONSTANT_ENTRY();
        }

        void CONSTANT_ENTRY()
        {
            Number();
            Identifier();
            Compile("CONSTANT");
            Compile("AS");
            Number();
            Compile(".", "separator period");
        }

        void PROCEDURE()
        {
            Compile("PROCEDURE", "procedure division");
            Compile("DIVISION");
            Compile(".", "separator period");
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
            Compile("DISPLAY");
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
            Compile(".", "separator period");
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
                    compiled.Add("current");
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
            
            compiled.Add("current");
            Continue();
            return;
        }
        
        void Compile(string expected, string scope = "")
        {
            Token current = Current();
            if (current.value != expected)
            {
                ErrorHandler.Parser.Report(Current(), "expected", expected);
                Continue();
                return;
            }
            current.scope = scope;
            compiled.Add("current");
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
            compiled.Add("current");
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
            compiled.Add("current");
            Continue();
            return;
        }

    }
}