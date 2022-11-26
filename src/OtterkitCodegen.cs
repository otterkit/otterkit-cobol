namespace Otterkit;

public static class OtterkitCodegen
{
    public static void Generate(List<Token> tokens)
    {
        ProgramBuilder compiled = new();
        int index = 0;

        while (Current().value != "DATA")
        {
            if (Current().value == "PROGRAM-ID")
                compiled.DefineIdentification(LookAhead(1).value);

            Continue();
        }

        while (Current().value != "PROCEDURE")
        {

            if (Current().type == TokenType.Numeric && LookAhead(2).value == "CONSTANT")
            {
                string Identifier;
                string value;
                Continue();
                Identifier = Current().value;
                
            }

            Continue();
        }

        // Generator helper methods.
        Token LookAhead(int amount)
        {
            return tokens[index + amount];
        }

        void Continue()
        {
            index += 1;
            return;
        }

        Token Current()
        {
            return tokens[index];
        }
    }

}