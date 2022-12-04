namespace Otterkit;

public static class OtterkitCodegen
{
    public static void Generate(List<Token> tokens, string fileName)
    {
        ProgramBuilder compiled = new();
        int index = 0;

        while (Current().value != "DATA")
        {
            if (Current().value == "PROGRAM-ID")
                compiled.DefineIdentification(LookAhead(2).value);

            Continue();
        }

        string scope = string.Empty;
        while (Current().value != "PROCEDURE")
        {
            if (Current().value == "WORKING-STORAGE" || Current().value == "LOCAL-STORAGE") 
                scope = Current().value;

            if (Current().type == TokenType.Numeric && (Current().value.Equals("01") || Current().value.Equals("1")) && !LookAhead(2).value.Equals("CONSTANT"))
            {
                DataItemBuilder Record = new(compiled, Continue, Current, LookAhead);
                Record.BuildDataItem(scope);
            }

            if (Current().type == TokenType.Numeric && LookAhead(2).value.Equals("CONSTANT"))
            {
                DataItemBuilder Constant = new(compiled, Continue, Current, LookAhead);
                Constant.BuildDataItem(scope);
            }

            if (Current().type == TokenType.Numeric && Current().value.Equals("77"))
            {
                DataItemBuilder SevenSeven = new(compiled, Continue, Current, LookAhead);
                SevenSeven.BuildDataItem(scope);
            }

            Continue();
        }

        while (Current().value != "EOF")
        {
            StatementBuilder statement = new(compiled, Continue, Current, LookAhead);
            statement.BuildStatement();

            if(Current().value != "EOF")
            {
                if (Current().value.Equals("END") && LookAhead(1).value.Equals("PROGRAM"))
                {
                    Continue();
                    Continue();
                    List<Token> NextProgram = tokens.GetRange(index, tokens.Count - index - 1);
                    Generate(NextProgram, fileName);
                    break;
                }
            }

            Continue();
        }

        compiled.CompileHeader();
        compiled.CompileIdentification();
        compiled.CompileData();
        compiled.CompileProcedure();

        Directory.CreateDirectory(".otterkit");
        File.WriteAllText($".otterkit/{compiled.UnformattedID}.cs", compiled.ExportCompiled());

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