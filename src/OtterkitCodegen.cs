namespace Otterkit;

public static class OtterkitCodegen
{
    public static void Generate(List<Token> tokens, string fileName)
    {
        string file = fileName.Replace("cob", "cs");
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

            if (Current().type == TokenType.Numeric && LookAhead(2).value == "CONSTANT")
            {
                DataItemBuilder Constant = new(compiled, Continue, Current, LookAhead);
                Constant.BuildDataItem(scope);
            }

            if (Current().type == TokenType.Numeric && Current().value == "77")
            {
                DataItemBuilder SevenSeven = new(compiled, Continue, Current, LookAhead);
                SevenSeven.BuildDataItem(scope);
            }

            Continue();
        }

        compiled.CompileHeader();
        compiled.CompileIdentification();
        compiled.CompileData();
        compiled.CompileProcedure();

        Directory.CreateDirectory(".otterkit");
        File.WriteAllText($".otterkit/{file}", compiled.ExportCompiled());

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