namespace Otterkit;

public static class Codegen
{
    private static string ProgramEntryPoint = string.Empty;

    public static void Generate(List<Token> tokens, string fileName)
    {
        ProgramBuilder compiled = new();
        int index = 0;

        while (!CurrentEquals("DATA"))
        {
            if (CurrentEquals("PROGRAM-ID"))
            {
                compiled.DefineIdentification(Lookahead(2).value);
                if (ProgramEntryPoint == string.Empty)
                {
                    ProgramEntryPoint = compiled.Identification;
                }
            }

            if (CurrentEquals("FUNCTION-ID"))
                compiled.DefineIdentification(Lookahead(2).value);

            Continue();
        }

        string scope = string.Empty;
        while (!CurrentEquals("PROCEDURE"))
        {
            if (CurrentEquals("WORKING-STORAGE") || CurrentEquals("LOCAL-STORAGE"))
                scope = Current().value;

            if (Current().type == TokenType.Numeric && (CurrentEquals("01") || CurrentEquals("1")) && !LookaheadEquals(2, "CONSTANT"))
            {
                DataItemBuilder Record = new(compiled, Continue, Current, Lookahead);
                Record.BuildDataItem(scope);
            }

            if (Current().type == TokenType.Numeric && LookaheadEquals(2, "CONSTANT"))
            {
                DataItemBuilder Constant = new(compiled, Continue, Current, Lookahead);
                Constant.BuildDataItem(scope);
            }

            if (Current().type == TokenType.Numeric && Current().value.Equals("77"))
            {
                DataItemBuilder SevenSeven = new(compiled, Continue, Current, Lookahead);
                SevenSeven.BuildDataItem(scope);
            }

            Continue();
        }

        while (!CurrentEquals("EOF"))
        {
            StatementBuilder statement = new(compiled, Continue, Current, Lookahead);
            statement.BuildStatement();

            if (!CurrentEquals("EOF"))
            {
                if (CurrentEquals("END") && (LookaheadEquals(1, "PROGRAM") || LookaheadEquals(1, "FUNCTION")) && !LookaheadEquals(4, "EOF"))
                {
                    Continue(2);
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
        File.WriteAllText($".otterkit/OtterkitExport/{compiled.UnformattedID}.cs", compiled.ExportCompiled());
        string startupCode = $"""
        using OtterkitLibrary;
        using OtterkitExport;

        {ProgramEntryPoint} startup = new();
        startup.Procedure();
        """;

        File.WriteAllText(".otterkit/OtterkitExport/Startup.cs", startupCode);

        // Generator helper methods.
        Token Lookahead(int amount)
        {
            return tokens[index + amount];
        }

        bool LookaheadEquals(int lookahead, string stringToCompare)
        {
            return Lookahead(lookahead).value.Equals(stringToCompare);
        }

        Token Current()
        {
            return tokens[index];
        }

        bool CurrentEquals(string stringToCompare)
        {
            return Current().value.Equals(stringToCompare);
        }

        void Continue(int amount = 1)
        {
            index += amount;
            return;
        }
    }

}