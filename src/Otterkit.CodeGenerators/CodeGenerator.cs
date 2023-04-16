using Otterkit.Types;

namespace Otterkit.CodeGenerators;

public static class CodeGenerator
{
    private static string ProgramEntryPoint = string.Empty;

    public static void Generate(List<Token> tokens, string fileName)
    {
        ProgramBuilder compiled = new();
        int index = 0;

        while (Current().Scope is not TokenScope.EnvironmentDivision and not TokenScope.DataDivision and not TokenScope.ProcedureDivision)
        {
            if (CurrentEquals("PROGRAM-ID"))
            {
                compiled.DefineIdentification(Lookahead(2).Value);
                if (ProgramEntryPoint == string.Empty)
                {
                    ProgramEntryPoint = compiled.Identification;
                }
            }

            if (CurrentEquals("FUNCTION-ID"))
                compiled.DefineIdentification(Lookahead(2).Value);

            Continue();
        }

        CurrentScope scope = CurrentScope.WorkingStorage;
        while (Current().Scope is not TokenScope.ProcedureDivision)
        {
            if (CurrentEquals("WORKING-STORAGE"))
                scope = CurrentScope.WorkingStorage;

            if (CurrentEquals("LOCAL-STORAGE"))
                scope = CurrentScope.LocalStorage;

            if (Current().Type == TokenType.Numeric && (CurrentEquals("01") || CurrentEquals("1")) && !LookaheadEquals(2, "CONSTANT"))
            {
                DataItemBuilder Record = new(compiled, Continue, Current, Lookahead);
                Record.BuildDataItem(scope);
            }

            if (Current().Type == TokenType.Numeric && LookaheadEquals(2, "CONSTANT"))
            {
                DataItemBuilder Constant = new(compiled, Continue, Current, Lookahead);
                Constant.BuildDataItem(scope);
            }

            if (Current().Type == TokenType.Numeric && Current().Value.Equals("77"))
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
            return Lookahead(lookahead).Value.Equals(stringToCompare, StringComparison.OrdinalIgnoreCase);
        }

        Token Current()
        {
            return tokens[index];
        }

        bool CurrentEquals(string stringToCompare)
        {
            return Current().Value.Equals(stringToCompare, StringComparison.OrdinalIgnoreCase);
        }

        void Continue(int amount = 1)
        {
            index += amount;
            return;
        }
    }

}