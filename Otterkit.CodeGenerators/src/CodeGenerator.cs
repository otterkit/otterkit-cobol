using static Otterkit.Types.TokenHandling;
using Otterkit.Types;

namespace Otterkit.CodeGenerators;

public static class CodeGenerator
{
    private static string Main = string.Empty;
    private static bool HasHeader = false;

    public static void Generate(List<Token> tokens, string fileName)
    {
        ProgramBuilder compiled = new();
        
        if (!HasHeader)
        {
            compiled.AppendHeader();

            HasHeader = true;
        }
        
        while (Current().Scope is not TokenScope.EnvironmentDivision and not TokenScope.DataDivision and not TokenScope.ProcedureDivision)
        {
            if (CurrentEquals("PROGRAM-ID"))
            {
                compiled.FormatIdentification(Peek(2));

                if (Main == string.Empty) Main = compiled.Identification.Unwrap();
            }

            if (CurrentEquals("FUNCTION-ID"))
            {
                compiled.FormatIdentification(Peek(2));
            }

            Continue();
        }

        compiled.AppendIdentification();

        SourceScope scope = SourceScope.WorkingStorage;
        
        while (Current().Scope is not TokenScope.ProcedureDivision)
        {
            if (CurrentEquals("WORKING-STORAGE"))
                scope = SourceScope.WorkingStorage;

            if (CurrentEquals("LOCAL-STORAGE"))
                scope = SourceScope.LocalStorage;

            if (Current().Type == TokenType.Numeric && (CurrentEquals("01") || CurrentEquals("1")) && !PeekEquals(2, "CONSTANT"))
            {
                VariableBuilder variable = new(compiled);

                variable.BuildVariable(scope);
            }

            if (Current().Type == TokenType.Numeric && PeekEquals(2, "CONSTANT"))
            {
                VariableBuilder Constant = new(compiled);
                Constant.BuildVariable(scope);
            }

            if (Current().Type == TokenType.Numeric && Current().Value.Equals("77"))
            {
                VariableBuilder SevenSeven = new(compiled);
                SevenSeven.BuildVariable(scope);
            }

            Continue();
        }

        compiled.InitializeProcedure();

        while (!CurrentEquals("EOF"))
        {
            StatementBuilder statement = new(compiled);

            statement.BuildStatement();

            if (!CurrentEquals("EOF"))
            {
                if (CurrentEquals("END") && (PeekEquals(1, "PROGRAM") || PeekEquals(1, "FUNCTION")) && !PeekEquals(4, "EOF"))
                {
                    Continue(2);

                    Generate(tokens, fileName);
                    break;
                }
            }

            Continue();
        }

        compiled.FinalizeProcedure();

        File.WriteAllText($".otterkit/Artifacts/Source.cs", compiled.ExportCompiled());

        string startupCode = $"using Otterkit.Runtime;using OtterkitExport;{Main}.Procedure();";

        File.WriteAllText(".otterkit/Artifacts/Main.cs", startupCode);
    }

}