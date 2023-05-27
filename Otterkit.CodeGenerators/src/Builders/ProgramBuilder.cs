using System.Text;
using Otterkit.Types;

namespace Otterkit.CodeGenerators;

public class ProgramBuilder
{
    private StringBuilder Compiled = new();
    public Option<string> Identification;
    public Option<Token> Name;

    public string ExportCompiled()
    {
        return Compiled.ToString();
    }

    public void FormatIdentification(Token token)
    {
        var identification = token.Value;

        Name = token;

        Identification = string.Create(identification.Length + 1, identification, (span, value) =>
        {
            span[0] = '_';

            for (int i = 0; i < value.Length; i++)
            {
                span[i + 1] = value[i] switch
                {
                    '-' => '_',
                    _ => value[i],
                };
            }
        });
    }

    public void Append(StringBuilder snippet)
    {
        Compiled.Append(snippet);
    }

    public void AppendHeader()
    {
        Compiled.Append("using System.Text;using Otterkit.Runtime;namespace OtterkitExport;");
    }

    public void AppendIdentification()
    {
        Compiled.Append($$"""public static class {{Identification.Unwrap()}}{""");
    }

    public void InitializeProcedure()
    {
        Compiled.Append("public static void Procedure(){");
    }

    public void FinalizeProcedure()
    {
        Compiled.Append("}}");
    }
}
