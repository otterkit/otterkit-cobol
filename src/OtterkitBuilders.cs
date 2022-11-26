namespace Otterkit;

public class ProgramBuilder
{
    static readonly string Tab = "    ";
    private string Compiled { get; set; }
    private string Identification { get; set; }
    private string WorkingStorage { get; set; }
    private string LocalStorage { get; set; }
    private string Statements { get; set; }

    public ProgramBuilder()
    {
        this.Compiled = string.Empty;
        this.Identification = string.Empty;
        this.WorkingStorage = string.Empty;
        this.LocalStorage = string.Empty;
        this.Statements = string.Empty;
    }

    public string ExportCompiled()
    {
        return Compiled;
    }

    public void DefineIdentification(string Identification)
    {
        this.Identification = Identification;
    }

    public void AppendWorkingStorage(string dataItem)
    {
        WorkingStorage += String.Concat(Tab, dataItem, "\n");
    }

    public void AppendLocalStorage(string dataItem)
    {
        LocalStorage += String.Concat(Tab, Tab, dataItem, "\n");
    }

    public void AppendStatement(string statement)
    {
        Statements += String.Concat(Tab, Tab, statement, "\n");
    }


    public void CompileHeader()
    {
        Compiled += """
        using OtterkitLibrary;
        namespace OtterkitExport;

        """;
    }

    public void CompileIdentification()
    {
        string ID = $$"""
        // PROGRAM-ID. {{this.Identification}}.
        public class {{this.Identification}}
        {
        
        """;

        Compiled += ID;
    }

    public void CompileWorkingStorage()
    {
        string WS = $$"""
            // WORKING-STORAGE SECTION.
        {{this.WorkingStorage}}

        """;

        Compiled += WS;
    }

    public void CompileProcedure()
    {
        string Procedure = $$"""
        // PROCEDURE DIVISION.
        public void Procedure()
        {
            // LOCAL-STORAGE SECTION.
    {{this.LocalStorage}}

            // PROCEDURE STATEMENTS.
    {{this.Statements}}
        }
    }
    
    """;

        Compiled += Procedure;
    }
}
