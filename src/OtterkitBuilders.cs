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
        string FormattedID = Identification;
        FormattedID = "_" + FormattedID.Replace("-", "_");
        this.Identification = FormattedID;
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
        using System.Text;
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
            private static readonly Encoding encoding = Encoding.UTF8;
        
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

public class DataItemBuilder
{
    private string CompiledDataItem = string.Empty;
    private string LevelNumber = string.Empty;
    private string Identifier = string.Empty;
    private string DataType = string.Empty;
    private int Length = 0;
    private int FractionalLength = 0;
    private string DataValue = string.Empty;
    private ProgramBuilder ProgramBuilder;
    private Action Continue;
    private Func<Token> Current;
    private Func<int, Token> Lookahead;


    public DataItemBuilder(ProgramBuilder ProgramBuilder, Action Continue, Func<Token> Current, Func<int, Token> Lookahead)
    {
        this.ProgramBuilder = ProgramBuilder;
        this.Current = Current;
        this.Continue = Continue;
        this.Lookahead = Lookahead;
    }

    public void ExportDataItem()
    {
        ProgramBuilder.AppendWorkingStorage(CompiledDataItem);
    }

    public void BuildDataItem()
    {
        if (Current().type != TokenType.Numeric)
            throw new ArgumentException("Unexpected Input: Data Item Builder has to start with a level number");

        this.LevelNumber = Current().value;
        Continue();

        this.Identifier = Current().value;
        FormatIdentifier();
        Continue();

        if (this.LevelNumber == "77")
            BuildSevenSeven();

        if (Current().value == "CONSTANT")
            BuildConstant();

        return;
    }

    private void BuildConstant()
    {
        if (Lookahead(1).value == "GLOBAL" || Lookahead(2).value == "GLOBAL")
            CompiledDataItem += "public static readonly Constant " + this.Identifier + " = ";

        if (Lookahead(1).value != "GLOBAL" && Lookahead(2).value != "GLOBAL")
            CompiledDataItem += "private static readonly Constant " + this.Identifier + " = ";

        while (Current().value != "AS")
        {
            Continue();
        }

        Continue();

        if (Current().value == "LENGTH")
        {
            Continue();
            if (Current().value == "OF")
                Continue();

            // new(encoding.GetBytes(_WS_FIRST_NAME.Bytes.Length.ToString()), 0, _WS_FIRST_NAME.Bytes.Length, 0, new byte[_WS_FIRST_NAME.Bytes.Length]);
            string FormattedValue = FormatIdentifier(Current().value);
            CompiledDataItem += "new(encoding.GetBytes(" + FormattedValue;
            CompiledDataItem += ".Length.ToString()));";
        }

        if (Current().value == "BYTE-LENGTH")
        {
            Continue();
            if (Current().value == "OF")
                Continue();

            string FormattedValue = FormatIdentifier(Current().value);
            CompiledDataItem += "new(encoding.GetBytes(" + FormattedValue;
            CompiledDataItem += ".Bytes.Length.ToString()));";
        }

        if (Current().type == TokenType.String)
            CompiledDataItem += "new(" + Current().value + "u8);";

        if (Current().type == TokenType.Numeric)
            CompiledDataItem += "new(\"" + Current().value + "\"u8);";

        Continue();

        if (Current().value != ".")
            throw new ArgumentException("Unexpected Input: Constant must end with a separator period");

        ExportDataItem();
        return;
    }

    private void BuildSevenSeven()
    {
        bool isSigned = false;
        string dataTypes(Token current) => current.value switch
        {
            "X" => "Alphanumeric",
            "A" => "Alphabetic",
            "N" => "National",
            "1" => "Boolean",
            "9" => "Numeric",
            "S9" => "Numeric",
            _ => "Error"
        };

        while (Current().value != ".")
        {
            if (Current().value == "PIC" || Current().value == "PICTURE")
            {
                Continue();
                if (Current().value == "IS") Continue();

                this.DataType = dataTypes(Current());
                if (Current().value == "S9")
                    isSigned = true;

                Continue();
                Continue();

                if (this.DataType == "Alphanumeric")
                    this.Length = int.Parse(Current().value);

                if (this.DataType == "Alphabetic")
                    this.Length = int.Parse(Current().value);

                if (this.DataType == "National")
                    this.Length = int.Parse(Current().value);

                if (this.DataType == "Numeric")
                {
                    this.Length = int.Parse(Current().value);
                    if(Lookahead(2).value == "V9")
                        this.FractionalLength = int.Parse(Lookahead(4).value);
                }
            }

            if (Current().value == "VALUE")
            {
                Continue();
                this.DataValue = Current().value;
            }

            Continue();
        }

        CompiledDataItem += $"private static {DataType} {Identifier} = ";

        switch (DataType)
        {
            
            case "Alphanumeric":
            case "Alphabetic":
            case "National":
                string value = DataValue == String.Empty ? "\" \"" : this.DataValue;
                CompiledDataItem += $"new({value}u8, 0, {Length}, new byte[{Length}]);";
            break;

            case "Numeric":
                value = DataValue == String.Empty ? "\"0\"" : $"\"{DataValue}\"";
                int TotalLength = FractionalLength == 0 ? Length : Length + FractionalLength + 1;

                if (isSigned)
                {
                    if (value.IndexOfAny(new char[] {'+', '-'}) != 0) 
                        value = value.Insert(1, "+");
                    
                    TotalLength = FractionalLength == 0 ? Length : Length + FractionalLength + 2;
                    CompiledDataItem += $"new({value}u8, 0, {Length}, {FractionalLength}, new byte[{TotalLength}]);";
                    break;
                }

                CompiledDataItem += $"new({value}u8, 0, {Length}, {FractionalLength}, new byte[{TotalLength}]);";
                break;
        }

        ExportDataItem();
        return;
    }

    private void FormatIdentifier()
    {
        string FormattedIdentifier = this.Identifier;
        FormattedIdentifier = "_" + FormattedIdentifier.Replace("-", "_");
        this.Identifier = FormattedIdentifier;
    }

    private string FormatIdentifier(string Identifier)
    {
        string FormattedIdentifier = Identifier;
        FormattedIdentifier = "_" + FormattedIdentifier.Replace("-", "_");
        return FormattedIdentifier;
    }
}