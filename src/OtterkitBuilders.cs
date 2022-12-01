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
        WorkingStorage += $"{Tab}{dataItem}\n";
    }

    public void AppendLocalStorage(string dataItem)
    {
        LocalStorage += $"{Tab}{dataItem}\n";
    }

    public void AppendStatement(string statement)
    {
        Statements += $"{Tab}{Tab}{statement}\n";
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

        // PROGRAM-ID. {{Identification}}.
        public class {{Identification}}
        {
            private static readonly Encoding encoding = Encoding.UTF8;
        
        """;

        Compiled += ID;
    }

    public void CompileData()
    {
        string WS = $$"""

            // WORKING-STORAGE SECTION.
        {{WorkingStorage}}
            // LOCAL-STORAGE SECTION.
        {{LocalStorage}}
        """;

        Compiled += WS;
    }

    public void CompileProcedure()
    {
        string Procedure = $$"""

        // PROCEDURE DIVISION.
        public void Procedure()
        {
            // PROCEDURE STATEMENTS.
    {{Statements}}
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
    private string Section = string.Empty;
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
        if (Section == "WORKING-STORAGE")
            ProgramBuilder.AppendWorkingStorage(CompiledDataItem);

        if (Section == "LOCAL-STORAGE")
            ProgramBuilder.AppendLocalStorage(CompiledDataItem);
    }

    public void BuildDataItem(string section = "WORKING-STORAGE")
    {
        Section = section;
        if (Current().type != TokenType.Numeric)
            throw new ArgumentException("Unexpected Input: Data Item Builder has to start with a level number");

        LevelNumber = Current().value;
        Continue();

        Identifier = Current().value;
        FormatIdentifier();
        Continue();

        if (LevelNumber == "77")
            BuildSevenSeven();

        if (Current().value == "CONSTANT")
            BuildConstant();

        return;
    }

    private void BuildConstant()
    {
        string sectionAccessModifier = string.Empty;

        if (Section.Equals("WORKING-STORAGE"))
            CompiledDataItem = $"private static readonly Constant {Identifier} = ";

        if (Section.Equals("LOCAL-STORAGE"))
            CompiledDataItem = $"private readonly Constant {Identifier} = ";

        while (Current().value != "AS")
        {
            Continue();
        }

        Continue();

        if (Current().value.Equals("LENGTH"))
        {
            Continue();
            if (Current().value.Equals("OF"))
                Continue();

            // new(encoding.GetBytes(_WS_FIRST_NAME.Bytes.Length.ToString()), 0, _WS_FIRST_NAME.Bytes.Length, 0, new byte[_WS_FIRST_NAME.Bytes.Length]);
            string FormattedValue = FormatIdentifier(Current().value);
            CompiledDataItem += $"new(encoding.GetBytes({FormattedValue}.Length.ToString()));";
        }

        if (Current().value.Equals("BYTE-LENGTH"))
        {
            Continue();
            if (Current().value.Equals("OF"))
                Continue();

            string FormattedValue = FormatIdentifier(Current().value);
            CompiledDataItem += $"new(encoding.GetBytes({FormattedValue}.Bytes.Length.ToString()));";
        }

        if (Current().type == TokenType.String)
            CompiledDataItem += $"new({Current().value}u8);";

        if (Current().type == TokenType.Numeric)
            CompiledDataItem += $"new(\"{Current().value}\"u8);";

        Continue();

        if (!Current().value.Equals("."))
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
            "1" => "OtterkitBoolean",
            "9" => "Numeric",
            "S9" => "Numeric",
            _ => "Error"
        };

        while (!Current().value.Equals("."))
        {
            if (Current().value.Equals("PIC") || Current().value.Equals("PICTURE"))
            {
                Continue();
                if (Current().value.Equals("IS")) Continue();

                DataType = dataTypes(Current());
                if (Current().value.Equals("S9"))
                    isSigned = true;

                Continue();
                Continue();

                if (DataType.Equals("Alphanumeric"))
                    Length = int.Parse(Current().value);

                if (DataType.Equals("Alphabetic"))
                    Length = int.Parse(Current().value);

                if (DataType.Equals("National"))
                    Length = int.Parse(Current().value);

                if (DataType.Equals("OtterkitBoolean"))
                    Length = int.Parse(Current().value);

                if (DataType.Equals("Numeric"))
                {
                    Length = int.Parse(Current().value);
                    if(Lookahead(2).value == "V9")
                        FractionalLength = int.Parse(Lookahead(4).value);
                }
            }

            if (Current().value.Equals("VALUE"))
            {
                Continue();
                DataValue = Current().value;
            }

            Continue();
        }

        if (Section.Equals("WORKING-STORAGE"))
            CompiledDataItem = $"private static {DataType} {Identifier} = ";

        if (Section.Equals("LOCAL-STORAGE"))
            CompiledDataItem = $"private {DataType} {Identifier} = ";

        switch (DataType)
        {
            
            case "Alphanumeric":
            case "Alphabetic":
            case "National":
            case "OtterkitBoolean":
                string value = DataValue.Equals(String.Empty) ? "\" \"" : DataValue;
                CompiledDataItem += $"new({value}u8, 0, {Length}, new byte[{Length}]);";
            break;

            case "Numeric":
                value = DataValue.Equals(String.Empty) ? "\"0\"" : $"\"{DataValue}\"";
                int TotalLength = FractionalLength == 0 ? Length : Length + FractionalLength + 1;

                if (isSigned)
                {
                    if (value.IndexOfAny(new char[] { '+', '-' }) != 1) 
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
        string FormattedIdentifier = Identifier;
        FormattedIdentifier = "_" + FormattedIdentifier.Replace("-", "_");
        Identifier = FormattedIdentifier;
    }

    private string FormatIdentifier(string Identifier)
    {
        string FormattedIdentifier = Identifier;
        FormattedIdentifier = "_" + FormattedIdentifier.Replace("-", "_");
        return FormattedIdentifier;
    }
}

public class StatementBuilder
{
    private string CompiledStatement = string.Empty;
    private ProgramBuilder ProgramBuilder;
    private Action Continue;
    private Func<Token> Current;
    private Func<int, Token> Lookahead;


    public StatementBuilder(ProgramBuilder ProgramBuilder, Action Continue, Func<Token> Current, Func<int, Token> Lookahead)
    {
        this.ProgramBuilder = ProgramBuilder;
        this.Current = Current;
        this.Continue = Continue;
        this.Lookahead = Lookahead;
    }

    public void ExportStatement()
    {
        ProgramBuilder.AppendStatement(CompiledStatement);
    }

    public void BuildStatement()
    {
        Statement();
    }

    private void Statement()
    {
        switch (Current().value)
        {
            case "DISPLAY":
                DISPLAY();
                break;

            case "ACCEPT":
                ACCEPT();
                break;

            case "STOP":
                STOP();
                break;
        }
    }

    private void DISPLAY()
    {
        CompiledStatement += "Statements.DISPLAY(";
        string displayStrings = string.Empty;
        Continue();

        while (Current().type == TokenType.Identifier
            || Current().type == TokenType.Numeric
            || Current().type == TokenType.String
        )
        {
            string identifier;
            if (Current().type == TokenType.Identifier)
            {
                identifier = FormatIdentifier(Current().value);
                displayStrings += $"{identifier}.Display, ";
            }

            if (Current().type == TokenType.Numeric)
                displayStrings += $"\"{Current().value}\", ";

            if (Current().type == TokenType.String)
                displayStrings += $"{Current().value}, ";

            Continue();
        }

        Continue();
        if (Current().value.Equals("UPON"))
        {
            Continue();
            if(Current().value.Equals("STANDARD-OUTPUT"))
                CompiledStatement += $"\"{Current().value}\", ";

            if(Current().value.Equals("STANDARD-ERROR"))
                CompiledStatement += $"\"{Current().value}\", ";
        }

        if (!Current().value.Equals("UPON"))
            CompiledStatement += $"\" \", ";

        Continue();
        if (Current().value.Equals("WITH") || Current().value.Equals("NO"))
            CompiledStatement += "false, ";

        if (!Current().value.Equals("WITH") && !Current().value.Equals("NO"))
            CompiledStatement += "true, ";

        CompiledStatement += $"{displayStrings}String.Empty);";
        ExportStatement();
    }

    private void ACCEPT()
    {
        CompiledStatement += "Statements.ACCEPT(";
        // Statements.ACCEPT(dataItem, from, format)
        Continue();
        CompiledStatement += $"{FormatIdentifier(Current().value)}, ";
        Continue();

        if (!Current().value.Equals("FROM"))
            CompiledStatement += "\"STANDARD-INPUT\");";

        if (Current().value.Equals("FROM"))
        {
            Continue();
            switch (Current().value)
            {
                case "STANDARD-INPUT":
                case "COMMAND-LINE":
                    CompiledStatement += $"\"{Current().value}\");";
                    break;

                case "DATE":
                    CompiledStatement += $"\"{Current().value}\"";
                    if(Lookahead(1).value.Equals("YYYYMMDD"))
                        CompiledStatement += $", \"{Lookahead(1).value}\");";

                    if(!Lookahead(1).value.Equals("YYYYMMDD"))
                        CompiledStatement += ");";
                    break;

                case "DAY":
                    CompiledStatement += $"\"{Current().value}\"";
                    if(Lookahead(1).value.Equals("YYYYDDD"))
                        CompiledStatement += $", \"{Lookahead(1).value}\");";

                    if(!Lookahead(1).value.Equals("YYYYDDD"))
                        CompiledStatement += ");";
                    break;

                case "DAY-OF-WEEK":
                    CompiledStatement += $"\"{Current().value}\");";
                    break;

                case "TIME":
                    CompiledStatement += $"\"{Current().value}\");";
                    break;
            }
        }
        ExportStatement();
    }

    private void STOP()
    {
        // Statements.STOP();
        // Statements.STOP(error, status);
        CompiledStatement += "Statements.STOP(";
        Continue();
        Continue();
        
        if (Current().value.Equals("."))
        {
            CompiledStatement += ");";
            ExportStatement();
            return;
        }

        if (Current().value.Equals("WITH"))
            Continue();

        if (Current().value.Equals("NORMAL"))
            CompiledStatement += "false, ";

        if (Current().value.Equals("ERROR"))
            CompiledStatement += "true, ";

        Continue();
        if (Current().value.Equals("."))
        {
            CompiledStatement += "\"0\");";
            ExportStatement();
            return;
        }

        Continue();
        switch (Current().type)
        {
            case TokenType.Identifier:
                CompiledStatement += $"{FormatIdentifier(Current().value)}.Display);";
                break;
            case TokenType.Numeric:
                CompiledStatement += $"\"{Current().value}\");";
                break;
            case TokenType.String:
                CompiledStatement += $"{Current().value});";
                break;
        }
        ExportStatement();
    }

    private string FormatIdentifier(string Identifier)
    {
        string FormattedIdentifier = Identifier;
        FormattedIdentifier = "_" + FormattedIdentifier.Replace("-", "_");
        return FormattedIdentifier;
    }
}