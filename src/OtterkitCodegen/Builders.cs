namespace Otterkit;

public class ProgramBuilder
{
    static readonly string Tab = "    ";
    private string Compiled { get; set; }
    public string Identification { get; set; }
    private string WorkingStorage { get; set; }
    private string LocalStorage { get; set; }
    private string Statements { get; set; }
    public string UnformattedID { get; set; }

    public ProgramBuilder()
    {
        this.Compiled = string.Empty;
        this.Identification = string.Empty;
        this.WorkingStorage = string.Empty;
        this.LocalStorage = string.Empty;
        this.Statements = string.Empty;
        this.UnformattedID = string.Empty;
    }

    public string ExportCompiled()
    {
        return Compiled;
    }

    public void DefineIdentification(string Identification)
    {
        string FormattedID = Identification;
        UnformattedID = Identification;
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

        // PROGRAM-ID. {{UnformattedID}}.
        public class {{Identification}}
        {
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
    private string ExternalName = string.Empty;
    private string DataType = string.Empty;
    private int Length = 0;
    private int FractionalLength = 0;
    private string DataValue = string.Empty;
    private CurrentScope Section;
    private readonly ProgramBuilder ProgramBuilder;
    private readonly Action<int> Continue;
    private readonly Func<Token> Current;
    private readonly Func<int, Token> Lookahead;

    public DataItemBuilder(ProgramBuilder ProgramBuilder, Action<int> Continue, Func<Token> Current, Func<int, Token> Lookahead)
    {
        this.ProgramBuilder = ProgramBuilder;
        this.Current = Current;
        this.Continue = Continue;
        this.Lookahead = Lookahead;
    }

    public void ExportDataItem()
    {
        if (Section == CurrentScope.WorkingStorage)
            ProgramBuilder.AppendWorkingStorage(CompiledDataItem);

        if (Section == CurrentScope.LocalStorage)
            ProgramBuilder.AppendLocalStorage(CompiledDataItem);
    }

    public void BuildDataItem(CurrentScope section = CurrentScope.WorkingStorage)
    {
        Section = section;
        if (Current().type != TokenType.Numeric)
            throw new ArgumentException("Unexpected Input: Data Item Builder has to start with a level number");

        LevelNumber = Current().value;
        Continue(1);

        Identifier = Current().value;
        Continue(1);

        DataSignature Item = SymbolTable.GetDataItem($"{ProgramBuilder.UnformattedID}#{Identifier}");

        if (LevelNumber.Equals("77"))
        {
            BuildSevenSeven();
            ExportDataItem();
            return;
        }

        if (CurrentEquals("CONSTANT"))
        {
            BuildConstant();
            ExportDataItem();
            return;
        }

        if (!CurrentEquals("CONSTANT") && LevelNumber.Equals("01") || LevelNumber.Equals("1"))
        {
            BuildDataDescriptionEntry();
            ExportDataItem();
            return;
        }
    }

    private void BuildDataDescriptionEntry()
    {
        bool isElementary = false;
        bool isExternal = false;
        bool isSigned = false;
        bool isLevelOne = (LevelNumber.Equals("01") || LevelNumber.Equals("1"));

        while (!CurrentEquals("."))
        {
            if (CurrentEquals("PIC") || CurrentEquals("PICTURE"))
            {
                isElementary = true;
                Continue(1);
                if (CurrentEquals("IS")) Continue(1);

                DataType = Current().value;
                if (CurrentEquals("S9")) isSigned = true;

                Continue(2);

                Length = int.Parse(Current().value);

                if ((DataType.Equals("9") || DataType.Equals("S9")) && LookaheadEquals(2, "V9"))
                    FractionalLength = int.Parse(Lookahead(4).value);
            }

            if ((CurrentEquals("IS") && LookaheadEquals(1, "EXTERNAL")) || CurrentEquals("EXTERNAL"))
            {
                string externalizedName = Identifier;
                isExternal = true;

                if (CurrentEquals("IS"))
                    Continue(1);

                if (LookaheadEquals(1, "EXTERNAL"))
                {
                    Continue(2);
                    externalizedName = FormatIdentifier(Current().value[1..^1]);
                }

                ExternalName = externalizedName;
            }

            if (CurrentEquals("VALUE"))
            {
                Continue(1);
                DataValue = Current().value;
            }

            Continue(1);
        }

        static string ConvertType(string current)
        {
            return current switch
            {
                "X" => "Alphanumeric",
                "A" => "Alphabetic",
                "N" => "National",
                "1" => "OtterkitBoolean",
                "9" => "Numeric",
                "S9" => "Numeric",
                _ => "Error"
            };
        }

        if (Section == CurrentScope.WorkingStorage && isElementary)
            CompiledDataItem = $"\n    private static {ConvertType(DataType)} {FormatIdentifier(Identifier)} = ";

        if (Section == CurrentScope.LocalStorage && isElementary)
            CompiledDataItem = $"\n    private {ConvertType(DataType)} {FormatIdentifier(Identifier)} = ";

        string value;
        int TotalLength = 0;

        if (!DataType.Equals("9") && !DataType.Equals("S9"))
        {
            value = DataValue.Equals(String.Empty) ? "\" \"" : DataValue;
            TotalLength = Length;

            if (isElementary && isExternal)
                CompiledDataItem += $"new(_{FormatIdentifier(Identifier)}.Memory, 0, {Length});";

            if (isElementary && !isExternal)
                CompiledDataItem += $"new({value}u8, 0, {Length}, new byte[{Length}]);"; 
        }

        if (DataType.Equals("9") || DataType.Equals("S9"))
        {
            value = DataValue.Equals(String.Empty) ? "\"0\"" : $"\"{DataValue}\"";

            if (isSigned)
            {
                TotalLength = FractionalLength == 0 ? Length : Length + FractionalLength + 2;
                if (value.IndexOfAny(new char[] { '+', '-' }) != 1) value = value.Insert(1, "+");

                if (isElementary && isExternal)
                    CompiledDataItem += $"new(_{FormatIdentifier(Identifier)}.Memory, 0, {Length}, {FractionalLength}, {isSigned.ToString().ToLower()});";

                if (isElementary && !isExternal)
                    CompiledDataItem += $"new({value}u8, 0, {Length}, {FractionalLength}, new byte[{TotalLength}]);";

            }

            if (!isSigned)
            {
                TotalLength = FractionalLength == 0 ? Length : Length + FractionalLength + 1;

                if (isElementary && isExternal)
                    CompiledDataItem += $"new(_{FormatIdentifier(Identifier)}.Memory, 0, {Length}, {FractionalLength}, {isSigned.ToString().ToLower()});";

                if (isElementary && !isExternal)
                    CompiledDataItem += $"new({value}u8, 0, {Length}, {FractionalLength}, new byte[{TotalLength}]);";
            }
        }


        if (Section == CurrentScope.WorkingStorage && isLevelOne && isExternal)
            CompiledDataItem = $"""
            private static DataItem _{FormatIdentifier(Identifier)} = new(External.Resolver("{Identifier}", {TotalLength}));{CompiledDataItem}
            """;

        if (Section == CurrentScope.LocalStorage && isLevelOne && isExternal)
            CompiledDataItem = $"""
            private DataItem _{FormatIdentifier(Identifier)} = new(External.Resolver("{Identifier}", {TotalLength}));{CompiledDataItem}
            """;

        if (Section == CurrentScope.WorkingStorage && isLevelOne && !isExternal)
            CompiledDataItem = $"""
            private static DataItem _{FormatIdentifier(Identifier)} = new({TotalLength});{CompiledDataItem}
            """;

        if (Section == CurrentScope.LocalStorage && isLevelOne && !isExternal)
            CompiledDataItem = $"""
            private DataItem _{FormatIdentifier(Identifier)} = new({TotalLength});{CompiledDataItem}
            """;

    }

    private void BuildConstant()
    {
        string sectionAccessModifier = string.Empty;

        if (Section == CurrentScope.WorkingStorage)
            CompiledDataItem = $"private static readonly Constant {FormatIdentifier(Identifier)} = ";

        if (Section == CurrentScope.LocalStorage)
            CompiledDataItem = $"private readonly Constant {FormatIdentifier(Identifier)} = ";

        while (!CurrentEquals("AS")) Continue(1);

        Continue(1);

        if (CurrentEquals("LENGTH"))
        {
            Continue(1);
            if (CurrentEquals("OF")) Continue(1);

            string FormattedValue = FormatIdentifier(Current().value);
            CompiledDataItem += $"new(Encoding.UTF8.GetBytes({FormattedValue}.Length.ToString()));";
        }

        if (CurrentEquals("BYTE-LENGTH"))
        {
            Continue(1);
            if (CurrentEquals("OF")) Continue(1);

            string FormattedValue = FormatIdentifier(Current().value);
            CompiledDataItem += $"new(Encoding.UTF8.GetBytes({FormattedValue}.Bytes.Length.ToString()));";
        }

        if (Current().type == TokenType.String)
            CompiledDataItem += $"new({Current().value}u8);";

        if (Current().type == TokenType.Numeric)
            CompiledDataItem += $"new(\"{Current().value}\"u8);";

        Continue(1);

        if (!CurrentEquals("."))
            throw new ArgumentException("Unexpected Input: Constant must end with a separator period");

    }

    private void BuildSevenSeven()
    {
        bool isSigned = false;

        static string dataTypes(Token current)
        {
            return current.value switch
            {
                "X" => "Alphanumeric",
                "A" => "Alphabetic",
                "N" => "National",
                "1" => "OtterkitBoolean",
                "9" => "Numeric",
                "S9" => "Numeric",
                _ => "Error"
            };
        }

        while (!CurrentEquals("."))
        {
            if (CurrentEquals("PIC") || CurrentEquals("PICTURE"))
            {
                Continue(1);
                if (CurrentEquals("IS")) Continue(1);

                DataType = dataTypes(Current());
                if (CurrentEquals("S9")) isSigned = true;

                Continue(2);

                Length = int.Parse(Current().value);

                if (Lookahead(1).value.Equals("V9")) FractionalLength = int.Parse(Lookahead(4).value);

            }

            if (CurrentEquals("VALUE"))
            {
                Continue(1);
                DataValue = Current().value;
            }

            Continue(1);
        }

        if (Section == CurrentScope.WorkingStorage)
            CompiledDataItem = $"private static {DataType} {FormatIdentifier(Identifier)} = ";

        if (Section == CurrentScope.LocalStorage)
            CompiledDataItem = $"private {DataType} {FormatIdentifier(Identifier)} = ";

        string value;
        if (DataType.Equals("Numeric"))
        {
            value = DataValue.Equals(String.Empty) ? "\"0\"" : $"\"{DataValue}\"";
            int TotalLength = FractionalLength == 0 ? Length : Length + FractionalLength + 1;

            if (isSigned)
            {
                if (value.IndexOfAny(new char[] { '+', '-' }) != 1) value = value.Insert(1, "+");

                TotalLength = FractionalLength == 0 ? Length : Length + FractionalLength + 2;

                CompiledDataItem += $"new({value}u8, 0, {Length}, {FractionalLength}, new byte[{TotalLength}]);";
                return;
            }

            CompiledDataItem += $"new({value}u8, 0, {Length}, {FractionalLength}, new byte[{TotalLength}]);";
            return; 
        }

        value = DataValue.Equals(String.Empty) ? "\" \"" : DataValue;

        CompiledDataItem += $"new({value}u8, 0, {Length}, new byte[{Length}]);";

        return;
    }

    // Data item builder helper methods.
    private static string FormatIdentifier(string Identifier)
    {
        string FormattedIdentifier = Identifier;
        FormattedIdentifier = "_" + FormattedIdentifier.Replace("-", "_");
        return FormattedIdentifier;
    }

    bool LookaheadEquals(int lookahead, string stringToCompare)
    {
        return Lookahead(lookahead).value.Equals(stringToCompare, StringComparison.OrdinalIgnoreCase);
    }

    bool CurrentEquals(string stringToCompare)
    {
        return Current().value.Equals(stringToCompare, StringComparison.OrdinalIgnoreCase);
    }
}

public class StatementBuilder
{
    private string CompiledStatement = string.Empty;
    private readonly ProgramBuilder ProgramBuilder;
    private readonly Action<int> Continue;
    private readonly Func<Token> Current;
    private readonly Func<int, Token> Lookahead;


    public StatementBuilder(ProgramBuilder ProgramBuilder, Action<int> Continue, Func<Token> Current, Func<int, Token> Lookahead)
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
        if (CurrentEquals("DISPLAY"))
        {
            DISPLAY();
        }
        else if (CurrentEquals("CALL"))
        {
            CALL();
        }
        else if (CurrentEquals("ACCEPT"))
        {
            ACCEPT();
        }
        else if (CurrentEquals("STOP"))
        {
            STOP();
        }
        else if (CurrentEquals("IF"))
        {
            IF();
        }
    }

    private void DISPLAY()
    {
        CompiledStatement += "Statements.DISPLAY(";
        string displayStrings = string.Empty;
        Continue(1);

        while (Current().type is TokenType.Identifier or TokenType.Numeric or TokenType.String)
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
                displayStrings += $"\"{Current().value}\", ";

            Continue(1);
        }

        if (CurrentEquals("UPON"))
        {
            Continue(1);
            if (CurrentEquals("STANDARD-OUTPUT"))
                CompiledStatement += $"\"{Current().value}\", ";

            if (CurrentEquals("STANDARD-ERROR"))
                CompiledStatement += $"\"{Current().value}\", ";

            Continue(1);
        }
        else if (!CurrentEquals("UPON"))
        {
            CompiledStatement += $"\" \", ";
        }

        if (CurrentEquals("WITH") || CurrentEquals("NO"))
            CompiledStatement += "false, ";

        if (!CurrentEquals("WITH") && !CurrentEquals("NO"))
            CompiledStatement += "true, ";

        CompiledStatement += $"{displayStrings}String.Empty);";
        ExportStatement();
    }

    private void CALL()
    {
        Continue(1);
        string ProgramName = $"{FormatIdentifier(Current().value[1..^1])}";
        CompiledStatement += $"{ProgramName} {ProgramName} = new();";
        CompiledStatement += "\n        Statements.CALL(";
        CompiledStatement += $"() => {ProgramName}.Procedure());";
        ExportStatement();
    }

    private void ACCEPT()
    {
        CompiledStatement += "Statements.ACCEPT(";
        // Statements.ACCEPT(dataItem, from, format)
        Continue(1);
        CompiledStatement += $"{FormatIdentifier(Current().value)}, ";
        Continue(1);

        if (!CurrentEquals("FROM"))
            CompiledStatement += "\"STANDARD-INPUT\");";

        if (CurrentEquals("FROM"))
        {
            Continue(1);
            switch (Current().value.ToUpperInvariant())
            {
                case "STANDARD-INPUT":
                case "COMMAND-LINE":
                    CompiledStatement += $"\"{Current().value}\");";
                    break;

                case "DATE":
                    CompiledStatement += $"\"{Current().value}\"";
                    if (LookaheadEquals(1, "YYYYMMDD"))
                        CompiledStatement += $", \"{Lookahead(1).value}\");";

                    if (!LookaheadEquals(1, "YYYYMMDD"))
                        CompiledStatement += ");";
                    break;

                case "DAY":
                    CompiledStatement += $"\"{Current().value}\"";
                    if (LookaheadEquals(1, "YYYYDDD"))
                        CompiledStatement += $", \"{Lookahead(1).value}\");";

                    if (!LookaheadEquals(1, "YYYYDDD"))
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

    private void IF()
    {
        List<Token> expression = new();
        Continue(1);
        while (!CurrentEquals("THEN") && !CurrentEquals(TokenContext.IsStatement))
        {
            expression.Add(Current());
            Continue(1);
        }

        expression = Analyzer.ShuntingYard(expression, Analyzer.ConditionalPrecedence);

        foreach (var item in expression)
        {
            Console.WriteLine(item);
        }

        var compiledExpression = Analyzer.PostfixToCSharpInfix(expression, Analyzer.ConditionalPrecedence);

        CompiledStatement += $"if ({compiledExpression}) {{";

        ExportStatement();
    }

    private void STOP()
    {
        // Statements.STOP();
        // Statements.STOP(error, status);
        CompiledStatement += "Statements.STOP(";
        Continue(2);

        if (CurrentEquals("."))
        {
            CompiledStatement += ");";
            ExportStatement();
            return;
        }

        if (CurrentEquals("WITH"))
            Continue(1);

        if (CurrentEquals("NORMAL"))
            CompiledStatement += "false, ";

        if (CurrentEquals("ERROR"))
            CompiledStatement += "true, ";

        Continue(1);
        if (CurrentEquals("."))
        {
            CompiledStatement += "\"0\");";
            ExportStatement();
            return;
        }

        if (CurrentEquals("STATUS"))
        {
            Continue(1);
        }

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

    // Statement builder helper methods.
    private static string FormatIdentifier(string Identifier)
    {
        string FormattedIdentifier = Identifier;
        FormattedIdentifier = "_" + FormattedIdentifier.Replace("-", "_");
        return FormattedIdentifier;
    }

    bool LookaheadEquals(int lookahead, string stringToCompare)
    {
        return Lookahead(lookahead).value.Equals(stringToCompare, StringComparison.OrdinalIgnoreCase);
    }

    bool CurrentEquals(string stringToCompare)
    {
        return Current().value.Equals(stringToCompare, StringComparison.OrdinalIgnoreCase);
    }

    bool CurrentEquals(TokenContext contextToCompare)
    {
        return Current().context == contextToCompare;
    }
}