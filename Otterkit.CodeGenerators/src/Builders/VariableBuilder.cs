using static Otterkit.Types.CompilerContext;
using static Otterkit.Types.TokenHandling;
using Otterkit.Analyzers;
using Otterkit.Types;
using System.Text;

namespace Otterkit.CodeGenerators;

public class VariableBuilder
{
    private readonly StringBuilder Compiled = new();
    private string LevelNumber = string.Empty;
    private string Identifier = string.Empty;
    private string ExternalName = string.Empty;
    private string DataType = string.Empty;
    private int Length = 0;
    private int FractionalLength = 0;
    private string DataValue = string.Empty;
    private SourceScope Section;
    private readonly ProgramBuilder Builder;

    public VariableBuilder(ProgramBuilder builder)
    {
        Builder = builder;
    }

    public void ExportVariable()
    {
        if (Section == SourceScope.WorkingStorage)
            Builder.Append(Compiled);

        if (Section == SourceScope.LocalStorage)
            Builder.Append(Compiled);
    }

    public void BuildVariable(SourceScope section = SourceScope.WorkingStorage)
    {
        Section = section;

        var token = Peek(1);

        var id = Builder.Name.Unwrap();

        var callable = ActiveNames.Fetch<CallableUnit>(id);

        var variable = callable.DataNames.FetchUnique(token);

        if (variable.LevelNumber is 77)
        {
            Build77(variable, token);
            ExportVariable();
            return;
        }

        if (variable.IsConstant)
        {
            BuildConstant();
            ExportVariable();
            return;
        }

        if (variable.LevelNumber is 1)
        {
            BuildDataEntry();
            ExportVariable();
            return;
        }
    }

    private void BuildDataEntry()
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

                DataType = Current().Value;
                if (CurrentEquals("S9")) isSigned = true;

                Continue(2);

                Length = int.Parse(Current().Value);

                if ((DataType.Equals("9") || DataType.Equals("S9")) && PeekEquals(2, "V9"))
                    FractionalLength = int.Parse(Peek(4).Value);
            }

            if ((CurrentEquals("IS") && PeekEquals(1, "EXTERNAL")) || CurrentEquals("EXTERNAL"))
            {
                string externalizedName = Identifier;
                isExternal = true;

                if (CurrentEquals("IS"))
                    Continue(1);

                if (PeekEquals(1, "EXTERNAL"))
                {
                    Continue(2);
                    externalizedName = FormatIdentifier(Current().Value[1..^1]);
                }

                ExternalName = externalizedName;
            }

            if (CurrentEquals("VALUE"))
            {
                Continue(1);
                DataValue = Current().Value;
            }

            Continue(1);
        }

        static string ConvertType(ReadOnlySpan<char> current)
        {
            return current[0] switch
            {
                'X' => "Alphanumeric",
                'A' => "Alphabetic",
                'N' => "National",
                '1' => "Bit",
                '9' => "Numeric",
                'S' => "Numeric",
                _ => "Error"
            };
        }

        if (Section == SourceScope.WorkingStorage && isElementary)
        {
            Compiled.Append($"private static {ConvertType(DataType)} {FormatIdentifier(Identifier)} = ");
        }

        if (Section == SourceScope.LocalStorage && isElementary)
        {
            Compiled.Append($"private {ConvertType(DataType)} {FormatIdentifier(Identifier)} = ");
        }

        string value;
        int TotalLength = 0;

        if (!DataType.Equals("9") && !DataType.Equals("S9"))
        {
            value = DataValue.Equals(String.Empty) ? "\" \"" : DataValue;
            TotalLength = Length;

            if (isElementary && isExternal)
                Compiled.Append($"new(_{FormatIdentifier(Identifier)}.Memory, 0, {Length});");

            if (isElementary && !isExternal)
                Compiled.Append($"new({value}u8, 0, {Length}, new byte[{Length}]);");
        }

        if (DataType.Equals("9") || DataType.Equals("S9"))
        {
            value = DataValue.Equals(String.Empty) ? "\"0\"" : $"\"{DataValue}\"";

            if (isSigned)
            {
                TotalLength = FractionalLength == 0 ? Length : Length + FractionalLength + 2;
                if (value.IndexOfAny(new char[] { '+', '-' }) != 1) value = value.Insert(1, "+");

                if (isElementary && isExternal)
                    Compiled.Append($"new(_{FormatIdentifier(Identifier)}.Memory, 0, {Length}, {FractionalLength}, {isSigned.ToString().ToLower()});");

                if (isElementary && !isExternal)
                    Compiled.Append($"new({value}u8, 0, {Length}, {FractionalLength}, new byte[{TotalLength}]);");

            }

            if (!isSigned)
            {
                TotalLength = FractionalLength == 0 ? Length : Length + FractionalLength + 1;

                if (isElementary && isExternal)
                    Compiled.Append($"new(_{FormatIdentifier(Identifier)}.Memory, 0, {Length}, {FractionalLength}, {isSigned.ToString().ToLower()});");

                if (isElementary && !isExternal)
                    Compiled.Append($"new({value}u8, 0, {Length}, {FractionalLength}, new byte[{TotalLength}]);");
            }
        }

        if (Section == SourceScope.WorkingStorage && isLevelOne && isExternal)
            Compiled.Append($"""private static DataItem _{FormatIdentifier(Identifier)} = new(External.Resolver("{Identifier}", {TotalLength}));{Compiled}""");

        if (Section == SourceScope.LocalStorage && isLevelOne && isExternal)
            Compiled.Append($"""private DataItem _{FormatIdentifier(Identifier)} = new(External.Resolver("{Identifier}", {TotalLength}));{Compiled}""");

        if (Section == SourceScope.WorkingStorage && isLevelOne && !isExternal)
            Compiled.Append($"""private static DataItem _{FormatIdentifier(Identifier)} = new({TotalLength});{Compiled}""");

        if (Section == SourceScope.LocalStorage && isLevelOne && !isExternal)
            Compiled.Append($"""private DataItem _{FormatIdentifier(Identifier)} = new({TotalLength});{Compiled}""");

    }

    private void BuildConstant()
    {
        string sectionAccessModifier = string.Empty;

        if (Section == SourceScope.WorkingStorage)
            Compiled.Append($"private static readonly Constant {FormatIdentifier(Identifier)} = ");

        if (Section == SourceScope.LocalStorage)
            Compiled.Append($"private readonly Constant {FormatIdentifier(Identifier)} = ");

        while (!CurrentEquals("AS")) Continue(1);

        Continue(1);

        if (CurrentEquals("LENGTH"))
        {
            Continue(1);
            if (CurrentEquals("OF")) Continue(1);

            string FormattedValue = FormatIdentifier(Current().Value);
            Compiled.Append($"new(Encoding.UTF8.GetBytes({FormattedValue}.Length.ToString()));");
        }

        if (CurrentEquals("BYTE-LENGTH"))
        {
            Continue(1);
            if (CurrentEquals("OF")) Continue(1);

            string FormattedValue = FormatIdentifier(Current().Value);
            Compiled.Append($"new(Encoding.UTF8.GetBytes({FormattedValue}.Bytes.Length.ToString()));");
        }

        if (Current().Type == TokenType.String)
            Compiled.Append($"new({Current().Value}u8);");

        if (Current().Type == TokenType.Numeric)
            Compiled.Append($"new(\"{Current().Value}\"u8);");

        Continue(1);

        if (!CurrentEquals("."))
            throw new ArgumentException("Unexpected Input: Constant must end with a separator period");

    }

    private void Build77(DataEntry variable, Token token)
    {
        bool isSigned = false;

        var type = variable.Class switch
        {
            Classes.Alphabetic => "Alphabetic",
            Classes.Alphanumeric => "Alphanumeric",
            Classes.Boolean => "Bit",
            Classes.Index => "Index",
            Classes.MessageTag => "MessageTag",
            Classes.National => "National",
            Classes.Numeric => "Numeric",
            Classes.Object => "ObjectReference",
            Classes.Pointer => "Pointer",
            _ => throw new ArgumentException("Invalid Type"),
        };

        while (!CurrentEquals(".")) Continue(1);

        var formatted = FormatIdentifier(token.Value);

        if (Section == SourceScope.WorkingStorage)
        {
            Compiled.Append($"private static {type} {formatted} = ");
        }

        if (Section == SourceScope.LocalStorage)
        {
            Compiled.Append($"private {DataType} {formatted} = ");
        }

        var hasValue = variable[DataClause.Value];

        if (hasValue)
        {
            ReadOnlySpan<char> value = variable.FetchValue().Value;

            if (type is "Alphabetic" or "Alphanumeric" or "National" or "Bit")
            {
                Compiled.Append($"new({value}u8, new(20), 0, 20);");
                return;
            }

            if (type is "Numeric")
            {
                int TotalLength = FractionalLength == 0 ? Length : Length + FractionalLength + 1;

                if (isSigned)
                {
                    if (value.IndexOfAny("+-") != 1) { };

                    TotalLength = FractionalLength == 0 ? Length : Length + FractionalLength + 2;

                    Compiled.Append($"new({value}u8, 0, {Length}, {FractionalLength}, new byte[{TotalLength}]);");
                    return;
                }

                Compiled.Append($"new({value}u8, 0, {Length}, {FractionalLength}, new byte[{TotalLength}]);");
                return;
            }

            if (type is "ObjectReference")
            {
                Compiled.Append($"new({value}u8, new({Length}), 0, {Length});");
                return;
            }

            if (type is "Index")
            {
                Compiled.Append($"new({value}u8, 0, {Length}, new byte[{Length}]);");
                return;
            }

            if (type is "MessageTag")
            {
                Compiled.Append($"new({value}u8, 0, {Length}, new byte[{Length}]);");
                return;
            }

            if (type is "Pointer")
            {
                Compiled.Append($"new({value}u8, 0, {Length}, new byte[{Length}]);");
                return;
            }
        }
        else
        {
            if (type is "Alphabetic" or "Alphanumeric" or "National" or "Bit")
            {
                Compiled.Append($"new(\" \"u8, new({Length}), 0, {Length});");
                return;
            }

            if (type is "Numeric")
            {
                int TotalLength = FractionalLength == 0 ? Length : Length + FractionalLength + 1;

                if (isSigned)
                {
                    TotalLength = FractionalLength == 0 ? Length : Length + FractionalLength + 2;

                    Compiled.Append($"new(\" \"u8, 0, {Length}, {FractionalLength}, new byte[{TotalLength}]);");
                    return;
                }

                Compiled.Append($"new(\" \"u8, 0, {Length}, {FractionalLength}, new byte[{TotalLength}]);");
                return;
            }

            if (type is "ObjectReference")
            {
                Compiled.Append($"new(\" \"u8, new({Length}), 0, {Length});");
                return;
            }

            if (type is "Index")
            {
                Compiled.Append($"new(\" \"u8, 0, {Length}, new byte[{Length}]);");
                return;
            }

            if (type is "MessageTag")
            {
                Compiled.Append($"new(\" \"u8, 0, {Length}, new byte[{Length}]);");
                return;
            }

            if (type is "Pointer")
            {
                Compiled.Append($"new(\" \"u8, 0, {Length}, new byte[{Length}]);");
                return;
            }
        }
    }

    public string FormatIdentifier(string Identification)
    {
        return string.Create(Identification.Length + 1, Identification, (span, value) =>
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
}
