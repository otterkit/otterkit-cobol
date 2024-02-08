using static Otterkit.Types.CompilerContext;
using static Otterkit.Types.TokenHandling;
using Otterkit.Analyzers;
using Otterkit.Types;
using System.Text;

namespace Otterkit.CodeGenerators;

public class VariableBuilder
{
    private readonly StringBuilder Compiled = new();
    private string Identifier = string.Empty;
    private string DataType = string.Empty;
    private int Length = 0;
    private int FractionalLength = 0;
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

        if (variable.LevelNumber is 77 or > 0 and < 50)
        {
            BuildVariable(variable, token);
            ExportVariable();
            return;
        }

        if (variable.IsConstant)
        {
            BuildConstant();
            ExportVariable();
            return;
        }
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

    private static int Offset = 0;
    private static string GroupName = string.Empty;

    private void BuildVariable(DataEntry variable, Token token)
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
            _ => "Alphanumeric",
        };

        while (!CurrentEquals(".")) Continue();

        var length = variable.Length;

        var formatted = FormatIdentifier(token.Value);

        if (variable.LevelNumber is 1) Offset = 0;

        if (variable.LevelNumber is 1 && variable.IsGroup)
        {
            Compiled.Append($"public static OtterMemory _{formatted} = new({length});");

            GroupName = formatted;
        }

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

            if (type is "Alphabetic" or "Alphanumeric" or "National" or "Bit" && !variable.IsGroup)
            {
                Compiled.Append($"new({value}u8, new({length}), {Offset}, {length});");

                Offset += length;

                return;
            }

            if (type is "Alphanumeric" or "National" or "Bit" && variable.IsGroup)
            {
                Compiled.Append($"new(_{formatted}, {Offset}, {length});");

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
            if (type is "Alphabetic" or "Alphanumeric" or "National" or "Bit" && !variable.IsGroup && variable.LevelNumber is 1)
            {
                Compiled.Append($"new(\" \"u8, new({length}), {Offset}, {length});");
                return;
            }

            if (type is "Alphabetic" or "Alphanumeric" or "National" or "Bit" && !variable.IsGroup)
            {
                Compiled.Append($"new(\" \"u8, _{GroupName}, {Offset}, {length});");

                Offset += length;

                return;
            }

            if (type is "Alphanumeric" or "National" or "Bit" && variable.IsGroup)
            {
                Compiled.Append($"new(_{GroupName}, {Offset}, {length});");

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
