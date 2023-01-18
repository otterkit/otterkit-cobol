namespace Otterkit;

public static class EnumExtensions
{
    public static string Display(this UsageType usage)
    {
        return usage switch
        {
            UsageType.Binary => "BINARY",
            UsageType.BinaryChar => "BINARY-CHAR",
            UsageType.BinaryShort => "BINARY-SHORT",
            UsageType.BinaryLong => "BINARY-LONG",
            UsageType.BinaryDouble => "BINARY-DOUBLE",
            UsageType.Bit => "BIT",
            UsageType.Computational => "COMPUTATIONAL",
            UsageType.Display => "DISPLAY",
            UsageType.FloatBinary32 => "FLOAT-BINARY-32",
            UsageType.FloatBinary64 => "FLOAT-BINARY-64",
            UsageType.FloatBinary128 => "FLOAT-BINARY-128",
            UsageType.FloatDecimal16 => "FLOAT-DECIMAL-16",
            UsageType.FloatDecimal32 => "FLOAT-DECIMAL-32",
            UsageType.FloatExtended => "FLOAT-EXTENDED",
            UsageType.FloatLong => "FLOAT-LONG",
            UsageType.FloatShort => "FLOAT-SHORT",
            UsageType.Index => "INDEX",
            UsageType.MessageTag => "MESSAGE-TAG",
            UsageType.National => "NATIONAL",
            UsageType.ObjectReference => "OBJECT REFERENCE",
            UsageType.PackedDecimal => "PACKED-DECIMAL",
            UsageType.DataPointer => "POINTER",
            UsageType.FunctionPointer => "FUNCTION-POINTER",
            UsageType.ProgramPointer => "PROGRAM-POINTER",
            _ => "NONE"
        };
    }
}

public enum DirectiveType
{
    SourceFormat,
    None,
}

public enum SourceUnit
{
    Program,
    ProgramPrototype,
    Function,
    FunctionPrototype,
    Class,
    Interface,
    Method,
    MethodPrototype,
    MethodGetter,
    MethodSetter,
    Object,
    Factory
}

public enum CurrentScope
{
    ProgramId,
    FunctionId,
    InterfaceId,
    ClassId,
    MethodId,
    EnvironmentDivision,
    Repository,
    DataDivision,
    WorkingStorage,
    LocalStorage,
    LinkageSection,
    ProcedureDivision,
    Factory,
    Object,
}

public enum UsageType
{
    None,
    Binary,
    BinaryChar,
    BinaryShort,
    BinaryLong,
    BinaryDouble,
    Bit,
    Computational,
    Display,
    FloatBinary32,
    FloatBinary64,
    FloatBinary128,
    FloatDecimal16,
    FloatDecimal32,
    FloatExtended,
    FloatLong,
    FloatShort,
    Index,
    MessageTag,
    National,
    ObjectReference,
    PackedDecimal,
    DataPointer,
    FunctionPointer,
    ProgramPointer
}

public enum TokenType
{
    ReservedKeyword,
    FigurativeLiteral,
    IntrinsicFunction,
    Symbol,
    String,
    Numeric,
    Identifier,
    Expression,
    Device,
    EOF,
}

public enum TokenScope
{
    ProgramId,
    FunctionId,
    InterfaceId,
    ClassId,
    MethodId,
    Factory,
    Object,
    EnvironmentDivision,
    DataDivision,
    ProcedureDivision
}

public enum TokenContext
{
    IsClause,
    IsStatement,
    IsResolved,
    IsNotResolved,
    IsEOF
}

public enum ErrorType
{
    General,
    Expected,
    Choice,
    Recovery,
}

public enum EvaluateOperand
{
    Identifier,
    Literal,
    Arithmetic,
    Boolean,
    Range,
    Condition,
    TrueOrFalse,
    Any,
    Invalid
}
