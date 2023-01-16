namespace Otterkit;

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
