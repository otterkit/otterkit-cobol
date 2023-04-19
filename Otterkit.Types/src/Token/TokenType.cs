namespace Otterkit.Types;

public enum TokenType
{
    None,
    Error,
    ReservedKeyword,
    FigurativeLiteral,
    IntrinsicFunction,
    Symbol,
    Picture,
    String,
    HexString,
    Boolean,
    HexBoolean,
    National,
    HexNational,
    Numeric,
    Identifier,
    Expression,
    Device,
    EOF,
}
