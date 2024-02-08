namespace Otterkit.Types;

[Flags]
public enum TokenType
{
    None,
    Error,
    ReservedKeyword = 1 << 1,
    Figurative = 1 << 2,
    IntrinsicFunction = 1 << 3,
    Symbol = 1 << 4,
    Picture = 1 << 5,
    String = 1 << 6,
    HexString = 1 << 7,
    Boolean = 1 << 8,
    HexBoolean = 1 << 9,
    National = 1 << 10,
    HexNational = 1 << 11,
    Numeric = 1 << 12,
    Identifier = 1 << 13,
    Expression = 1 << 14,
    Device = 1 << 15,
    EOF = 1 << 16,
}
