using Otterkit.Types;

namespace Otterkit.Analyzers;

public static class ExtensionMethods
{
    public static string Display(this Usages usage)
    {
        return usage switch
        {
            Usages.Binary => "BINARY",
            Usages.BinaryChar => "BINARY-CHAR",
            Usages.BinaryShort => "BINARY-SHORT",
            Usages.BinaryLong => "BINARY-LONG",
            Usages.BinaryDouble => "BINARY-DOUBLE",
            Usages.Bit => "BIT",
            Usages.Computational => "COMPUTATIONAL",
            Usages.Display => "DISPLAY",
            Usages.FloatBinary32 => "FLOAT-BINARY-32",
            Usages.FloatBinary64 => "FLOAT-BINARY-64",
            Usages.FloatBinary128 => "FLOAT-BINARY-128",
            Usages.FloatDecimal16 => "FLOAT-DECIMAL-16",
            Usages.FloatDecimal32 => "FLOAT-DECIMAL-32",
            Usages.FloatExtended => "FLOAT-EXTENDED",
            Usages.FloatLong => "FLOAT-LONG",
            Usages.FloatShort => "FLOAT-SHORT",
            Usages.Index => "INDEX",
            Usages.MessageTag => "MESSAGE-TAG",
            Usages.National => "NATIONAL",
            Usages.ObjectReference => "OBJECT REFERENCE",
            Usages.PackedDecimal => "PACKED-DECIMAL",
            Usages.DataPointer => "POINTER",
            Usages.FunctionPointer => "FUNCTION-POINTER",
            Usages.ProgramPointer => "PROGRAM-POINTER",
            _ => "NONE"
        };
    }

    public static string Display(this TokenType tokenType, bool uppercased)
    {
        return (tokenType, uppercased) switch
        {
            (TokenType.Identifier, true) => "Identifier",
            (TokenType.Identifier, false) => "identifier",

            (TokenType.ReservedKeyword, true) => "Reserved word",
            (TokenType.ReservedKeyword, false) => "reserved word",

            (TokenType.IntrinsicFunction, true) => "Intrinsic function",
            (TokenType.IntrinsicFunction, false) => "intrinsic function",

            (TokenType.String, true) => "Alphanumeric literal",
            (TokenType.String, false) => "alphanumeric literal",
            
            (TokenType.HexString, true) => "Hexadecimal alphanumeric literal",
            (TokenType.HexString, false) => "hexadecimal alphanumeric literal",

            (TokenType.National, true) => "National literal",
            (TokenType.National, false) => "national literal",

            (TokenType.HexNational, true) => "Hexadecimal national literal",
            (TokenType.HexNational, false) => "hexadecimal national literal",

            (TokenType.Boolean, true) => "Boolean literal",
            (TokenType.Boolean, false) => "boolean literal",

            (TokenType.HexBoolean, true) => "Hexadecimal boolean literal",
            (TokenType.HexBoolean, false) => "hexadecimal boolean literal",

            (TokenType.Numeric, true) => "Numeric literal",
            (TokenType.Numeric, false) => "numeric literal",

            (TokenType.Symbol, true) => "Symbol",
            (TokenType.Symbol, false) => "symbol",

            (TokenType.Device, true) => "Device name",
            (TokenType.Device, false) => "device name",

            (TokenType.EOF, true) => "End of file",
            (TokenType.EOF, false) => "end of file",

            (TokenType.Expression, true) => "Expression",
            (TokenType.Expression, false) => "expression",
            _ => "Error"
        };
    }
}
