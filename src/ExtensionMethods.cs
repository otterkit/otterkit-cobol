using Otterkit.Types;

namespace Otterkit;

public static class ExtensionMethods
{
    public static bool IsEmpty<T>(this HashSet<T> hashSet)
    {
        return hashSet.Count == 0;
    }

    public static bool ContainsAny<T>(this HashSet<T> hashSet, params T[] TArray)
    {
        foreach (var TItem in TArray)
        {
            if (hashSet.Contains(TItem)) return true;
        }

        return false;
    }

    public static T PeekBehind<T>(this Stack<T> stack)
    {
        var currentT = stack.Pop();
        var previousT = stack.Peek();

        stack.Push(currentT);

        return previousT;
    }

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