namespace Otterkit.Runtime;

public interface ICOBOLType
{
    ReadOnlySpan<byte> Bytes { get; set; }
    string Display { get; }
}

public interface INumeric
{
    byte Extra { get; init; }
    byte Integer { get; init; }
    byte Fractional { get; init; }

    bool IsInteger { get; init; }
    bool IsSigned { get; init; }
    bool IsNegative { get; }
}
