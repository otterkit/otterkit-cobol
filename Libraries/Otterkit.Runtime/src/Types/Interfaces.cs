namespace Otterkit.Runtime;

public interface ICOBOLType
{
    ReadOnlySpan<byte> Bytes { get; set; }
    string Display { get; }
}

public interface INumeric
{
    (int Integer, int Fractional) Length { get; init; }

    bool IsInteger { get; init; }
    bool IsSigned { get; init; }
    bool IsNegative { get; }
}
