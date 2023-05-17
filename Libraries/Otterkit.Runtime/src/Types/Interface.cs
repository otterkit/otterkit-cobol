namespace Otterkit.Runtime;

public interface ICOBOLType
{
    Memory<byte> Memory { get; init; }
    ReadOnlySpan<byte> Bytes { get; set; }
    string Display { get; }
}
