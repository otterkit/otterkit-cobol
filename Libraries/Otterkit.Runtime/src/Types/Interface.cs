namespace Otterkit.Runtime;

public interface ICOBOLType
{
    ReadOnlySpan<byte> Bytes { get; set; }
    string Display { get; }
}
