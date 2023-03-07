namespace OtterkitLibrary;

public interface ICOBOLType
{
    Memory<byte> Memory { get; init; }
    ICOBOLType[] Fields { get; init; }
    ReadOnlySpan<byte> Bytes { get; set; }
    string Display { get; }
}
