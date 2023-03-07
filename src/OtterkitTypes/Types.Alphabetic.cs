using System.Text;

namespace OtterkitLibrary;

public sealed class Alphabetic : ICOBOLType
{
    public Memory<byte> Memory { get; init; }
    public ICOBOLType[] Fields { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }

    public Alphabetic(ReadOnlySpan<byte> value, int offset, int length, Memory<byte> memory)
    {
        this.Fields = Array.Empty<ICOBOLType>();
        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
        Memory.Span.Fill(32);

        int byteLength = Length < value.Length
            ? Length
            : value.Length;

        value[..byteLength].CopyTo(Memory.Span);
    }

    public Alphabetic(Memory<byte> memory, int offset, int length)
    {
        this.Fields = Array.Empty<ICOBOLType>();
        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            return Memory.Span;
        }
        set
        {
            Memory.Span.Fill(32);

            int length = Length < value.Length
            ? Length
            : value.Length;

            value[..length].CopyTo(Memory.Span);
        }
    }

    public string Display
    {
        get
        {
            return Encoding.UTF8.GetString(Memory.Span);
        }
    }
}
