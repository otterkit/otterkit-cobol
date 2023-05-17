using System.Text;

namespace Otterkit.Runtime;

public readonly struct Alphabetic : ICOBOLType
{
    public readonly Memory<byte> Memory { get; init; }
    public readonly int Length;
    private readonly int Offset;

    public Alphabetic(ReadOnlySpan<byte> value, int offset, int length, Memory<byte> memory)
    {
        Offset = offset;
        Length = length;
        Memory = memory.Slice(offset, length);
        
        Memory.Span.Fill(32);

        int byteLength = Length < value.Length
            ? Length
            : value.Length;

        value[..byteLength].CopyTo(Memory.Span);
    }

    public Alphabetic(Memory<byte> memory, int offset, int length)
    {
        Offset = offset;
        Length = length;
        Memory = memory.Slice(offset, length);
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
