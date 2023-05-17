using System.Text;

namespace Otterkit.Runtime;


public readonly struct National : ICOBOLType
{
    public readonly Memory<byte> Memory { get; init; }
    public readonly int Length;
    private readonly int Offset;

    public National(ReadOnlySpan<byte> value, int offset, int length, Memory<byte> memory)
    {
        Length = length;
        Offset = offset;

        Memory = memory.Slice(offset, length);
 
        Memory.Span.Fill(32);

        int byteLength = Length < value.Length
            ? Length
            : value.Length;

        value[..byteLength].CopyTo(Memory.Span);
    }

    public National(Memory<byte> memory, int offset, int length)
    {
        Length = length;
        Offset = offset;

        Memory = memory.Slice(offset, length);
    }

    public National(ReadOnlySpan<byte> national)
    {
        Length = national.Length;
        
        Memory = new byte[Length];
 
        national.CopyTo(Memory.Span);
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
