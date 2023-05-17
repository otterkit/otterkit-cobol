using System.Text;

namespace Otterkit.Library;


public sealed class National : ICOBOLType
{
    public Memory<byte> Memory { get; init; }
    public ICOBOLType[] Fields { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }

    public National(ReadOnlySpan<byte> value, int offset, int length, Memory<byte> memory, ICOBOLType[]? fields = null)
    {
        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
        this.Fields = fields ?? Array.Empty<ICOBOLType>();
        Memory.Span.Fill(32);

        int byteLength = Length < value.Length
            ? Length
            : value.Length;

        value[..byteLength].CopyTo(Memory.Span);
    }

    public National(Memory<byte> memory, int offset, int length, ICOBOLType[]? fields = null)
    {
        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
        this.Fields = fields ?? Array.Empty<ICOBOLType>();
    }

    public National(ReadOnlySpan<byte> national)
    {
        this.Length = national.Length;
        this.Memory = new byte[Length];
        this.Fields = Array.Empty<ICOBOLType>();
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
