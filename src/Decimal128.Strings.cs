using System.Runtime.InteropServices;
using System.Text;

namespace Otterkit.Numerics;

public readonly partial struct Decimal128
{
    public static Decimal128 FromString(ReadOnlySpan<byte> utf8String)
    {
        return DecQuadBindings.FromString(MemoryMarshal.GetReference(utf8String));
    }

    public unsafe byte[] ToUTF8Array()
    {
        var pointer = DecQuadBindings.ToString(this);
    
        int length = 0;
        byte current = pointer[0];

        while (current != 0)
        {
            current = pointer[length];
            length++;
        }

        var span = new ReadOnlySpan<byte> (pointer, length);
        var copy = new byte[length];
        
        span.CopyTo(copy);
        
        return copy;        
    }

    public ReadOnlySpan<byte> ToUTF8Span()
    {
        return ToUTF8Array();
    }

    public ReadOnlyMemory<byte> ToUTF8Memory()
    {
        return ToUTF8Array();
    }

    public unsafe ReadOnlySpan<byte> AsUnsafeSpan()
    {
        var pointer = DecQuadBindings.ToString(this);
    
        int length = 0;
        byte current = pointer[0];

        while (current != 0)
        {
            current = pointer[length];
            length++;
        }

        return new ReadOnlySpan<byte> (pointer, length);        
    }


    public override unsafe string ToString()
    {
        var pointer = DecQuadBindings.ToString(this);

        int length = 0;
        byte current = pointer[0];

        while (current != 0)
        {
            current = pointer[length];
            length++;
        }

        var outString = Encoding.UTF8.GetString(pointer, length);

        return outString;
    }

    public unsafe string ToEngineeringString()
    {
        var pointer = DecQuadBindings.ToEngineeringString(this);

        int length = 0;
        byte current = pointer[0];

        while (current != 0)
        {
            current = pointer[length];
            length++;
        }

        var outString = Encoding.UTF8.GetString(pointer, length);

        return outString;
    }
}
