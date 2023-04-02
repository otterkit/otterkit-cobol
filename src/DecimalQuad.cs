using System.Runtime.InteropServices;
using System.Text;

namespace Otterkit.Numerics;

[StructLayout(LayoutKind.Sequential)]
public readonly partial struct DecimalQuad
{
    internal readonly ulong _upperBits;
    internal readonly ulong _lowerBits;

    public DecimalQuad(ulong upperBits, ulong lowerBits)
    {
        _upperBits = upperBits;
        _lowerBits = lowerBits;
    }

    public DecimalQuad(ReadOnlySpan<byte> utf8String)
    {
        this = DecQuadBindings.FromString(MemoryMarshal.GetReference(utf8String));
    }

    public DecimalQuad FromString(ReadOnlySpan<byte> utf8String)
    {
        return DecQuadBindings.FromString(MemoryMarshal.GetReference(utf8String));
    }

    public unsafe byte[] ToByteArray()
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

    public ReadOnlySpan<byte> ToSpan()
    {
        return ToByteArray();
    }

    public ReadOnlyMemory<byte> ToMemory()
    {
        return ToByteArray();
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
