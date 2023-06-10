using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using System.Text;

namespace Otterkit.Numerics;

[StructLayout(LayoutKind.Explicit)]
public unsafe partial struct Decimal64 : IStandardDecimal<Decimal64>
{
    private const int d64Bytes = 8;

    [FieldOffset(0)]
    private fixed byte bytes[d64Bytes];
    
    [FieldOffset(0)]
    private fixed ushort shorts[d64Bytes/2];

    [FieldOffset(0)]
    private fixed uint words[d64Bytes/4];

    [FieldOffset(0)]
    private fixed ulong longs[d64Bytes/8];

    public Decimal64(ulong bits)
    {
        longs[0] = bits;
    }

    public Decimal64(ReadOnlySpan<byte> utf8String)
    {
        this = Decimal64Bindings.FromString(MemoryMarshal.GetReference(utf8String));
    }

    public Decimal64(ReadOnlySpan<char> characters)
    {
        var length = Encoding.UTF8.GetByteCount(characters);

        Span<byte> utf8String = stackalloc byte[length];

        Encoding.UTF8.GetBytes(characters, utf8String);
        
        this = Decimal64Bindings.FromString(MemoryMarshal.GetReference(utf8String));
    }

    public override bool Equals([NotNullWhen(true)] object? obj)
    {
        if (obj == null || GetType() != obj.GetType())
        {
            return false;
        }
        
        if (obj is Decimal64 decQuad)
        {
            return Equals(decQuad);
        }

        return false;
    }

    public bool Equals(Decimal64 decQuad)
    {
        return Decimal64Bindings.Compare(this, decQuad) == 0;
    }
    
    public int CompareTo(object? obj)
    {
        if (obj is null) return 1;
        
        if (obj is Decimal64 decQuad)
        {
            return CompareTo(decQuad);
        }
        
        throw new ArgumentException("CompareTo argument must be a Decimal64", nameof(obj));
    }

    public int CompareTo(Decimal64 other)
    {
        // Call decQuad compare function.
        var compare = Decimal64Bindings.Compare(this, other);

        // decQuad bindings will only return -5 if one
        // of the arguments is NaN.
        if (compare is not -5) return compare;

        // Determine which one is NaN.
        return IsNaN(this) ? (IsNaN(other) ? 0 : -1) : 1;
    }

    public override int GetHashCode()
    {
        // Does this work with this encoding?
        return longs[0].GetHashCode();
    }

    public static ulong Encode(Decimal64 value)
    {
        return value.longs[0];
    }

    public static Decimal64 Decode(ulong bits)
    {
        return new Decimal64(bits);
    }
}
