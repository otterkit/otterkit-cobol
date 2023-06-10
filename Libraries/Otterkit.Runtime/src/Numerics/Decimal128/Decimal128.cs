using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using System.Text;

namespace Otterkit.Numerics;

[StructLayout(LayoutKind.Explicit)]
public unsafe partial struct Decimal128 : IStandardDecimal<Decimal128>
{
    private const int d128Bytes = 16;

    [FieldOffset(0)]
    private fixed byte bytes[d128Bytes];
    
    [FieldOffset(0)]
    private fixed ushort shorts[d128Bytes/2];

    [FieldOffset(0)]
    private fixed uint words[d128Bytes/4];

    [FieldOffset(0)]
    private fixed ulong longs[d128Bytes/8];

    public Decimal128(ulong upperBits, ulong lowerBits)
    {
        longs[0] = upperBits;
        longs[1] = lowerBits;
    }

    public Decimal128(ReadOnlySpan<byte> utf8String)
    {
        this = Decimal128Bindings.FromString(MemoryMarshal.GetReference(utf8String));
    }

    public Decimal128(ReadOnlySpan<char> characters)
    {
        var length = Encoding.UTF8.GetByteCount(characters);

        Span<byte> utf8String = stackalloc byte[length];

        Encoding.UTF8.GetBytes(characters, utf8String);
        
        this = Decimal128Bindings.FromString(MemoryMarshal.GetReference(utf8String));
    }

    public override bool Equals([NotNullWhen(true)] object? obj)
    {
        if (obj == null || GetType() != obj.GetType())
        {
            return false;
        }
        
        if (obj is Decimal128 decQuad)
        {
            return Equals(decQuad);
        }

        return false;
    }

    public bool Equals(Decimal128 decQuad)
    {
        return Decimal128Bindings.Compare(this, decQuad) == 0;
    }
    
    public int CompareTo(object? obj)
    {
        if (obj is null) return 1;
        
        if (obj is Decimal128 decQuad)
        {
            return CompareTo(decQuad);
        }
        
        throw new ArgumentException("CompareTo argument must be a Decimal128", nameof(obj));
    }

    public int CompareTo(Decimal128 other)
    {
        // Call decQuad compare function.
        var compare = Decimal128Bindings.Compare(this, other);

        // decQuad bindings will only return -5 if one
        // of the arguments is NaN.
        if (compare is not -5) return compare;

        // Determine which one is NaN.
        return IsNaN(this) ? (IsNaN(other) ? 0 : -1) : 1;
    }

    public override int GetHashCode()
    {
        // Does this work with this encoding?
        return HashCode.Combine(longs[0], longs[1]);
    }

    public static (ulong UpperBits, ulong LowerBits) Encode(Decimal128 value)
    {
        return (value.longs[0], value.longs[1]);
    }

    public static Decimal128 Decode(ulong upperBits, ulong lowerBits)
    {
        return new Decimal128(upperBits, lowerBits);
    }
}
