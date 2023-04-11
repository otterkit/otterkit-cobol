using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using System.Text;

namespace Otterkit.Numerics;

[StructLayout(LayoutKind.Sequential)]
public readonly partial struct Decimal64 :
    IStandardDecimal<Decimal64>
{
    internal readonly ulong _Bits;

    public Decimal64(ulong bits)
    {
        _Bits = bits;
    }

    public Decimal64(ReadOnlySpan<byte> utf8String)
    {
        this = DecDoubleBindings.FromString(MemoryMarshal.GetReference(utf8String));
    }

    public Decimal64(ReadOnlySpan<char> characters)
    {
        var length = Encoding.UTF8.GetByteCount(characters);

        Span<byte> utf8String = stackalloc byte[length];

        Encoding.UTF8.GetBytes(characters, utf8String);
        
        this = DecDoubleBindings.FromString(MemoryMarshal.GetReference(utf8String));
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
        return DecDoubleBindings.Compare(this, decQuad) == 0;
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
        var compare = DecDoubleBindings.Compare(this, other);

        // decQuad bindings will only return -5 if one
        // of the arguments is NaN.
        if (compare is not -5) return compare;

        // Determine which one is NaN.
        return IsNaN(this) ? (IsNaN(other) ? 0 : -1) : 1;
    }

    public override int GetHashCode()
    {
        // Does this work with this encoding?
        return _Bits.GetHashCode();
    }

    public static ulong Encode(Decimal64 value)
    {
        return value._Bits;
    }

    public static Decimal64 Decode(ulong bits)
    {
        return new Decimal64(bits);
    }
}
