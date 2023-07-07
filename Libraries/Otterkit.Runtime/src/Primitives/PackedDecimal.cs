using System.Runtime.InteropServices;
using Otterkit.Numerics;

namespace Otterkit.Runtime;

[StructLayout(LayoutKind.Sequential)]
public unsafe struct PackedDecimal : ICOBOLType
{
    internal fixed byte Nibbles[18];
    public int Exponent;

    public bool IsNegative => (Nibbles[17] & 0b00000001) == 0;

    public PackedDecimal(Decimal128 value)
    {
        fixed (PackedDecimal* pointer = &this)
        {
            Decimal128.ToPacked(value, ref pointer->Exponent, ref *pointer->Nibbles);
        }
    }

    public static explicit operator Decimal128(PackedDecimal value)
    {
        return Decimal128.FromPacked(value.Exponent, in *value.Nibbles);
    }

    public static explicit operator PackedDecimal(Decimal128 value)
    {
        return new(value);
    }

    public ReadOnlySpan<byte> Bytes
    { 
        get
        {
            Span<byte> span = stackalloc byte[45];

            var length = ((Decimal128)this).AsSpan(span);

            var bytes = new byte[length];

            span.CopyTo(bytes);

            return bytes;
        } 

        set
        {
            var dec = Decimal128.Parse(value);

            fixed (PackedDecimal* pointer = &this)
            {
                Decimal128.ToPacked(dec, ref pointer->Exponent, ref *pointer->Nibbles);
            }
        } 
    }

    public string Display => ((Decimal128)this).ToString();
}
