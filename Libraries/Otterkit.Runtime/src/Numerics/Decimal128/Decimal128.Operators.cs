using System.Numerics;
using System.Runtime.InteropServices;

namespace Otterkit.Numerics;

public partial struct Decimal128
{
    public static Decimal128 operator +(Decimal128 value)
    {
        return Decimal128Bindings.Plus(value);
    }

    public static Decimal128 operator +(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.Add(left, right);
    }

    public static Decimal128 operator ++(Decimal128 value)
    {
        return Decimal128Bindings.Add(value, Decimal128.One);
    }

    public static Decimal128 operator -(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.Subtract(left, right);
    }

    public static Decimal128 operator -(Decimal128 value)
    {
        return Decimal128Bindings.Minus(value);
    }

    public static Decimal128 operator --(Decimal128 value)
    {
        return Decimal128Bindings.Subtract(value, Decimal128.One);
    }

    public static Decimal128 operator /(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.Divide(left, right);
    }

    public static Decimal128 operator %(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.Remainder(left, right);
    }

    public static Decimal128 operator *(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.Multiply(left, right);
    }

    public static bool operator ==(Decimal128 left, Decimal128 right)
    {
        var compare = Decimal128Bindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is 0;
    }

    public static bool operator !=(Decimal128 left, Decimal128 right)
    {
        var compare = Decimal128Bindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is not 0;
    }

    public static bool operator <(Decimal128 left, Decimal128 right)
    {
        var compare = Decimal128Bindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is -1;
    }

    public static bool operator <=(Decimal128 left, Decimal128 right)
    {
        var compare = Decimal128Bindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is -1 or 0;
    }

    public static bool operator >(Decimal128 left, Decimal128 right)
    {
        var compare = Decimal128Bindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is 1;
    }

    public static bool operator >=(Decimal128 left, Decimal128 right)
    {
        var compare = Decimal128Bindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is 1 or 0;
    }

    public static implicit operator Decimal128(int value)
    {
        return Decimal128Bindings.FromInt32(value);
    }

    public static explicit operator Decimal128(ReadOnlySpan<byte> value)
    {
        return Decimal128Bindings.FromString(MemoryMarshal.GetReference(value));
    }
}