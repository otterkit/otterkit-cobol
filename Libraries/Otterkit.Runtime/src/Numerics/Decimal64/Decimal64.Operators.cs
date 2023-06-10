using System.Runtime.InteropServices;

namespace Otterkit.Numerics;

public partial struct Decimal64
{
    public static Decimal64 operator +(Decimal64 value)
    {
        return Decimal64Bindings.Plus(value);
    }

    public static Decimal64 operator +(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.Add(left, right);
    }

    public static Decimal64 operator ++(Decimal64 value)
    {
        return Decimal64Bindings.Add(value, Decimal64.One);
    }

    public static Decimal64 operator -(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.Subtract(left, right);
    }

    public static Decimal64 operator -(Decimal64 value)
    {
        return Decimal64Bindings.Minus(value);
    }

    public static Decimal64 operator --(Decimal64 value)
    {
        return Decimal64Bindings.Subtract(value, Decimal64.One);
    }

    public static Decimal64 operator /(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.Divide(left, right);
    }

    public static Decimal64 operator %(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.Remainder(left, right);
    }

    public static Decimal64 operator *(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.Multiply(left, right);
    }

    public static bool operator ==(Decimal64 left, Decimal64 right)
    {
        var compare = Decimal64Bindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is 0;
    }

    public static bool operator !=(Decimal64 left, Decimal64 right)
    {
        var compare = Decimal64Bindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is not 0;
    }

    public static bool operator <(Decimal64 left, Decimal64 right)
    {
        var compare = Decimal64Bindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is -1;
    }

    public static bool operator <=(Decimal64 left, Decimal64 right)
    {
        var compare = Decimal64Bindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is -1 or 0;
    }

    public static bool operator >(Decimal64 left, Decimal64 right)
    {
        var compare = Decimal64Bindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is 1;
    }

    public static bool operator >=(Decimal64 left, Decimal64 right)
    {
        var compare = Decimal64Bindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is 1 or 0;
    }

    public static implicit operator Decimal64(int value)
    {
        return Decimal64Bindings.FromInt32(value);
    }

    public static explicit operator Decimal64(ReadOnlySpan<byte> value)
    {
        return Decimal64Bindings.FromString(MemoryMarshal.GetReference(value));
    }
}