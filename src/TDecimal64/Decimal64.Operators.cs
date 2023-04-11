using System.Runtime.InteropServices;

namespace Otterkit.Numerics;

public readonly partial struct Decimal64
{
    public static Decimal64 operator +(Decimal64 value)
    {
        return DecDoubleBindings.Plus(value);
    }

    public static Decimal64 operator +(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.Add(left, right);
    }

    public static Decimal64 operator ++(Decimal64 value)
    {
        return DecDoubleBindings.Add(value, Decimal64.One);
    }

    public static Decimal64 operator -(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.Subtract(left, right);
    }

    public static Decimal64 operator -(Decimal64 value)
    {
        return DecDoubleBindings.Minus(value);
    }

    public static Decimal64 operator --(Decimal64 value)
    {
        return DecDoubleBindings.Subtract(value, Decimal64.One);
    }

    public static Decimal64 operator /(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.Divide(left, right);
    }

    public static Decimal64 operator %(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.Remainder(left, right);
    }

    public static Decimal64 operator *(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.Multiply(left, right);
    }

    public static bool operator ==(Decimal64 left, Decimal64 right)
    {
        var compare = DecDoubleBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is 0;
    }

    public static bool operator !=(Decimal64 left, Decimal64 right)
    {
        var compare = DecDoubleBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is not 0;
    }

    public static bool operator <(Decimal64 left, Decimal64 right)
    {
        var compare = DecDoubleBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is -1;
    }

    public static bool operator <=(Decimal64 left, Decimal64 right)
    {
        var compare = DecDoubleBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is -1 or 0;
    }

    public static bool operator >(Decimal64 left, Decimal64 right)
    {
        var compare = DecDoubleBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is 1;
    }

    public static bool operator >=(Decimal64 left, Decimal64 right)
    {
        var compare = DecDoubleBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is 1 or 0;
    }

    public static implicit operator Decimal64(int value)
    {
        return DecDoubleBindings.FromInt32(value);
    }

    public static explicit operator Decimal64(ReadOnlySpan<byte> value)
    {
        return DecDoubleBindings.FromString(MemoryMarshal.GetReference(value));
    }
}