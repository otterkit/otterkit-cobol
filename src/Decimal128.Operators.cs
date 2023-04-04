using System.Numerics;

namespace Otterkit.Numerics;

public readonly partial struct Decimal128
{
    public static Decimal128 operator +(Decimal128 value)
    {
        return DecQuadBindings.Plus(value);
    }

    public static Decimal128 operator +(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.Add(left, right);
    }

    public static Decimal128 operator ++(Decimal128 value)
    {
        return DecQuadBindings.Add(value, Decimal128.One);
    }

    public static Decimal128 operator -(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.Subtract(left, right);
    }

    public static Decimal128 operator -(Decimal128 value)
    {
        return DecQuadBindings.Minus(value);
    }

    public static Decimal128 operator --(Decimal128 value)
    {
        return DecQuadBindings.Subtract(value, Decimal128.One);
    }

    public static Decimal128 operator /(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.Divide(left, right);
    }

    public static Decimal128 operator %(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.Remainder(left, right);
    }

    public static Decimal128 operator *(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.Multiply(left, right);
    }

    public static bool operator ==(Decimal128 left, Decimal128 right)
    {
        var compare = DecQuadBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is 0;
    }

    public static bool operator !=(Decimal128 left, Decimal128 right)
    {
        var compare = DecQuadBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is not 0;
    }

    public static bool operator <(Decimal128 left, Decimal128 right)
    {
        var compare = DecQuadBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is -1;
    }

    public static bool operator <=(Decimal128 left, Decimal128 right)
    {
        var compare = DecQuadBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is -1 or 0;
    }

    public static bool operator >(Decimal128 left, Decimal128 right)
    {
        var compare = DecQuadBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is 1;
    }

    public static bool operator >=(Decimal128 left, Decimal128 right)
    {
        var compare = DecQuadBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is 1 or 0;
    }
}