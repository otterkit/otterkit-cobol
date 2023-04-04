using System.Numerics;

namespace Otterkit.Numerics;

public readonly partial struct DecimalQuad
{
    public static DecimalQuad operator +(DecimalQuad value)
    {
        return DecQuadBindings.Plus(value);
    }

    public static DecimalQuad operator +(DecimalQuad left, DecimalQuad right)
    {
        return DecQuadBindings.Add(left, right);
    }

    public static DecimalQuad operator ++(DecimalQuad value)
    {
        return DecQuadBindings.Add(value, DecimalQuad.One);
    }

    public static DecimalQuad operator -(DecimalQuad left, DecimalQuad right)
    {
        return DecQuadBindings.Subtract(left, right);
    }

    public static DecimalQuad operator -(DecimalQuad value)
    {
        return DecQuadBindings.Minus(value);
    }

    public static DecimalQuad operator --(DecimalQuad value)
    {
        return DecQuadBindings.Subtract(value, DecimalQuad.One);
    }

    public static DecimalQuad operator /(DecimalQuad left, DecimalQuad right)
    {
        return DecQuadBindings.Divide(left, right);
    }

    public static DecimalQuad operator %(DecimalQuad left, DecimalQuad right)
    {
        return DecQuadBindings.Remainder(left, right);
    }

    public static DecimalQuad operator *(DecimalQuad left, DecimalQuad right)
    {
        return DecQuadBindings.Multiply(left, right);
    }

    public static bool operator ==(DecimalQuad left, DecimalQuad right)
    {
        var compare = DecQuadBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is 0;
    }

    public static bool operator !=(DecimalQuad left, DecimalQuad right)
    {
        var compare = DecQuadBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is not 0;
    }

    public static bool operator <(DecimalQuad left, DecimalQuad right)
    {
        var compare = DecQuadBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is -1;
    }

    public static bool operator <=(DecimalQuad left, DecimalQuad right)
    {
        var compare = DecQuadBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is -1 or 0;
    }

    public static bool operator >(DecimalQuad left, DecimalQuad right)
    {
        var compare = DecQuadBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is 1;
    }

    public static bool operator >=(DecimalQuad left, DecimalQuad right)
    {
        var compare = DecQuadBindings.Compare(left, right);

        if (compare == -5) throw new ArithmeticException("Operand was NaN");

        return compare is 1 or 0;
    }
}