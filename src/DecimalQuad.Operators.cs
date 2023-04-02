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

    public static DecimalQuad operator -(DecimalQuad left, DecimalQuad right)
    {
        return DecQuadBindings.Subtract(left, right);
    }

    public static DecimalQuad operator -(DecimalQuad value)
    {
        return DecQuadBindings.Minus(value);
    }

    public static DecimalQuad operator /(DecimalQuad left, DecimalQuad right)
    {
        return DecQuadBindings.Divide(left, right);
    }

    public static DecimalQuad operator *(DecimalQuad left, DecimalQuad right)
    {
        return DecQuadBindings.Multiply(left, right);
    }
}