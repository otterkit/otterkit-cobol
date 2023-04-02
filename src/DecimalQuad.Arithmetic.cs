namespace Otterkit.Numerics;

public readonly partial struct DecimalQuad
{
    public static DecimalQuad Abs(in DecimalQuad value)
    {
        return DecQuadBindings.Abs(value);
    }

    public static DecimalQuad Add(in DecimalQuad left, in DecimalQuad right)
    {
        return DecQuadBindings.Add(left, right);
    }

    public static DecimalQuad Subtract(in DecimalQuad left, in DecimalQuad right)
    {
        return DecQuadBindings.Subtract(left, right);
    }

    public static DecimalQuad Divide(in DecimalQuad left, in DecimalQuad right)
    {
        return DecQuadBindings.Divide(left, right);
    }

    public static DecimalQuad Remainder(in DecimalQuad left, in DecimalQuad right)
    {
        return DecQuadBindings.Remainder(left, right);
    }

    public static DecimalQuad RemainderNear(in DecimalQuad left, in DecimalQuad right)
    {
        return DecQuadBindings.RemainderNear(left, right);
    }

    public static DecimalQuad Multiply(in DecimalQuad left, in DecimalQuad right)
    {
        return DecQuadBindings.Multiply(left, right);
    }

    public static DecimalQuad FusedMultiplyAdd(in DecimalQuad leftMultiply, in DecimalQuad rightMultiply, in DecimalQuad valueAdd)
    {
        return DecQuadBindings.FusedMultiplyAdd(leftMultiply, rightMultiply, valueAdd);
    }

    public static DecimalQuad Plus(in DecimalQuad value)
    {
        return DecQuadBindings.Plus(value);
    }

    public static DecimalQuad Minus(in DecimalQuad value)
    {
        return DecQuadBindings.Minus(value);
    }
}