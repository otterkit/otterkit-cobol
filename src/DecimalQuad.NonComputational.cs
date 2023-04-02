namespace Otterkit.Numerics;

public readonly partial struct DecimalQuad
{
    public static bool IsCanonical(DecimalQuad value)
    {
        return DecQuadBindings.IsCanonical(value) is 1U;
    }

    public static bool IsFinite(DecimalQuad value)
    {
        return DecQuadBindings.IsFinite(value) is 1U;
    }

    public static bool IsInteger(DecimalQuad value)
    
    {
        return DecQuadBindings.IsInteger(value) is 1U;
    }

    public static bool IsNaN(DecimalQuad value)
    {
        return DecQuadBindings.IsNaN(value) is 1U;
    }

    public static bool IsNegative(DecimalQuad value)
    {
        return DecQuadBindings.IsNegative(value) is 1U;
    }

    public static bool IsNormal(DecimalQuad value)
    {
        return DecQuadBindings.IsNormal(value) is 1U;
    }

    public static bool IsPositive(DecimalQuad value)
    {
        return DecQuadBindings.IsPositive(value) is 1U;
    }

    public static bool IsSignaling(DecimalQuad value)
    {
        return DecQuadBindings.IsSignaling(value) is 1U;
    }

    public static bool IsSigned(DecimalQuad value)
    {
        return DecQuadBindings.IsSigned(value) is 1U;
    }

    public static bool IsZero(DecimalQuad value)
    {
        return DecQuadBindings.IsZero(value) is 1U;
    }

    public static bool SameQuantum(DecimalQuad left, DecimalQuad right)
    {
        return DecQuadBindings.SameQuantum(left, right) is 1U;
    }

    public static uint Radix(DecimalQuad value)
    {
        return DecQuadBindings.Radix(value);
    }
}