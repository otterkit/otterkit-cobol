namespace Otterkit.Numerics;

public readonly partial struct Decimal128
{
    public static bool IsCanonical(Decimal128 value)
    {
        return DecQuadBindings.IsCanonical(value) is 1U;
    }

    public static bool IsFinite(Decimal128 value)
    {
        return DecQuadBindings.IsFinite(value) is 1U;
    }

    public static bool IsInteger(Decimal128 value)
    
    {
        return DecQuadBindings.IsInteger(value) is 1U;
    }

    public static bool IsNaN(Decimal128 value)
    {
        return DecQuadBindings.IsNaN(value) is 1U;
    }

    public static bool IsNegative(Decimal128 value)
    {
        return DecQuadBindings.IsNegative(value) is 1U;
    }

    public static bool IsNormal(Decimal128 value)
    {
        return DecQuadBindings.IsNormal(value) is 1U;
    }

    public static bool IsPositive(Decimal128 value)
    {
        return DecQuadBindings.IsPositive(value) is 1U;
    }

    public static bool IsSignaling(Decimal128 value)
    {
        return DecQuadBindings.IsSignaling(value) is 1U;
    }

    public static bool IsSigned(Decimal128 value)
    {
        return DecQuadBindings.IsSigned(value) is 1U;
    }

    public static bool IsZero(Decimal128 value)
    {
        return DecQuadBindings.IsZero(value) is 1U;
    }

    public static bool SameQuantum(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.SameQuantum(left, right) is 1U;
    }

    public static uint Radix(Decimal128 value)
    {
        return DecQuadBindings.Radix(value);
    }
}