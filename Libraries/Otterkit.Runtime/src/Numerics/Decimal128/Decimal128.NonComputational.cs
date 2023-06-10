namespace Otterkit.Numerics;

public partial struct Decimal128
{
    public static bool IsCanonical(Decimal128 value)
    {
        return Decimal128Bindings.IsCanonical(value) is 1U;
    }

    public static bool IsFinite(Decimal128 value)
    {
        return Decimal128Bindings.IsFinite(value) is 1U;
    }

    public static bool IsInfinity(Decimal128 value)
    {
        return Decimal128Bindings.IsInfinite(value) is 1U;
    }

    public static bool IsNegativeInfinity(Decimal128 value)
    {
        return Decimal128Bindings.IsInfinite(value) is 1U && Decimal128Bindings.IsNegative(value) is 1U;
    }

    public static bool IsPositiveInfinity(Decimal128 value)
    {
        return Decimal128Bindings.IsInfinite(value) is 1U && Decimal128Bindings.IsPositive(value) is 1U;
    }

    public static bool IsInteger(Decimal128 value)
    {
        return Decimal128Bindings.IsInteger(value) is 1U;
    }

    public static bool IsEvenInteger(Decimal128 value)
    {
        var isInteger = Decimal128Bindings.IsInteger(value) is 1U;

        return isInteger && value % 2 == Decimal128.Zero;
    }

    public static bool IsOddInteger(Decimal128 value)
    {
        var isInteger = Decimal128Bindings.IsInteger(value) is 1U;

        return isInteger && value % 2 != Decimal128.Zero;
    }

    public static bool IsNaN(Decimal128 value)
    {
        return Decimal128Bindings.IsNaN(value) is 1U;
    }

    public static bool IsNormal(Decimal128 value)
    {
        return Decimal128Bindings.IsNormal(value) is 1U;
    }

    public static bool IsSubnormal(Decimal128 value)
    {
        return Decimal128Bindings.IsSubnormal(value) is 1U;
    }

    public static bool IsPositive(Decimal128 value)
    {
        return Decimal128Bindings.IsPositive(value) is 1U;
    }

    public static bool IsNegative(Decimal128 value)
    {
        return Decimal128Bindings.IsNegative(value) is 1U;
    }

    public static bool IsSigned(Decimal128 value)
    {
        return Decimal128Bindings.IsSigned(value) is 1U;
    }

    public static bool IsSignaling(Decimal128 value)
    {
        return Decimal128Bindings.IsSignaling(value) is 1U;
    }

    public static bool IsZero(Decimal128 value)
    {
        return Decimal128Bindings.IsZero(value) is 1U;
    }

    public static bool SameQuantum(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.SameQuantum(left, right) is 1U;
    }

    public static bool IsComplexNumber(Decimal128 value)
    {
        // It should never be a complex number, right?
        return false;
    }

    public static bool IsImaginaryNumber(Decimal128 value)
    {
        // It should never be an imaginary number, right?
        return false;
    }

    public static bool IsRealNumber(Decimal128 value)
    {
        // It should always be a real number, right?
        return true;
    }
}