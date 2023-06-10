namespace Otterkit.Numerics;

public partial struct Decimal64
{
    public static bool IsCanonical(Decimal64 value)
    {
        return Decimal64Bindings.IsCanonical(value) is 1U;
    }

    public static bool IsFinite(Decimal64 value)
    {
        return Decimal64Bindings.IsFinite(value) is 1U;
    }

    public static bool IsInfinity(Decimal64 value)
    {
        return Decimal64Bindings.IsInfinite(value) is 1U;
    }

    public static bool IsNegativeInfinity(Decimal64 value)
    {
        return Decimal64Bindings.IsInfinite(value) is 1U && Decimal64Bindings.IsNegative(value) is 1U;
    }

    public static bool IsPositiveInfinity(Decimal64 value)
    {
        return Decimal64Bindings.IsInfinite(value) is 1U && Decimal64Bindings.IsPositive(value) is 1U;
    }

    public static bool IsInteger(Decimal64 value)
    {
        return Decimal64Bindings.IsInteger(value) is 1U;
    }

    public static bool IsEvenInteger(Decimal64 value)
    {
        var isInteger = Decimal64Bindings.IsInteger(value) is 1U;

        return isInteger && value % 2 == Decimal64.Zero;
    }

    public static bool IsOddInteger(Decimal64 value)
    {
        var isInteger = Decimal64Bindings.IsInteger(value) is 1U;

        return isInteger && value % 2 != Decimal64.Zero;
    }

    public static bool IsNaN(Decimal64 value)
    {
        return Decimal64Bindings.IsNaN(value) is 1U;
    }

    public static bool IsNormal(Decimal64 value)
    {
        return Decimal64Bindings.IsNormal(value) is 1U;
    }

    public static bool IsSubnormal(Decimal64 value)
    {
        return Decimal64Bindings.IsSubnormal(value) is 1U;
    }

    public static bool IsPositive(Decimal64 value)
    {
        return Decimal64Bindings.IsPositive(value) is 1U;
    }

    public static bool IsNegative(Decimal64 value)
    {
        return Decimal64Bindings.IsNegative(value) is 1U;
    }

    public static bool IsSigned(Decimal64 value)
    {
        return Decimal64Bindings.IsSigned(value) is 1U;
    }

    public static bool IsSignaling(Decimal64 value)
    {
        return Decimal64Bindings.IsSignaling(value) is 1U;
    }

    public static bool IsZero(Decimal64 value)
    {
        return Decimal64Bindings.IsZero(value) is 1U;
    }

    public static bool SameQuantum(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.SameQuantum(left, right) is 1U;
    }

    public static bool IsComplexNumber(Decimal64 value)
    {
        // It should never be a complex number, right?
        return false;
    }

    public static bool IsImaginaryNumber(Decimal64 value)
    {
        // It should never be an imaginary number, right?
        return false;
    }

    public static bool IsRealNumber(Decimal64 value)
    {
        // It should always be a real number, right?
        return true;
    }
}