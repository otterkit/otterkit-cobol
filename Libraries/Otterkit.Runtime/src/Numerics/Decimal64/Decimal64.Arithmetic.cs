namespace Otterkit.Numerics;

public partial struct Decimal64
{
    public static Decimal64 Round(Decimal64 value, RoundingMode mode)
    {
        if (mode == RoundingMode.RoundForReround)
        {
            throw new NotSupportedException("Round for reround is not supported");
        }

        if ((int)mode is not < 8)
        {
            throw new ArgumentOutOfRangeException(nameof(mode), mode, "Rounding mode must be within enum range");
        }

        return Decimal64Bindings.ToIntegralValue(value, mode);
    }

    public static Decimal64 Abs(Decimal64 value)
    {
        return Decimal64Bindings.Abs(value);
    }

    public static Decimal64 Add(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.Add(left, right);
    }

    public static Decimal64 Subtract(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.Subtract(left, right);
    }

    public static Decimal64 Divide(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.Divide(left, right);
    }

    public static Decimal64 Remainder(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.Remainder(left, right);
    }

    public static Decimal64 RemainderNear(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.RemainderNear(left, right);
    }

    public static Decimal64 Multiply(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.Multiply(left, right);
    }

    public static Decimal64 FusedMultiplyAdd(Decimal64 leftMultiply, Decimal64 rightMultiply, Decimal64 valueAdd)
    {
        return Decimal64Bindings.FusedMultiplyAdd(leftMultiply, rightMultiply, valueAdd);
    }

    public static Decimal64 Plus(Decimal64 value)
    {
        return Decimal64Bindings.Plus(value);
    }

    public static Decimal64 Minus(Decimal64 value)
    {
        return Decimal64Bindings.Minus(value);
    }

    public static Decimal64 Max(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.Max(left, right);
    }

    public static Decimal64 MaxMagnitude(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.MaxMag(left, right);
    }

    public static Decimal64 MaxMagnitudeNumber(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.MaxMag(left, right);
    }

    public static Decimal64 Min(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.Min(left, right);
    }

    public static Decimal64 MinMagnitude(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.MinMag(left, right);
    }

    public static Decimal64 MinMagnitudeNumber(Decimal64 left, Decimal64 right)
    {
        return Decimal64Bindings.MinMag(left, right);
    }
}
