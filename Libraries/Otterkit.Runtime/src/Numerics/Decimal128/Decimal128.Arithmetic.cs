namespace Otterkit.Numerics;

public partial struct Decimal128
{
    public static Decimal128 Round(Decimal128 value, RoundingMode mode)
    {
        if (mode == RoundingMode.RoundForReround)
        {
            throw new NotSupportedException("Round for reround is not supported");
        }

        if ((int)mode is not < 8)
        {
            throw new ArgumentOutOfRangeException(nameof(mode), mode, "Rounding mode must be within enum range");
        }

        return Decimal128Bindings.ToIntegralValue(value, mode);
    }

    public static Decimal128 Abs(Decimal128 value)
    {
        return Decimal128Bindings.Abs(value);
    }

    public static Decimal128 Add(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.Add(left, right);
    }

    public static Decimal128 Subtract(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.Subtract(left, right);
    }

    public static Decimal128 Divide(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.Divide(left, right);
    }

    public static Decimal128 Remainder(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.Remainder(left, right);
    }

    public static Decimal128 RemainderNear(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.RemainderNear(left, right);
    }

    public static Decimal128 Multiply(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.Multiply(left, right);
    }

    public static Decimal128 FusedMultiplyAdd(Decimal128 leftMultiply, Decimal128 rightMultiply, Decimal128 valueAdd)
    {
        return Decimal128Bindings.FusedMultiplyAdd(leftMultiply, rightMultiply, valueAdd);
    }

    public static Decimal128 Plus(Decimal128 value)
    {
        return Decimal128Bindings.Plus(value);
    }

    public static Decimal128 Minus(Decimal128 value)
    {
        return Decimal128Bindings.Minus(value);
    }

    public static Decimal128 Max(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.Max(left, right);
    }

    public static Decimal128 MaxMagnitude(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.MaxMag(left, right);
    }

    public static Decimal128 MaxMagnitudeNumber(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.MaxMag(left, right);
    }

    public static Decimal128 Min(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.Min(left, right);
    }

    public static Decimal128 MinMagnitude(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.MinMag(left, right);
    }

    public static Decimal128 MinMagnitudeNumber(Decimal128 left, Decimal128 right)
    {
        return Decimal128Bindings.MinMag(left, right);
    }
}
