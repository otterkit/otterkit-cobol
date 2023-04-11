namespace Otterkit.Numerics;

public readonly partial struct Decimal128
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

        return DecQuadBindings.ToIntegralValue(value, mode);
    }

    public static Decimal128 Abs(Decimal128 value)
    {
        return DecQuadBindings.Abs(value);
    }

    public static Decimal128 Add(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.Add(left, right);
    }

    public static Decimal128 Subtract(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.Subtract(left, right);
    }

    public static Decimal128 Divide(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.Divide(left, right);
    }

    public static Decimal128 Remainder(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.Remainder(left, right);
    }

    public static Decimal128 RemainderNear(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.RemainderNear(left, right);
    }

    public static Decimal128 Multiply(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.Multiply(left, right);
    }

    public static Decimal128 FusedMultiplyAdd(Decimal128 leftMultiply, Decimal128 rightMultiply, Decimal128 valueAdd)
    {
        return DecQuadBindings.FusedMultiplyAdd(leftMultiply, rightMultiply, valueAdd);
    }

    public static Decimal128 Plus(Decimal128 value)
    {
        return DecQuadBindings.Plus(value);
    }

    public static Decimal128 Minus(Decimal128 value)
    {
        return DecQuadBindings.Minus(value);
    }

    public static Decimal128 Max(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.Max(left, right);
    }

    public static Decimal128 MaxMagnitude(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.MaxMag(left, right);
    }

    public static Decimal128 MaxMagnitudeNumber(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.MaxMag(left, right);
    }

    public static Decimal128 Min(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.Min(left, right);
    }

    public static Decimal128 MinMagnitude(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.MinMag(left, right);
    }

    public static Decimal128 MinMagnitudeNumber(Decimal128 left, Decimal128 right)
    {
        return DecQuadBindings.MinMag(left, right);
    }
}
