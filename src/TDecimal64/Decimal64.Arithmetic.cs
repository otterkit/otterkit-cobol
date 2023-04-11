namespace Otterkit.Numerics;

public readonly partial struct Decimal64
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

        return DecDoubleBindings.ToIntegralValue(value, mode);
    }

    public static Decimal64 Abs(Decimal64 value)
    {
        return DecDoubleBindings.Abs(value);
    }

    public static Decimal64 Add(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.Add(left, right);
    }

    public static Decimal64 Subtract(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.Subtract(left, right);
    }

    public static Decimal64 Divide(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.Divide(left, right);
    }

    public static Decimal64 Remainder(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.Remainder(left, right);
    }

    public static Decimal64 RemainderNear(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.RemainderNear(left, right);
    }

    public static Decimal64 Multiply(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.Multiply(left, right);
    }

    public static Decimal64 FusedMultiplyAdd(Decimal64 leftMultiply, Decimal64 rightMultiply, Decimal64 valueAdd)
    {
        return DecDoubleBindings.FusedMultiplyAdd(leftMultiply, rightMultiply, valueAdd);
    }

    public static Decimal64 Plus(Decimal64 value)
    {
        return DecDoubleBindings.Plus(value);
    }

    public static Decimal64 Minus(Decimal64 value)
    {
        return DecDoubleBindings.Minus(value);
    }

    public static Decimal64 Max(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.Max(left, right);
    }

    public static Decimal64 MaxMagnitude(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.MaxMag(left, right);
    }

    public static Decimal64 MaxMagnitudeNumber(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.MaxMag(left, right);
    }

    public static Decimal64 Min(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.Min(left, right);
    }

    public static Decimal64 MinMagnitude(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.MinMag(left, right);
    }

    public static Decimal64 MinMagnitudeNumber(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.MinMag(left, right);
    }
}
