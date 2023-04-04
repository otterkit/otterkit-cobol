namespace Otterkit.Numerics;

public readonly partial struct Decimal128
{
    public static Decimal128 Abs(in Decimal128 value)
    {
        return DecQuadBindings.Abs(value);
    }

    public static Decimal128 Add(in Decimal128 left, in Decimal128 right)
    {
        return DecQuadBindings.Add(left, right);
    }

    public static Decimal128 Subtract(in Decimal128 left, in Decimal128 right)
    {
        return DecQuadBindings.Subtract(left, right);
    }

    public static Decimal128 Divide(in Decimal128 left, in Decimal128 right)
    {
        return DecQuadBindings.Divide(left, right);
    }

    public static Decimal128 Remainder(in Decimal128 left, in Decimal128 right)
    {
        return DecQuadBindings.Remainder(left, right);
    }

    public static Decimal128 RemainderNear(in Decimal128 left, in Decimal128 right)
    {
        return DecQuadBindings.RemainderNear(left, right);
    }

    public static Decimal128 Multiply(in Decimal128 left, in Decimal128 right)
    {
        return DecQuadBindings.Multiply(left, right);
    }

    public static Decimal128 FusedMultiplyAdd(in Decimal128 leftMultiply, in Decimal128 rightMultiply, in Decimal128 valueAdd)
    {
        return DecQuadBindings.FusedMultiplyAdd(leftMultiply, rightMultiply, valueAdd);
    }

    public static Decimal128 Plus(in Decimal128 value)
    {
        return DecQuadBindings.Plus(value);
    }

    public static Decimal128 Minus(in Decimal128 value)
    {
        return DecQuadBindings.Minus(value);
    }
}