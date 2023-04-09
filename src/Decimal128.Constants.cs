namespace Otterkit.Numerics;


public readonly partial struct Decimal128
{
    // NOTE: Decimal constants should be initialized using the IEEE 754 bits constructor.
    // This avoids a call to the decimal encoder, which would make these considerably slower to use.
    public static int Radix => 10;

    public static Decimal128 MaxValue => new Decimal128(17581207694884470015UL, 8646858456403230671UL);
    public static Decimal128 MinValue => new Decimal128(17581207694884470015UL, 17870230493258006479UL);
    public static Decimal128 Epsilon => new Decimal128(0UL, 0UL);

    public static Decimal128 NaN => new Decimal128(0UL, 8935141660703064064UL);
    public static Decimal128 PositiveInfinity => new Decimal128(0UL, 8646911284551352320UL);
    public static Decimal128 NegativeInfinity => new Decimal128(0UL, 17870283321406128128UL);

    public static Decimal128 NegativeOne => new Decimal128(1UL, 11675582033958010880UL);
    public static Decimal128 NegativeZero => new Decimal128(0UL, 11675582033958010880UL);
    public static Decimal128 Zero => new Decimal128(0UL, 2452209997103235072UL);
    public static Decimal128 One => new Decimal128(1UL, 2452209997103235072UL);
    
    public static Decimal128 E => new Decimal128(6113038750254058338UL, 3026411820705289796UL);
    public static Decimal128 Pi => new Decimal128(13034088920725710467UL, 3314592266757321723UL);
    public static Decimal128 Tau => new Decimal128(12239285542586115078UL, 4179290646227926638UL);

    public static Decimal128 AdditiveIdentity => Decimal128.Zero;
    public static Decimal128 MultiplicativeIdentity => Decimal128.One;
}
