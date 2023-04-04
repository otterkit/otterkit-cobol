namespace Otterkit.Numerics;


public readonly partial struct Decimal128
{
    // NOTE: Decimal constants should be initialized using the IEEE 754 bits constructor.
    // This avoids a call to the decimal encoder, which would make these considerably slower to use.

    public static Decimal128 NegativeOne => new Decimal128(1UL, 11675582033958010880UL);
    public static Decimal128 Zero => new Decimal128(0UL, 2452209997103235072UL);
    public static Decimal128 One => new Decimal128(1UL, 2452209997103235072UL);
    
    public static Decimal128 E => new Decimal128(6113038750254058338UL, 3026411820705289796UL);
    public static Decimal128 Pi => new Decimal128(13034088920725710467UL, 3314592266757321723UL);
    public static Decimal128 Tau => new Decimal128(12239285542586115078UL, 4179290646227926638UL);
}
