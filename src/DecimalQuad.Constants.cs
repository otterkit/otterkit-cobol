namespace Otterkit.Numerics;


public readonly partial struct DecimalQuad
{
    // NOTE: Decimal constants should be initialized using the IEEE 754 bits constructor.
    // This avoids a call to the decimal encoder, which would make these considerably slower to use.

    public static DecimalQuad NegativeOne => new DecimalQuad(1UL, 11675582033958010880UL);
    public static DecimalQuad Zero => new DecimalQuad(0UL, 2452209997103235072UL);
    public static DecimalQuad One => new DecimalQuad(1UL, 2452209997103235072UL);
    
    public static DecimalQuad E => new DecimalQuad(6113038750254058338UL, 3026411820705289796UL);
    public static DecimalQuad Pi => new DecimalQuad(13034088920725710467UL, 3314592266757321723UL);
    public static DecimalQuad Tau => new DecimalQuad(12239285542586115078UL, 4179290646227926638UL);
}
