namespace Otterkit.Numerics;


public partial struct Decimal64
{
    // NOTE: Decimal constants should be initialized using the IEEE 754 bits constructor.
    // This avoids a call to the decimal encoder, which would make these considerably slower to use.
    public static int Radix => 10;

    public static Decimal64 MaxValue => new Decimal64(8646066034181405951UL);
    public static Decimal64 MinValue => new Decimal64(17869438071036181759UL);
    public static Decimal64 Epsilon => new Decimal64(0UL);

    public static Decimal64 NaN => new Decimal64(8935141660703064064UL);
    public static Decimal64 PositiveInfinity => new Decimal64(8646911284551352320UL);
    public static Decimal64 NegativeInfinity => new Decimal64(17870283321406128128UL);

    public static Decimal64 NegativeOne => new Decimal64(11689092832840122369UL);
    public static Decimal64 NegativeZero => new Decimal64(11689092832840122368UL);
    public static Decimal64 Zero => new Decimal64(2465720795985346560UL);
    public static Decimal64 One => new Decimal64(2465720795985346561UL);
    
    public static Decimal64 E => new Decimal64(3026304887390037061UL);
    public static Decimal64 Pi => new Decimal64(3313736381946871739UL);
    public static Decimal64 Tau => new Decimal64(4178543526649521898UL);

    public static Decimal64 AdditiveIdentity => Decimal64.Zero;
    public static Decimal64 MultiplicativeIdentity => Decimal64.One;
}
