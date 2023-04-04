namespace Otterkit.Numerics;

public readonly partial struct Decimal128
{
    public static Decimal128 Factorial(int value)
    {
        // Does this switch expression look horrible? YES!
        // But is it a lot faster than the loop version,
        // and does it return the factorial in constant time?
        // ABSOLUTELY!

        // "But why not use an array to hold the values?"
        // Using a switch statement avoids having to allocate an array

        // "Why does the switch statement end at 31 factorial?"
        // 31! has 34 digits which is the maximum number of digits
        // that the IEEE754 DecimalHolder can hold, 32! has 36 digits

        return value switch
        {
            0 => Decimal128.One,
            1 => Decimal128.One,
            2 => new(2UL, 2452209997103235072UL),
            3 => new(6UL, 2452209997103235072UL),
            4 => new(36UL, 2452209997103235072UL),
            5 => new(160UL, 2452209997103235072UL),
            6 => new(928UL, 2452209997103235072UL),
            7 => new(5184UL, 2452209997103235072UL),
            8 => new(65952UL, 2452209997103235072UL),
            9 => new(493582UL, 2452209997103235072UL),
            10 => new(3973132UL, 2452209997103235072UL),
            11 => new(60715020UL, 2452209997103235072UL),
            12 => new(663750400UL, 2452209997103235072UL),
            13 => new(6751813644UL, 2452209997103235072UL),
            14 => new(115150712064UL, 2452209997103235072UL),
            15 => new(1520272121856UL, 2452209997103235072UL),
            16 => new(35644956063744UL, 2452209997103235072UL),
            17 => new(516611056461824UL, 2452209997103235072UL),
            18 => new(7321085160628224UL, 2452209997103235072UL),
            19 => new(182190314218647552UL, 2452209997103235072UL),
            20 => new(2939034172312584192UL, 2452209997103235072UL),
            21 => new(1182700937254862848UL, 2452209997103235077UL),
            22 => new(4612715152460752896UL, 2452209997103235146UL),
            23 => new(13860875154160943104UL, 2452209997103237461UL),
            24 => new(9802000263254867968UL, 2452209997103286308UL),
            25 => new(75910986768318464UL, 2452209997104653393UL),
            26 => new(1340671693921714176UL, 2452209997137004262UL),
            27 => new(603994964052934656UL, 2452209998184238053UL),
            28 => new(2183239355567439872UL, 2452210023148712241UL),
            29 => new(11387031360374833152UL, 2452210552091535355UL),
            30 => new(12782119962489651200UL, 2452234552676927241UL),
            31 => new(14733426917820596224UL, 7640376717878771162UL),
            _ => Decimal128.Zero
        };
    }
}
