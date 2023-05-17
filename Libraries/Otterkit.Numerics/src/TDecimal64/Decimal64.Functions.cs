namespace Otterkit.Numerics;

public readonly partial struct Decimal64
{
    public static Decimal64 Pow(Decimal64 left, Decimal64 right)
    {
        return DecDoubleBindings.Pow(left, right);
    }

    public static Decimal64 Sqrt(Decimal64 value)
    {
        return DecDoubleBindings.Sqrt(value);
    }

    public static Decimal64 Ln(Decimal64 value)
    {
        return DecDoubleBindings.Ln(value);
    }

    public static Decimal64 Exp(Decimal64 value)
    {
        return DecDoubleBindings.Exp(value);
    }

    public static Decimal64 LogB(Decimal64 value)
    {
        return DecDoubleBindings.LogB(value);
    }

    public static Decimal64 Log10(Decimal64 value)
    {
        return DecDoubleBindings.Log10(value);
    }

    public static Decimal64 Tan(Decimal64 radians)
    {
        return Sin(radians) / Cos(radians);
    }

    public static Decimal64 Sin(Decimal64 radians)
    {
        var sum = radians;
        var term = radians;

        var iteration = 1;

        while(true) 
        {
            var previous = sum;

            term = -term * radians * radians / (2 * iteration) / (2 * iteration + 1);
            sum += term;

            iteration++;

            if (previous == sum) break;
        }
        
        return sum;
    }

    public static Decimal64 Cos(Decimal64 radians)
    {
        return Sin(Decimal64.Pi / 2 - radians);
    }

    public static Decimal64 Atan(Decimal64 ratio)
    {
        Decimal64 halfPi = Decimal64.Pi / 2;

        if (ratio < -1)
            return -halfPi - Atan(1 / ratio);

        if (ratio > 1)
            return halfPi - Atan(1 / ratio);

        Decimal64 coefficient = 2;

        Decimal64 iteration = ratio / (ratio * ratio + 1);

        Decimal64 result = iteration;

        for (int i = 0; i < 64; i++)
        {
            iteration *= (ratio * ratio / (ratio * ratio + 1) * coefficient / (coefficient + 1));

            result += iteration;

            coefficient += 2;
        }

        return result;
    }

    public static Decimal64 Asin(Decimal64 ratio)
    {
        if (ratio < -1 || ratio > 1)
            throw new ArgumentException($"ASIN argument must be >= -1 and <= +1");

        Decimal64 sqrtArg = (1 - ratio * ratio);

        return Atan(ratio / (Decimal64.Sqrt(sqrtArg) + 1)) * 2;
    }

    public static Decimal64 Acos(Decimal64 ratio)
    {
        if (ratio < -1 || ratio > 1)
            throw new ArgumentException($"ACOS argument must be >= -1 and <= +1");

        Decimal64 sqrtArg = (1 - ratio * ratio);

        return Atan(Decimal64.Sqrt(sqrtArg) / (ratio + 1)) * 2;
    }

    public static Decimal64 Factorial(int value)
    {
        // Does this switch expression look horrible? YES!
        // But is it a lot faster than the loop/recursion version,
        // and does it return the factorial in constant time?
        // ABSOLUTELY!

        // "But why not use an array to hold the values?"
        // Using a switch expression avoids having to allocate an array

        // "Why does the switch expression end at 31 factorial?"
        // 31! has 34 digits which is the maximum number of digits
        // that the IEEE754 Decimal64 can hold, 32! has 36 digits

        return value switch
        {
            0 => Decimal64.One,
            1 => Decimal64.One,
            2 => new(2465720795985346562UL),
            3 => new(2465720795985346566UL),
            4 => new(2465720795985346596UL),
            5 => new(2465720795985346720UL),
            6 => new(2465720795985347488UL),
            7 => new(2465720795985351744UL),
            8 => new(2465720795985412512UL),
            9 => new(2465720795985840142UL),
            10 => new(2465720795989319692UL),
            11 => new(2465720796046061580UL),
            12 => new(2465720796649096960UL),
            13 => new(2465720802737160204UL),
            14 => new(2465722316257468416UL),
            15 => new(2465722316257468416UL),
            16 => new(2465756440941410304UL),
            17 => new(2466237407041808384UL),
            18 => new(4195668738615189504UL),
            19 => new(2756509272916441504UL),
            20 => new(3046177598754511680UL),
            21 => new(3911527006393844428UL),
            22 => new(2760886892466528008UL),
            23 => new(3050849275328182878UL),
            24 => new(4204396754410850778UL),
            25 => new(2766003091966184543UL),
            26 => new(3631082893231888470UL),
            27 => new(2768674473250981437UL),
            28 => new(3346253979567938745UL),
            29 => new(7670841794375903106UL),
            30 => new(3062257162502964381UL),
            31 => new(7674452965754920365UL),
            _ => Decimal64.Zero
        };
    }
}
