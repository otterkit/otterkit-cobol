using System.Text.RegularExpressions;
using System.Text;
namespace OtterkitLibrary;

public static class Functions
{
    public static DecimalHolder ABS(DecimalHolder argument)
    {
        return DecimalMath.Abs(argument.Bytes);
    }

    public static DecimalHolder ACOS(DecimalHolder ratio)
    {
        if (ratio < "-1"u8 || ratio > "1"u8)
            throw new ArgumentException($"The argument of ACOS must be >= -1 and <= to +1");

        DecimalHolder param = ("1"u8 - ratio * ratio);
        return ATAN(DecimalMath.Sqrt(param.Bytes) / (ratio + "1"u8)) * "2"u8;
    }

    public static DecimalHolder ANNUITY(DecimalHolder interest, DecimalHolder periods)
    {
        if (interest == "0"u8)
        {
            return "1"u8 / periods;
        }
        // (argument-1 / (1 – (1 + argument-1)** (– (argument-2))))
        return interest / (new DecimalHolder("1"u8) - DecimalMath.Pow(("1"u8 + interest).Bytes, (-periods).Bytes));
    }

    public static DecimalHolder ASIN(DecimalHolder ratio)
    {
        if (ratio < "-1"u8 || ratio > "1"u8)
            throw new ArgumentException($"The argument of ASIN must be >= -1 and <= to +1");

        DecimalHolder param = ("1"u8 - ratio * ratio);
        return ATAN(ratio / (DecimalMath.Sqrt(param.Bytes) + new DecimalHolder("1"u8))) * "2"u8;
    }

    public static DecimalHolder ATAN(DecimalHolder ratio)
    {
        if (ratio < "-1"u8)
            return -(PI() / "2"u8) - ATAN("1"u8 / ratio);

        if (ratio > "1"u8)
            return PI() / "2"u8 - ATAN("1"u8 / ratio);

        DecimalHolder coefficient = "2"u8;
        DecimalHolder iteration = ratio / (ratio * ratio + "1"u8);
        DecimalHolder result = iteration;

        for (int i = 0; i < 64; i++)
        {
            iteration *= (ratio * ratio / (ratio * ratio + "1"u8) * coefficient / (coefficient + "1"u8));

            result += iteration;
            coefficient += "2"u8;
        }

        return result;
    }

    public static void BASE_CONVERT(DecimalHolder input, DecimalHolder current, DecimalHolder target)
    {
        // TODO BASE-CONVERT
    }

    public static void BOOLEAN_OF_INTEGER(DecimalHolder argument)
    {
        // TODO BOOLEAN-OF-INTEGER
        // Need to implement usage bit first
    }

    public static DecimalHolder BYTE_LENGTH(DecimalHolder argument)
    {
        // Does not cover all BYTE-LENGTH functionality
        DecimalHolder byteLength = "0"u8;
        foreach (var bytes in argument.Bytes)
        {
            byteLength++;
        }
        return byteLength;
    }

    public static Alphanumeric CHAR(Numeric argument)
    {
        ReadOnlySpan<byte> bytes = argument.Bytes;
        int parseInt = int.Parse(Encoding.UTF8.GetString(bytes));
        ReadOnlySpan<byte> charAsByte = new(((byte)parseInt));

        return new Alphanumeric(charAsByte, 0, 1, new byte[1]);
    }

    public static string CHAR_NATIONAL(Numeric argument)
    {
        int parseInt = int.Parse(argument.Display);
        return ((char)parseInt).ToString();
    }

    public static DecimalHolder COMBINED_DATETIME(DecimalHolder date, DecimalHolder time)
    {
        // TODO: implement other date time intrinsic functions
        return date;
    }

    public static string CONCAT(params string[] strings)
    {
        string concat = String.Concat(strings);
        return concat;
    }

    public static void CONVERT(DecimalHolder value, DecimalHolder source, DecimalHolder target)
    {
        // TODO: Need to implement other COBOL data types first
    }

    public static DecimalHolder COS(DecimalHolder radians)
    {
        return SIN(PI() / "2"u8 - radians);
    }

    public static string CURRENT_DATE()
    {
        DateTime currentDate = DateTime.Now;
        TimeSpan offset = TimeZoneInfo.Local.GetUtcOffset(DateTime.UtcNow);
        string formattedDate = currentDate.ToString("yyyyMMddHHmmssff");
        string DatePlusOffset;

        if (offset.ToString().Contains("+"))
        { 
            DatePlusOffset = new String(formattedDate + "+" + offset.ToString("hhmm"));
        }
        else
        {
            DatePlusOffset = new String(formattedDate + "-" + offset.ToString("hhmm"));
        }

        return DatePlusOffset;
    }

    public static void DATE_OF_INTEGER(Numeric date)
    {
        DateTime Y1600 = new(1600, 12, 31);
        int intValue = int.Parse(date.Display);

        DateTime dateOfInteger = Y1600.AddDays(intValue);
    }

    public static void DATE_TO_YYYYMMDD(DecimalHolder date)
    {
        // TODO: implement DATE-TO-YYYYMMDD
    }

    public static void DAY_OF_INTEGER(DecimalHolder date)
    {
        // TODO: implement DAY-OF-INTEGER
    }

    public static void DAY_TO_YYYYDDD(DecimalHolder date)
    {
        // TODO: implement DAY-TO-YYYYDDD
    }

    public static void DISPLAY_OF(DecimalHolder date)
    {
        // TODO: implement National type
    }

    public static DecimalHolder E()
    {
        return new DecimalHolder("2.718281828459045235360287471352662"u8);
    }

    public static void EXCEPTION_FILE(string? filename)
    {
        // Implement COBOL exceptions
    }

    public static void EXCEPTION_FILE_N(string? filename)
    {
        // Implement COBOL exceptions
    }

    public static void EXCEPTION_LOCATION()
    {
        // Implement COBOL exceptions
    }

    public static void EXCEPTION_LOCATION_N()
    {
        // Implement COBOL exceptions
    }

    public static void EXCEPTION_STATEMENT()
    {
        // Implement COBOL exceptions
    }

    public static void EXCEPTION_STATUS()
    {
        // Implement COBOL exceptions
    }

    public static Numeric EXP(Numeric exponent)
    {
        DecimalHolder result = DecimalMath.Exp(exponent.Bytes);
        int PositionOfDecimal = result.Bytes.IndexOf("."u8);
        Numeric temporary = new(result.Bytes, 0, 32, 2, new byte[34]);
        return temporary;
    }

    public static Numeric EXP10(Numeric exponent)
    {
        DecimalHolder result = DecimalMath.Pow("10"u8, exponent.Bytes);
        int PositionOfDecimal = result.Bytes.IndexOf("."u8);
        Numeric temporary = new(result.Bytes, 0, 32, 2, new byte[34]);
        return temporary;
    }

    public static Int128 FACTORIAL(Int128 argument)
    {
        if (argument == 0 || argument == 1)
        {
            return 1;
        }

        if (argument == 2)
        {
            return 2;
        }

		Int128 factorial = argument;
        for (Int128 i = factorial - 1; i > 0; i--)
        {
            factorial *= i;
        }
        
        argument--;

        return factorial;
    }

    public static DecimalHolder FACTORIAL(DecimalHolder argument)
    {
        // These two variables (FirstInteger and SecondInteger)
        // convert the bytes value from the DecimalHolder argument
        // into an int value that can be used for the switch statement
        // 48 is the position of 0 in UTF-8 encoded bytes

        int FirstInteger = argument.Bytes.Length == 2 ? ((argument.Bytes[0] - 48) * 10) : argument.Bytes[0] - 48;
        int SecondInteger = argument.Bytes.Length == 2 ? argument.Bytes[1] - 48 : 0;

        int IntegerOfDecimal = FirstInteger + SecondInteger;

        DecimalHolder factorial = "0"u8;
        switch (IntegerOfDecimal)
        {
            // Does this switch statement look horrible? YES!
            // But is it a lot faster than the loop version,
            // and does it return the factorial in constant time?
            // ABSOLUTELY!

            // "But why not use an array to hold the values?"
            // Using a switch statement avoids having to allocate an array

            // "Why does the switch statement end at 31 factorial?"
            // 31! has 34 digits which is the maximum number of digits
            // that the IEEE754 DecimalHolder can hold, 32! has 36 digits

            case 0: factorial = new("1"u8); break;
            case 1: factorial = new("1"u8); break;
            case 2: factorial = new("2"u8); break;
            case 3: factorial = new("6"u8); break;
            case 4: factorial = new("24"u8); break;
            case 5: factorial = new("120"u8); break;
            case 6: factorial = new("720"u8); break;
            case 7: factorial = new("5040"u8); break;
            case 8: factorial = new("40320"u8); break;
            case 9: factorial = new("362880"u8); break;
            case 10: factorial = new("3628800"u8); break;
            case 11: factorial = new("39916800"u8); break;
            case 12: factorial = new("479001600"u8); break;
            case 13: factorial = new("6227020800"u8); break;
            case 14: factorial = new("87178291200"u8); break;
            case 15: factorial = new("1307674368000"u8); break;
            case 16: factorial = new("20922789888000"u8); break;
            case 17: factorial = new("355687428096000"u8); break;
            case 18: factorial = new("6402373705728000"u8); break;
            case 19: factorial = new("121645100408832000"u8); break;
            case 20: factorial = new("2432902008176640000"u8); break;
            case 21: factorial = new("51090942171709440000"u8); break;
            case 22: factorial = new("1124000727777607680000"u8); break;
            case 23: factorial = new("25852016738884976640000"u8); break;
            case 24: factorial = new("620448401733239439360000"u8); break;
            case 25: factorial = new("15511210043330985984000000"u8); break;
            case 26: factorial = new("403291461126605635584000000"u8); break;
            case 27: factorial = new("10888869450418352160768000000"u8); break;
            case 28: factorial = new("304888344611713860501504000000"u8); break;
            case 29: factorial = new("8841761993739701954543616000000"u8); break;
            case 30: factorial = new("265252859812191058636308480000000"u8); break;
            case 31: factorial = new("8222838654177922817725562880000000"u8); break;
        }

        return factorial;
    }


    public static int FIND_STRING(string argument, string substring, int ignore, bool last, bool anycase)
    {
        if (!last && anycase && ignore == 0) 
            return argument.ToLower().IndexOf(substring.ToLower()) + 1;
        
        if (!last && ignore == 0) 
            return argument.IndexOf(substring) + 1;

        if (last && anycase) 
            return argument.ToLower().LastIndexOf(substring.ToLower()) + 1;

        if (last) 
            return argument.LastIndexOf(substring) + 1;

        List<int> matches = new();
        foreach (Match str in Regex.Matches(argument, substring, anycase ? RegexOptions.IgnoreCase: RegexOptions.None))
            matches.Add(str.Index + 1);

        if (ignore >= matches.Count) return 0;

        return matches[ignore];
    }

     public static string FORMATTED_CURRENT_DATE(string format)
    {
        DateTime currentDate = DateTime.Now;
        TimeSpan offset = TimeZoneInfo.Local.GetUtcOffset(DateTime.UtcNow);
        string formattedDate = currentDate.ToString(format);
        string DatePlusOffset;

        if (offset.ToString().Contains("+"))
        { 
            DatePlusOffset = new String(formattedDate + "+" + offset.ToString("hh:mm"));
        }
        else
        {
            DatePlusOffset = new String(formattedDate + "-" + offset.ToString("hh:mm"));
        }

        return DatePlusOffset;
    }

    public static string FORMATTED_DATE(string format, int date)
    {
        // TODO: Parse format string to remove time format
        DateTime Y1600 = new(1600, 12, 31);
        DateTime current = Y1600.AddDays(date);
        return current.ToString(format);
    }

    public static void FORMATTED_DATETIME(string format, int date)
    {
        // TODO: Implement FORMATTED-DATETIME
    }

    public static void FORMATTED_TIME(string format, DecimalHolder seconds, int offset)
    {
        // TODO: Implement FORMATTED-TIME
    }

    public static Numeric FRACTION_PART(Numeric argument)
    {
        Span<byte> FractionPart = stackalloc byte[2 + argument.FractionalLength];
        "0."u8.CopyTo(FractionPart);

        int indexOfDecimal = argument.Bytes.IndexOf("."u8);
        if (indexOfDecimal == -1)
            return new Numeric("0.0"u8, 0, 1, 1, new byte[2]);
        
        argument.Bytes.Slice(indexOfDecimal + 1).CopyTo(FractionPart.Slice(2));
        return new Numeric(FractionPart, 0, 1, argument.FractionalLength, new byte[2 + argument.FractionalLength]);
    }

    public static Numeric HIGHEST_ALGEBRAIC(Numeric argument)
    {
        int integer = argument.Length;
        int fraction = argument.FractionalLength;
        Span<byte> HighestAlgebraic = stackalloc byte[integer + fraction + 2];
        HighestAlgebraic[0] = 43;
        HighestAlgebraic.Slice(1, integer).Fill(57);

        if (fraction > 0)
        {
            HighestAlgebraic[integer + 1] = 46;
            HighestAlgebraic.Slice(integer + 2).Fill(57);
        }

        return new Numeric(HighestAlgebraic, 0, integer, fraction, new byte[integer + fraction + 2]);
    }

    public static Numeric INTEGER_OF_BOOLEAN(Boolean argument)
    {
        string str = Convert.ToInt64(argument.Display, 2).ToString();
        ReadOnlySpan<byte> bytes = Encoding.UTF8.GetBytes(str);

        return new Numeric(bytes, 0, bytes.Length, 0, new byte[bytes.Length]);
    }

    public static Numeric INTEGER_OF_DATE(Numeric argument)
    {
        DateTime Y1600 = new(1600, 12, 31);
        int intValue = int.Parse(argument.Display);
        // Datetime constructor => Year, Month, Day
        // Calculations below are to remove the appropriate parts from the argument
        // It Just Works™
        DateTime current = new((intValue / 10000),(intValue / 100) - (intValue / 10000 * 100),intValue - (intValue / 100 * 100));
        string str = (current.Date - Y1600.Date).Days.ToString();
        ReadOnlySpan<byte> bytes = Encoding.UTF8.GetBytes(str);

        return new Numeric(bytes, 0, bytes.Length, 0, new byte[bytes.Length]);
    }

    public static Numeric INTEGER_OF_DAY(Numeric argument)
    {
        DateTime Y1600 = new(1600, 12, 31);
        int intValue = int.Parse(argument.Display);
        // Datetime constructor => Year, Month, Day
        // Same idea as the INTEGER_OF_DATE function, but from Julian date form
        // It Just Works™
        DateTime fromJulianYear = new((intValue - intValue % 1000) / 1000, 1, 1);
        DateTime fromJulianDays = fromJulianYear.AddDays(intValue % 1000 - 1);

        string str = (fromJulianDays.Date - Y1600.Date).Days.ToString();
        ReadOnlySpan<byte> bytes = Encoding.UTF8.GetBytes(str);


        return new Numeric(bytes, 0, bytes.Length, 0, new byte[bytes.Length]);
    }

    public static void INTEGER_OF_FORMATTED_DATE(int argument)
    {
        // TODO: Implement INTEGER-OF-FORMATTED-DATE
    }

    public static Numeric INTEGER_PART(Numeric argument)
    {
        int indexOfDecimal = argument.Bytes.IndexOf("."u8);
        ReadOnlySpan<byte> IntegerPart = indexOfDecimal > 0 
            ? argument.Bytes.Slice(0, indexOfDecimal)
            : argument.Bytes;

        return new Numeric(IntegerPart, 0, argument.Length, 0, new byte[argument.Length]);
    }

    public static int LENGTH(string argument)
    {
        // Does not cover all LENGTH functionality
        return argument.Length;
    }

    public static void LOCALE_COMPARE(Alphanumeric argument)
    {
        // TODO: Implement COBOL locale functionality
    }

    public static void LOCALE_DATE(Alphanumeric argument)
    {
        // TODO: Implement COBOL locale functionality
    }

    public static void LOCALE_TIME(Alphanumeric argument)
    {
        // TODO: Implement COBOL locale functionality
    }

    public static void LOCALE_TIME_FROM_SECONDS(Alphanumeric argument)
    {
        // TODO: Implement COBOL locale functionality
    }

    public static DecimalHolder LOG(DecimalHolder argument)
    {
        return DecimalMath.Ln(argument);
    }

    public static DecimalHolder LOG10(DecimalHolder argument)
    {
        return DecimalMath.Log10(argument);
    }

    public static string LOWER_CASE(string argument, string? locale)
    {
        return argument.ToLower();
    }

    public static Numeric LOWEST_ALGEBRAIC(Numeric argument)
    {
        int integer = argument.Length;
        int fraction = argument.FractionalLength;
        Span<byte> LowestAlgebraic = stackalloc byte[integer + fraction + 2];
        LowestAlgebraic[0] = 45;
        LowestAlgebraic.Slice(1, integer).Fill(57);

        if (fraction > 0)
        {
            LowestAlgebraic[integer + 1] = 46;
            LowestAlgebraic.Slice(integer + 2).Fill(57);
        }

        bool isSigned = argument.isSigned;
        if (!isSigned) return new Numeric("0"u8, 0, 1, 0, new byte[1]);

        return new Numeric(LowestAlgebraic, 0, integer, fraction, new byte[integer + fraction + 2]);
    }

    public static void MAX(Numeric[] argument)
    {
        // TODO: Implement MAX
    }

    public static void MEAN(Numeric[] argument)
    {
        // TODO: Implement MEAN
    }

    public static void MEDIAN(Numeric[] argument)
    {
        // TODO: Implement MEDIAN
    }

    public static void MIDRANGE(Numeric[] argument)
    {
        // TODO: Implement MIDRANGE
    }
    
    public static void MIN(Numeric[] argument)
    {
        // TODO: Implement MIN
    }

    public static DecimalHolder MOD(Numeric left, Numeric right)
    {
        DecimalHolder mod = REM(left, right);
        if (mod < "0"u8) {
            mod = right.Bytes < new DecimalHolder("0"u8) ? mod - right.Bytes : mod + right.Bytes;
        }
        return mod;
    }

    public static void MODULE_NAME()
    {
        // TODO: Implement MODULE-NAME
    }

    public static void NATIONAL_OF()
    {
        // TODO: Implement NATIONAL types
    }

    public static DecimalHolder NUMVAL(Alphanumeric argument)
    {
        // Needs further testing, might not work properly
        // and doesn't implement all NUMVAL functionality
        return new DecimalHolder(argument.Bytes);
    }

    public static void NUMVAL_C(Alphanumeric argument)
    {
        // TODO: Implement NUMVAL-C
    }

    public static void NUMVAL_F(Alphanumeric argument)
    {
        // TODO: Implement NUMVAL-F
    }

    public static void ORD(Alphanumeric argument)
    {
        // TODO: Implement ORD
    }

    public static void ORD_MAX(Alphanumeric argument)
    {
        // TODO: Implement ORD-MAX
    }

    public static void ORD_MIN(Alphanumeric argument)
    {
        // TODO: Implement ORD-MIN
    }

    public static DecimalHolder PI()
    {
        return new DecimalHolder("3.141592653589793238462643383279503"u8);
    }

    public static void PRESENT_VALUE()
    {
        // TODO: Implement PRESENT-VALUE
    }

    public static void RANDOM(Numeric? argument)
    {
        // TODO: Implement RANDOM
    }

    public static void RANGE(Numeric? argument)
    {
        // TODO: Implement RANGE
        // This function depends on the functionality of MAX and MIN
    }

    public static DecimalHolder REM(Numeric left, Numeric right)
    {
        // The COBOL standard suggested this calculation:
        DecimalHolder subsidiaryQuotient = (new DecimalHolder(left.Bytes) / right.Bytes);
        if (subsidiaryQuotient.Bytes.IndexOf("."u8) > -1)
            subsidiaryQuotient = subsidiaryQuotient.Bytes.Slice(0, subsidiaryQuotient.Bytes.Length - 1);
        return (left.Bytes - (subsidiaryQuotient * right.Bytes));
    }

    public static string REVERSE(string argument)
    {
        char[] array = argument.ToCharArray();
        Array.Reverse(array);
        return new String(array);
    }

    public static void SECONDS_FROM_FORMATTED_TIME(Alphanumeric format, Alphanumeric time)
    {
        // TODO: Implement SECONDS-FROM-FORMATTED-TIME
    }

    public static Numeric SECONDS_PAST_MIDNIGHT()
    {
        DateTime timeNow = DateTime.Now;
        string TotalSeconds = timeNow.TimeOfDay.TotalSeconds.ToString();
        ReadOnlySpan<byte> bytes = Encoding.UTF8.GetBytes(TotalSeconds);
        return new Numeric(bytes, 0, bytes.Length, 0, new byte[bytes.Length]);
    }

    public static int SIGN(Numeric argument)
    {
        DecimalHolder temporary = argument.Bytes;
        if (temporary < "0"u8)
            return -1;

        if (temporary == "0"u8)
            return 0;

        return 1;
    }

    public static DecimalHolder SIN(DecimalHolder radians)
    {
        DecimalHolder Pie = PI();
        radians = radians % (Pie * "2"u8);

        if (radians < "0"u8)
            radians = "2"u8 * Pie - radians;

        DecimalHolder sign = "1"u8;
        if (radians > Pie)
        {
            radians -= Pie;
            sign = "-1"u8;
        }

        DecimalHolder result = radians;
        DecimalHolder coefficient = "3"u8;

        for (int i = 0; i < 10; i++)
        {
            DecimalHolder power = DecimalMath.Pow(radians.Bytes, coefficient.Bytes);
            DecimalHolder factorial = FACTORIAL(coefficient);

            if(i % 2 == 0)
                result = result - power / factorial;

            else
                result = result + power / factorial;


            coefficient = coefficient + "2"u8;
        }

        return sign * result;
    }


    public static void SMALLEST_ALGEBRAIC()
    {
        // TODO: Implement SMALLEST-ALGEBRAIC
    }

    public static Numeric SQRT(Numeric argument)
    {
        DecimalHolder temporary = DecimalMath.Sqrt(argument.Bytes);

        int indexOfDecimal = temporary.Bytes.IndexOf("."u8);
        if (indexOfDecimal == -1)
        {
            return new Numeric(temporary.Bytes, 0, 34, 0, new byte[34]);
        }

        return new Numeric(temporary.Bytes, 0, indexOfDecimal, 34 - indexOfDecimal + 1, new byte[34]);
    }

    public static void STANDARD_COMPARE(Alphanumeric argument)
    {
        // TODO: Implement STANDARD-COMPARE
    }

    public static void STANDARD_DEVIATION(Alphanumeric argument)
    {
        // TODO: Implement STANDARD-DEVIATION
    }

    public static void SUBSTITUTE(string argument, string replace, string to)
    {
        // TODO: Implement SUBSTITUTE
        // Should be possible with String.Replace()
    }

    public static Numeric SUM(params Numeric[] argument)
    {
        DecimalHolder sum = "0"u8;
        foreach (Numeric dec in argument)
        {
            sum += dec.Bytes;
        }

        int indexOfDecimal = sum.Bytes.IndexOf("."u8);
        if (indexOfDecimal == -1)
        {
            return new Numeric(sum.Bytes, 0, 34, 0, new byte[34]);
        }

        return new Numeric(sum.Bytes, 0, indexOfDecimal, 34 - indexOfDecimal + 1, new byte[34]);
    }

    public static DecimalHolder TAN(DecimalHolder argument)
    {
        return SIN(argument) / COS(argument);
    }

    public static void TEST_DATE_YYYYMMDD(DecimalHolder argument)
    {
        // TODO: Implement TEST-DATE-YYYYMMDD
    }

    public static void TEST_DAY_YYYYDDD(DecimalHolder argument)
    {
        // TODO: Implement TEST-DAY-YYYYDDD
    }

    public static void TEST_FORMATTED_DATETIME(DecimalHolder argument)
    {
        // TODO: Implement TEST-FORMATTED-DATETIME
    }

    public static void TEST_NUMVAL(DecimalHolder argument)
    {
        // TODO: Implement TEST-NUMVAL
    }

    public static void TEST_NUMVAL_C(DecimalHolder argument)
    {
        // TODO: Implement TEST-NUMVAL-c
    }

    public static void TEST_NUMVAL_F(DecimalHolder argument)
    {
        // TODO: Implement TEST-NUMVAL-F
    }

    public static string TRIM(string argument, string operation, char character = ' ')
    {
        if (operation == "LEADING")
            return argument.TrimStart(character);

        if (operation == "TRAILING")
            return argument.TrimEnd(character);    

        return argument.Trim(character);
    }

    public static string UPPER_CASE(string argument, string? locale)
    {
        return argument.ToUpper();
    }

    public static string WHEN_COMPILED(string argument)
    {
        // This is a compile time function instead of a runtime function
        // Might have to set a constant at compile time for this
        return argument;
    }

    public static void YEAR_TO_YYYY(DecimalHolder argument)
    {
        // TODO: Implement YEAR-TO-YYYY
    }
}
