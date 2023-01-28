using System.Text.RegularExpressions;
using System.Text;
namespace OtterkitLibrary;

public static class Functions
{
    public static Numeric ABS(Numeric argument)
    {
        DecimalHolder input = new(Encoding.UTF8.GetBytes(argument.Display));
        DecimalHolder abs = DecimalMath.Abs(input.Bytes);
        return new Numeric(abs, false);

    }

    public static Numeric ACOS(Numeric ratio)
    {
        DecimalHolder Dratio = new(ratio.Bytes);
        if (Dratio < "-1"u8 || Dratio > "1"u8)
            throw new ArgumentException($"The argument of ACOS must be >= -1 and <= to +1");

        DecimalHolder param = ("1"u8 - Dratio * Dratio);
        DecimalHolder result = ATAN(new Numeric((DecimalMath.Sqrt(param.Bytes) / (Dratio + "1"u8)), true)) * new DecimalHolder("2"u8);
        return new Numeric(result, true);
    }

    public static Numeric ANNUITY(Numeric interest, Numeric periods)
    {
        DecimalHolder Dinterest = new(Encoding.UTF8.GetBytes(interest.Display));
        DecimalHolder Dperiods = new(Encoding.UTF8.GetBytes(periods.Display));
        if (Dinterest == "0"u8)
        {
            return new Numeric("1"u8 / Dperiods, false);
        }
        // (argument-1 / (1 – (1 + argument-1)** (– (argument-2))))
        DecimalHolder output = interest / (new DecimalHolder("1"u8) - DecimalMath.Pow(("1"u8 + Dinterest).Bytes, (-Dperiods).Bytes));
        return new Numeric(output, false); //an annuity is always positive, right? 
    }

    public static Numeric ASIN(Numeric ratio)
    {
        DecimalHolder Dratio = new(Encoding.UTF8.GetBytes(ratio.Display));
        if (Dratio < "-1"u8 || Dratio > "1"u8)
            throw new ArgumentException($"The argument of ASIN must be >= -1 and <= to +1");

        DecimalHolder param = ("1"u8 - Dratio * Dratio);
        DecimalHolder result = new DecimalHolder(Encoding.UTF8.GetBytes(ATAN(new Numeric(Dratio / (DecimalMath.Sqrt(param.Bytes) + new DecimalHolder("1"u8)), true)).Display)) * "2"u8;
        return new Numeric(result, true);
    }

    public static Numeric ATAN(Numeric ratio)
    {
        DecimalHolder Dratio = new(Encoding.UTF8.GetBytes(ratio.Display));
        DecimalHolder half_pi = new DecimalHolder(Encoding.UTF8.GetBytes(PI().Display)) / "2"u8;
        if (Dratio < "-1"u8)
            return new Numeric(-(half_pi) - new DecimalHolder(Encoding.UTF8.GetBytes(ATAN(new Numeric("1"u8 / Dratio, true)).Display)), true);

        if (Dratio > "1"u8)
            return new Numeric(half_pi - new DecimalHolder(Encoding.UTF8.GetBytes(ATAN(new Numeric("1"u8 / Dratio, true)).Display)), true);

        DecimalHolder coefficient = "2"u8;
        DecimalHolder iteration = Dratio / (Dratio * Dratio + "1"u8);
        DecimalHolder result = iteration;

        for (int i = 0; i < 64; i++)
        {
            iteration *= (Dratio * Dratio / (Dratio * Dratio + "1"u8) * coefficient / (coefficient + "1"u8));

            result += iteration;
            coefficient += "2"u8;
        }

        return new Numeric(result, true);
    }

    public static void BASE_CONVERT(Numeric input, Numeric current, Numeric target)
    {
        // TODO BASE-CONVERT
    }

    public static void BOOLEAN_OF_INTEGER(Numeric argument)
    {
        // TODO BOOLEAN-OF-INTEGER
        // Need to implement usage bit first
    }

    public static Numeric BYTE_LENGTH(Numeric argument)
    {
        // Does not cover all BYTE-LENGTH functionality
        DecimalHolder Dargument = new(Encoding.UTF8.GetBytes(argument.Display));
        DecimalHolder byteLength = "0"u8;
        foreach (var _ in Dargument.Bytes)
        {
            byteLength++;
        }
        return new Numeric(byteLength, false);
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

    public static Numeric COMBINED_DATETIME(Numeric date, Numeric time)
    {
        // TODO: implement other date time intrinsic functions
        return new Numeric(date, false);
    }

    public static string CONCAT(params string[] strings)
    {
        string concat = String.Concat(strings);
        return concat;
    }

    public static void CONVERT(Numeric value, Numeric source, Numeric target)
    {
        // TODO: Need to implement other COBOL data types first
    }

    public static Numeric COS(Numeric radians)
    {
        DecimalHolder sineArgument = new DecimalHolder(Encoding.UTF8.GetBytes(PI().Display)) / "2"u8 - radians.Bytes;
        return SIN(new Numeric(sineArgument, true));
    }

    public static Alphanumeric CURRENT_DATE()
    {
        DateTime currentDate = DateTime.Now;
        TimeSpan offset = TimeZoneInfo.Local.GetUtcOffset(DateTime.UtcNow);
        string formattedDate = currentDate.ToString("yyyyMMddHHmmssff");
        string DatePlusOffset;

        if (offset.ToString().Contains('+'))
        {
            DatePlusOffset = new String(formattedDate + "+" + offset.ToString("hhmm"));
        }
        else
        {
            DatePlusOffset = new String(formattedDate + "-" + offset.ToString("hhmm"));
        }

        Alphanumeric output = new(Encoding.UTF8.GetBytes(DatePlusOffset), 0, 8, new byte[8]);

        return output;
    }

    public static Numeric DATE_OF_INTEGER(Numeric date)
    {
        DateTime date_object = RuntimeHelpers.New_date(date); //Linter not recognizing OtterkitHelpers.cs?

        String date_stamp = date_object.ToString("yyyyMMdd");

        Numeric output = new(Encoding.UTF8.GetBytes(date_stamp), 0, 8, 0, new byte[8]);

        return output;
    }

    public static Numeric DATE_TO_YYYYMMDD(Numeric date, Numeric? window, Numeric? Current)
    {
        DecimalHolder ten_thousand = new(Encoding.UTF8.GetBytes("10000"));

        DecimalHolder date_dec = new(Encoding.UTF8.GetBytes(date.Display));
        DecimalHolder yy = date_dec / ten_thousand;
        //TODO: replace INTEGER_PART with proper INTEGER
        Numeric yy_num = INTEGER_PART(new Numeric(yy, false));

        DecimalHolder mmdd = date_dec % ten_thousand;

        Numeric yyyy = YEAR_TO_YYYY(yy_num, window, Current);
        DecimalHolder year = new(Encoding.UTF8.GetBytes(yyyy.Display));
        DecimalHolder result = (year * ten_thousand) + mmdd;

        return new Numeric(result, false);

    }

    public static Numeric DAY_OF_INTEGER(Numeric date)
    {
        DateTime date_object = RuntimeHelpers.New_date(date);
        int day_of_year = date_object.DayOfYear;

        String date_stamp = date_object.ToString("yyyy" + day_of_year);

        Numeric output = new(Encoding.UTF8.GetBytes(date_stamp), 0, 7, 0, new byte[7]);

        return output;
    }

    public static Numeric DAY_TO_YYYYDDD(Numeric date, Numeric? window, Numeric? Current)
    {
        DecimalHolder thousand = new(Encoding.UTF8.GetBytes("1000"));

        DecimalHolder date_dec = new(Encoding.UTF8.GetBytes(date.Display));
        DecimalHolder yy = date_dec / thousand;
        //TODO: replace INTEGER_PART with proper INTEGER
        Numeric yy_num = INTEGER_PART(new Numeric(yy, false));

        DecimalHolder nnn = date_dec % thousand;

        Numeric yyyy = YEAR_TO_YYYY(yy_num, window, Current);
        DecimalHolder year = new(Encoding.UTF8.GetBytes(yyyy.Display));
        DecimalHolder result = (year * thousand) + nnn;

        return new Numeric(result, false);
    }

    public static void DISPLAY_OF(Numeric date)
    {
        // TODO: implement National type
    }

    public static Numeric E()
    {
        DecimalHolder output = new("2.718281828459045235360287471352662"u8);
        return new Numeric(output, false);
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
        Numeric temporary = new(result.Bytes, 0, 32, 2, new byte[34]);
        return temporary;
    }

    public static Numeric EXP10(Numeric exponent)
    {
        DecimalHolder result = DecimalMath.Pow("10"u8, exponent.Bytes);
        Numeric temporary = new(result.Bytes, 0, 32, 2, new byte[34]);
        return temporary;
    }

    /*
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
    */

    public static Numeric FACTORIAL(Numeric argument)
    {
        // These two variables (FirstInteger and SecondInteger)
        // convert the bytes value from the DecimalHolder argument
        // into an int value that can be used for the switch statement
        // 48 is the position of 0 in UTF-8 encoded bytes
        DecimalHolder normalized = argument.Bytes[0] == 43 ? argument.Bytes[1..] : argument.Bytes;

        int FirstInteger = normalized.Bytes.Length == 2 ? ((normalized.Bytes[0] - 48) * 10) : normalized.Bytes[0] - 48;
        int SecondInteger = normalized.Bytes.Length == 2 ? normalized.Bytes[1] - 48 : 0;

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

        return new Numeric(factorial, false);
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
        foreach (Match str in Regex.Matches(argument, substring, anycase ? RegexOptions.IgnoreCase : RegexOptions.None).Cast<Match>())
        {
            matches.Add(str.Index + 1);
        }

        if (ignore >= matches.Count) return 0;

        return matches[ignore];
    }

    public static string FORMATTED_CURRENT_DATE(string format)
    {
        DateTime currentDate = DateTime.Now;
        TimeSpan offset = TimeZoneInfo.Local.GetUtcOffset(DateTime.UtcNow);
        string formattedDate = currentDate.ToString(format);
        string DatePlusOffset;

        if (offset.ToString().Contains('+'))
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

        argument.Bytes[(indexOfDecimal + 1)..].CopyTo(FractionPart[2..]);
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
            HighestAlgebraic[(integer + 2)..].Fill(57);
        }

        return new Numeric(HighestAlgebraic, 0, integer, fraction, new byte[integer + fraction + 2]);
    }

    public static Numeric INTEGER_OF_BOOLEAN(COBOLBoolean argument)
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
        DateTime current = new((intValue / 10000), (intValue / 100) - (intValue / 10000 * 100), intValue - (intValue / 100 * 100));
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
            ? argument.Bytes[..indexOfDecimal]
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

    public static Numeric LOG(Numeric argument)
    {
        DecimalHolder output = DecimalMath.Ln(argument);
        return new Numeric(output, false);
    }

    public static Numeric LOG10(Numeric argument)
    {
        DecimalHolder output = DecimalMath.Log10(argument);
        return new Numeric(output, false);
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
            LowestAlgebraic[(integer + 2)..].Fill(57);
        }

        bool isSigned = argument.IsSigned;
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

    public static Numeric MOD(Numeric left, Numeric right)
    {
        Numeric rem = REM(left, right);
        DecimalHolder mod = new(Encoding.UTF8.GetBytes(rem.Display));
        if (mod < "0"u8)
        {
            mod = right.Bytes < new DecimalHolder("0"u8) ? mod - right.Bytes : mod + right.Bytes;
        }
        return new Numeric(mod, false);
    }

    public static void MODULE_NAME()
    {
        // TODO: Implement MODULE-NAME
    }

    public static void NATIONAL_OF()
    {
        // TODO: Implement NATIONAL types
    }

    public static Numeric NUMVAL(Alphanumeric argument)
    {
        // Needs further testing, might not work properly
        // and doesn't implement all NUMVAL functionality
        return new Numeric(new DecimalHolder(argument.Bytes), false);
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

    public static Numeric PI()
    {
        DecimalHolder pi = new("3.141592653589793238462643383279503"u8);
        return new Numeric(pi, false);
    }

    public static void PRESENT_VALUE()
    {
        // TODO: Implement PRESENT-VALUE
    }

    public static Numeric RANDOM(Numeric? argument = null)
    {
        Random random;
        Encoding encoding = Encoding.UTF8;

        if (argument is null)
        {
            random = new();
        }

        else
        {
            DecimalHolder range = argument.Bytes[..Math.Min(9, argument.Bytes.Length)];
            int int_arg = int.Parse(range.Display); // The range variable above makes sure that it fits inside of an Int32
            random = new(int_arg);//what about fractional arguments? Accoridng to the standard, those are not supported
        }

        double output = random.NextDouble();
        string s_output = output.ToString();
        DecimalHolder d_output = new(encoding.GetBytes(s_output));//since the string type is UTF-16, would this work?
        return new Numeric(d_output, false);
    }

    public static void RANGE(Numeric? argument)
    {
        // TODO: Implement RANGE
        // This function depends on the functionality of MAX and MIN
    }

    public static Numeric REM(Numeric left, Numeric right)
    {
        // The COBOL standard suggested this calculation:
        DecimalHolder subsidiaryQuotient = new DecimalHolder(left.Bytes) / right.Bytes;
        if (subsidiaryQuotient.Bytes.IndexOf("."u8) > -1)
            subsidiaryQuotient = subsidiaryQuotient.Bytes[..^1];
        DecimalHolder output = left.Bytes - (subsidiaryQuotient * right.Bytes);
        return new Numeric(output, true);
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

    public static Numeric SIGN(Numeric argument)
    {
        DecimalHolder temporary = argument.Bytes;
        if (temporary < "0"u8)
            return new Numeric("-1"u8, 0, 2, 0, new byte[3]);
        if (temporary == "0"u8)
            return new Numeric("0"u8, 0, 1, 0, new byte[1]);

        return new Numeric("1"u8, 0, 1, 0, new byte[1]);
    }

    public static Numeric SIN(Numeric argument)
    {
        DecimalHolder Pie = new(Encoding.UTF8.GetBytes(PI().Display));
        DecimalHolder radians = argument;
        radians %= Pie * "2"u8;

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
            DecimalHolder factorial = new(Encoding.UTF8.GetBytes(FACTORIAL(new Numeric(coefficient, false)).Display));

            if (i % 2 == 0) result -= power / factorial;

            else result += power / factorial;

            coefficient += "2"u8;
        }

        DecimalHolder @return = sign * result;
        return new Numeric(@return, true);
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

    public static Numeric TAN(Numeric argument)
    {
        DecimalHolder tanArgument = new DecimalHolder(SIN(argument).Bytes) / COS(argument).Bytes;
        return new Numeric(tanArgument, true);
    }

    public static Numeric TEST_DATE_YYYYMMDD(Numeric argument)
    {
        DecimalHolder input = new(argument.Bytes);
        DecimalHolder ten_thousand = new("10000"u8);
        DecimalHolder tweleve_ninety_nine = new("1299"u8);
        DecimalHolder hundred = new("100"u8);
        DecimalHolder year_lower_bound = new("1601000"u8);
        DecimalHolder year_upper_bound = new("99999999"u8);

        DecimalHolder month_check = input % ten_thousand;
        DecimalHolder day_check_1 = input % hundred;
        Numeric day_check_2 = INTEGER_PART(new(day_check_1, false));
        int day_check_3 = int.Parse(day_check_2.Display);

        DecimalHolder day_m_check_1 = (input % ten_thousand) / hundred;
        //TODO: Replace INTEGER_PART with proper INTEGER
        Numeric day_m_check_2 = INTEGER_PART(new Numeric(day_m_check_1, false));
        int day_m_check_3 = int.Parse(day_m_check_2.Display);

        DecimalHolder day_y_check_1 = input / ten_thousand;
        Numeric day_y_check_2 = INTEGER_PART(new Numeric(day_y_check_1, false));
        int day_y_check_3 = int.Parse(day_y_check_2.Display);

        Boolean day_works = true;

        try
        {
            DateTime check_month = new(day_y_check_3, day_m_check_3, day_check_3);
        }
        catch (System.ArgumentOutOfRangeException)
        {

            day_works = false;
        }

        if ((input < year_lower_bound) || (input > year_upper_bound))
        {
            return new Numeric("1"u8, 0, 1, 0, new byte[1]);
        }
        else if ((month_check < hundred) || (month_check > tweleve_ninety_nine))
        {
            return new Numeric("2"u8, 0, 1, 0, new byte[1]);
        }
        else if ((day_check_3 < 1) || (!day_works))
        {
            return new Numeric("3"u8, 0, 1, 0, new byte[1]);
        }
        else
        {
            return new Numeric("0"u8, 0, 1, 0, new byte[1]);
        }


    }

    public static Numeric TEST_DAY_YYYYDDD(Numeric argument)
    {
        DecimalHolder input = argument.Bytes;
        DecimalHolder thousand = new("1000"u8);
        DecimalHolder year_lower_bound = new("1601000"u8);
        DecimalHolder year_upper_bound = new("9999999"u8);

        DecimalHolder day_check_1 = input % thousand;
        Numeric day_check_2 = new(day_check_1, false);
        int day_check_3 = int.Parse(day_check_2.Display);

        DecimalHolder y_check_1 = input / thousand;
        Numeric y_check_2 = INTEGER_PART(new Numeric(y_check_1, false));
        int y_check_3 = int.Parse(y_check_2.Display);

        Boolean day_works = true;

        DateTime check_year = new(y_check_3, 1, 1);
        check_year.AddDays(day_check_3);

        if (check_year.Year != y_check_3)
        {
            day_works = false;
        }

        if ((input < year_lower_bound) || (input > year_upper_bound))
        {
            return new Numeric("1"u8, 0, 1, 0, new byte[1]);
        }
        else if ((day_check_3 < 1) || (!day_works))
        {
            return new Numeric("2"u8, 0, 1, 0, new byte[1]);
        }
        else
        {
            return new Numeric("0"u8, 0, 1, 0, new byte[1]);
        }

    }

    public static void TEST_FORMATTED_DATETIME(Numeric argument)
    {
        // TODO: Implement TEST-FORMATTED-DATETIME
    }

    public static void TEST_NUMVAL(Numeric argument)
    {
        // TODO: Implement TEST-NUMVAL
    }

    public static void TEST_NUMVAL_C(Numeric argument)
    {
        // TODO: Implement TEST-NUMVAL-c
    }

    public static void TEST_NUMVAL_F(Numeric argument)
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

    public static Numeric YEAR_TO_YYYY(Numeric yy, Numeric? window, Numeric? current)
    {
        //TODO: Implement runtime exceptions for invalid inputs
        window ??= new Numeric(new DecimalHolder(Encoding.UTF8.GetBytes("50")), false);
        
        if (current == null)
        {
            //TODO: make sure NUMVAL works fully
            String slice = CURRENT_DATE().Display[..4];
            Alphanumeric date = new(Encoding.UTF8.GetBytes(slice), 0, 8, new byte[8]);
            current = NUMVAL(date);
        }

        DecimalHolder yy_num = new(Encoding.UTF8.GetBytes(yy.Display));
        DecimalHolder window_num = new(Encoding.UTF8.GetBytes(window.Display));
        DecimalHolder current_num = new(Encoding.UTF8.GetBytes(current.Display));

        DecimalHolder max_year = window_num + current_num;
        DecimalHolder hundred = new(Encoding.UTF8.GetBytes("100"));
        DecimalHolder one = new(Encoding.UTF8.GetBytes("1"));
        //TODO: replace with true INTEGER function
        if ((max_year % hundred) >= yy_num)
        {
            Numeric part = INTEGER_PART(new Numeric((max_year / hundred), false));
            DecimalHolder coefficient = new(Encoding.UTF8.GetBytes(part.Display));
            DecimalHolder result = (yy_num + hundred * coefficient);
            return new Numeric(result, false);
        }
        else
        {
            Numeric part = INTEGER_PART(new Numeric((max_year / hundred), false));
            DecimalHolder coefficient = new(Encoding.UTF8.GetBytes(part.Display));
            coefficient -= one;
            DecimalHolder result = (yy_num + hundred * coefficient);
            return new Numeric(result, false);
        }
    }
}
