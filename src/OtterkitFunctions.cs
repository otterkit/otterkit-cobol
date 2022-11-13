using System.Text.RegularExpressions;
namespace OtterkitLibrary;

public static class Functions
{
    public static Decimal128 ABS(Decimal128 argument)
    {
        return Decimal128.Abs(argument);
    }

    public static void ACOS(Decimal128 argument)
    {
        // TODO ACOS
    }

    public static Decimal128 ANNUITY(Decimal128 interest, Decimal128 periods)
    {
        if (interest == Decimal128.Zero)
        {
            return 1 / periods;
        }
        // (argument-1 / (1 – (1 + argument-1)** (– (argument-2))))
        return interest / (1 - Decimal128.Pow((1 + interest.Value), (-periods).Value));
    }

    public static void ASIN(Decimal128 argument)
    {
        // TODO ASIN
    }

    public static void ATAN(Decimal128 argument)
    {
        // TODO ATAN
    }

    public static void BASE_CONVERT(Decimal128 input, Decimal128 current, Decimal128 target)
    {
        // TODO BASE-CONVERT
    }

    public static void BOOLEAN_OF_INTEGER(Decimal128 argument)
    {
        // TODO BOOLEAN-OF-INTEGER
        // Need to implement usage bit first
    }

    public static Decimal128 BYTE_LENGTH(Decimal128 argument)
    {
        // Does not cover all BYTE-LENGTH functionality
        return argument.Value.Length;
    }

    public static string CHAR(Decimal128 argument)
    {
        int parseInt = int.Parse(argument.Value);
        return ((char)parseInt).ToString();
    }

    public static string CHAR_NATIONAL(Decimal128 argument)
    {
        int parseInt = int.Parse(argument.Value);
        return ((char)parseInt).ToString();
    }

    public static Decimal128 COMBINED_DATETIME(Decimal128 date, Decimal128 time)
    {
        // TODO: implement other date time intrinsic functions
        return date;
    }

    public static string CONCAT(params string[] strings)
    {
        string concat = String.Concat(strings);
        return concat;
    }

    public static void CONVERT(Decimal128 value, Decimal128 source, Decimal128 target)
    {
        // TODO: Need to implement other COBOL data types first
    }

    public static Decimal128 COS(Decimal128 radians)
    {
        return SIN(PI() / 2 - radians);
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

    public static void DATE_OF_INTEGER(Decimal128 date)
    {
        DateTime Y1600 = new(1600, 12, 31);
        int intValue = int.Parse(date.Value);

        DateTime dateOfInteger = Y1600.AddDays(intValue);
    }

    public static void DATE_TO_YYYYMMDD(Decimal128 date)
    {
        // TODO: implement DATE-TO-YYYYMMDD
    }

    public static void DAY_OF_INTEGER(Decimal128 date)
    {
        // TODO: implement DAY-OF-INTEGER
    }

    public static void DAY_TO_YYYYDDD(Decimal128 date)
    {
        // TODO: implement DAY-TO-YYYYDDD
    }

    public static void DISPLAY_OF(Decimal128 date)
    {
        // TODO: implement National type
    }

    public static Decimal128 E()
    {
        return new Decimal128("2.718281828459045235360287471352662");
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

    public static Decimal128 EXP(Decimal128 exponent)
    {
        return Decimal128.Exp(exponent.Value);
    }

    public static Decimal128 EXP10(Decimal128 exponent)
    {
        return Decimal128.Pow("10", exponent.Value);
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

    public static void FORMATTED_TIME(string format, Decimal128 seconds, int offset)
    {
        // TODO: Implement FORMATTED-TIME
    }

    public static Decimal128 FRACTION_PART(Decimal128 argument)
    {
        int indexOfDecimal = argument.Value.IndexOf(".");
        string fractionPart = indexOfDecimal > 0 
            ? argument.Value.Substring(indexOfDecimal + 1)
            : "0";
        return new Decimal128(new String("0." + fractionPart));
    }

    public static Decimal128 HIGHEST_ALGEBRAIC(Numeric argument)
    {
        int integer = argument.length;
        int fraction = argument.fractionalLength;
        string isDecimal = fraction == 0 ? "" : "." + new String('9', fraction);
        return new Decimal128("+" + new String('9', integer) + isDecimal);
    }

    public static Decimal128 INTEGER_OF_BOOLEAN(Boolean argument)
    {
        return Convert.ToInt64(argument.Value, 2);
    }

    public static int INTEGER_OF_DATE(Decimal128 argument)
    {
        DateTime Y1600 = new(1600, 12, 31);
        int intValue = int.Parse(argument.Value);
        // Datetime constructor => Year, Month, Day
        // Calculations below are to remove the appropriate parts from the argument
        // It Just Works™
        DateTime current = new((intValue / 10000),(intValue / 100) - (intValue / 10000 * 100),intValue - (intValue / 100 * 100));
        return (current.Date - Y1600.Date).Days;
    }

    public static int INTEGER_OF_DAY(Decimal128 argument)
    {
        DateTime Y1600 = new(1600, 12, 31);
        int intValue = int.Parse(argument.Value);
        // Datetime constructor => Year, Month, Day
        // Same idea as the INTEGER_OF_DATE function, but from Julian date form
        // It Just Works™
        DateTime fromJulianYear = new((intValue - intValue % 1000) / 1000, 1, 1);
        DateTime fromJulianDays = fromJulianYear.AddDays(intValue % 1000 - 1);
        return (fromJulianDays.Date - Y1600.Date).Days;
    }

    public static void INTEGER_OF_FORMATTED_DATE(int argument)
    {
        // TODO: Implement INTEGER-OF-FORMATTED-DATE
    }

    public static Decimal128 INTEGER_PART(Decimal128 argument)
    {
        int indexOfDecimal = argument.Value.IndexOf(".");
        string IntegerPart = indexOfDecimal > 0 
            ? argument.Value.Substring(0, indexOfDecimal)
            : argument.Value;
        return new Decimal128(IntegerPart);
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

    public static Decimal128 LOG(Decimal128 argument)
    {
        return Decimal128.NaturalLog(argument.Value);
    }

    public static Decimal128 LOG10(Decimal128 argument)
    {
        return Decimal128.Log10(argument.Value);
    }

    public static string LOWER_CASE(string argument, string? locale)
    {
        return argument.ToLower();
    }

    public static Decimal128 LOWEST_ALGEBRAIC(Numeric argument)
    {
        int integer = argument.length;
        int fraction = argument.fractionalLength;
        string isDecimal = fraction == 0 ? "" : "." + new String('9', fraction);
        bool isSigned = argument.isSigned;
        if (!isSigned) return new Decimal128("0");

        return new Decimal128("-" + new String('9', integer) + isDecimal);
    }

    public static void MAX(Decimal128[] argument)
    {
        // TODO: Implement MAX
    }

    public static void MEAN(Decimal128[] argument)
    {
        // TODO: Implement MEAN
    }

    public static void MEDIAN(Decimal128[] argument)
    {
        // TODO: Implement MEDIAN
    }

    public static void MIDRANGE(Decimal128[] argument)
    {
        // TODO: Implement MIDRANGE
    }
    
    public static void MIN(Decimal128[] argument)
    {
        // TODO: Implement MIN
    }

    public static Decimal128 MOD(Decimal128 left, Decimal128 right)
    {
        Decimal128 mod = REM(left, right);
        if (mod < 0) {
            mod = right < 0 ? mod - right : mod + right;
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

    public static Decimal128 NUMVAL(Alphanumeric argument)
    {
        // Needs further testing, might not work properly
        // and doesn't implement all NUMVAL functionality
        return new Decimal128(argument.Bytes.ToString());
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

    public static Decimal128 PI()
    {
        return new Decimal128("3.141592653589793238462643383279503");
    }

    public static void PRESENT_VALUE()
    {
        // TODO: Implement PRESENT-VALUE
    }

    public static void RANDOM(Decimal128? argument)
    {
        // TODO: Implement RANDOM
    }

    public static void RANGE(Decimal128? argument)
    {
        // TODO: Implement RANGE
        // This function depends on the functionality of MAX and MIN
    }

    public static Decimal128 REM(Decimal128 left, Decimal128 right)
    {
        // The COBOL standard suggested this calculation:
        string subsidiaryQuotient = (left / right).Value;
        if (subsidiaryQuotient.Contains('.'))
            subsidiaryQuotient = subsidiaryQuotient.Remove(subsidiaryQuotient.Length - 1) + "0";
        return (left - (subsidiaryQuotient * right));
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

    public static Decimal128 SECONDS_PAST_MIDNIGHT()
    {
        DateTime timeNow = DateTime.Now;
        return timeNow.TimeOfDay.TotalSeconds;
    }

    public static int SIGN(Numeric argument)
    {
        if (argument.dataItem < Decimal128.Zero)
            return -1;

        if (argument.dataItem == Decimal128.Zero)
            return 0;

        return 1;
    }

    public static Decimal128 SIN(Decimal128 radians)
    {
        Decimal128 Pie = PI();
        radians = radians % (Pie * 2);

        if (radians < 0)
            radians = 2 * Pie - radians;

        sbyte sign = 1;
        if (radians > Pie)
        {
            radians -= Pie;
            sign = -1;
        }

        Decimal128 result = radians;
        int coefficient = 3;

        for (int i = 0; i < 10; i++)
        {
            Decimal128 power = Decimal128.Pow(radians.Value, coefficient.ToString());
            Int128 factorial = FACTORIAL(coefficient);

            if(i % 2 == 0)
                result -= power / factorial;
            else
                result += power / factorial;

            coefficient += 2;
        }

        return sign * result;
    }

    public static void SMALLEST_ALGEBRAIC()
    {
        // TODO: Implement SMALLEST-ALGEBRAIC
    }

    public static Decimal128 SQRT(Decimal128 argument)
    {
        return Decimal128.Sqrt(argument.Value);
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

    public static Decimal128 SUM(params Decimal128[] argument)
    {
        Decimal128 sum = 0;
        foreach (Decimal128 dec in argument)
        {
            sum += dec;
        }
        return sum;
    }

    public static void TAN(Decimal128 argument)
    {
        // TODO: Implement TAN
    }

    public static void TEST_DATE_YYYYMMDD(Decimal128 argument)
    {
        // TODO: Implement TEST-DATE-YYYYMMDD
    }

    public static void TEST_DAY_YYYYDDD(Decimal128 argument)
    {
        // TODO: Implement TEST-DAY-YYYYDDD
    }

    public static void TEST_FORMATTED_DATETIME(Decimal128 argument)
    {
        // TODO: Implement TEST-FORMATTED-DATETIME
    }

    public static void TEST_NUMVAL(Decimal128 argument)
    {
        // TODO: Implement TEST-NUMVAL
    }

    public static void TEST_NUMVAL_C(Decimal128 argument)
    {
        // TODO: Implement TEST-NUMVAL-c
    }

    public static void TEST_NUMVAL_F(Decimal128 argument)
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

    public static void YEAR_TO_YYYY(Decimal128 argument)
    {
        // TODO: Implement YEAR-TO-YYYY
    }
}
