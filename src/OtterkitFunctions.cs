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
        { // (argument-1 / (1 – (1 + argument-1)** (– (argument-2))))
            return 1 / periods;
        }
        return interest / (1 - Decimal128.Pow((1 + interest).ToString(), (-periods).ToString()));
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
        // TODO: Might not work properly, need test with NATIONAL types
        int parseInt = int.Parse(argument.Value);
        return ((char)parseInt).ToString();
    }

    public static Decimal128 COMBINED_DATETIME(Decimal128 date, Decimal128 time)
    {
        // TODO: implement other date time intrinsic functions
        return date;
    }

    public static Alphanumeric CONCAT(Alphanumeric[] strings, int length)
    {
        Alphanumeric concat = new(String.Concat(strings), length);
        return concat;
    }

    public static Alphabetic CONCAT(Alphabetic[] strings, int length)
    {
        Alphabetic concat = new(String.Concat(strings), length);
        return concat;
    }

    public static void CONVERT(Decimal128 value, Decimal128 source, Decimal128 targer)
    {
        // TODO: Need to implement other COBOL data types first
    }

    public static void COS(Decimal128 date)
    {
        // TODO: implement COS
    }

    public static Alphanumeric CURRENT_DATE()
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

        return new Alphanumeric(DatePlusOffset, 21);
    }

    public static void DATE_OF_INTEGER(Decimal128 date)
    {
        // TODO: implement DATE-OF-INTEGER
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

    public static int FACTORIAL(int argument)
    {
        if (argument == 0 || argument == 1)
        {
            return 1;
        }

        if (argument == 2)
        {
            return 2;
        }

		int factorial = 0;
		while (argument > 0)
		{
			factorial = argument;
			for (int i = factorial - 1; i > 0; i--)
			{
				factorial *= i;
			}
			argument--;
		}

        return factorial;
    }

    public static void FIND_STRING(Alphanumeric argument, Alphanumeric substring, int matches)
    {
        // TODO: Implement FIND-STRING
        //  Might be possible with substring and String.IndexOf()
    }

     public static Alphanumeric FORMATTED_CURRENT_DATE(string format)
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

        return new Alphanumeric(DatePlusOffset, 32);
    }

    public static void FORMATTED_DATE(int date, string format)
    {
        // TODO: Implement FORMATTED-DATE
    }

    public static void FORMATTED_DATETIME(int date, string format)
    {
        // TODO: Implement FORMATTED-DATETIME
    }

    public static void FORMATTED_TIME(Alphanumeric format, Decimal128 seconds, int offset)
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
        int integer = argument.integerLength;
        int fraction = argument.fractionalLength;
        string isDecimal = fraction == 0 ? "" : "." + new String('9', fraction);
        return new Decimal128("+" + new String('9', integer) + isDecimal);
    }

    public static void INTEGER_OF_BOOLEAN(int argument)
    {
        // TODO: Implement COBOL boolean type
    }

    public static void INTEGER_OF_DATE(int argument)
    {
        // TODO: Implement INTEGER-OF-DATE
    }

    public static void INTEGER_OF_DAY(int argument)
    {
        // TODO: Implement INTEGER-OF-DAY
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

    public static int LENGTH(Alphanumeric argument)
    {
        // Does not cover all LENGTH functionality
        return argument.Value.Length;
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

    public static Alphanumeric LOWER_CASE(Alphanumeric argument, string? locale)
    {
        return new Alphanumeric(argument.Value.ToLower(), argument.stringLength);
    }

    public static Alphabetic LOWER_CASE(Alphabetic argument, string? locale)
    {
        return new Alphabetic(argument.Value.ToLower(), argument.stringLength);
    }

    public static Decimal128 LOWEST_ALGEBRAIC(Numeric argument)
    {
        int integer = argument.integerLength;
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
        Decimal128 mod = left % right;
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
        // and doesn't not implement all NUMVAL functionality
        return new Decimal128(argument.Value);
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
        return left % right;
    }

    public static Alphanumeric REVERSE(Alphanumeric argument)
    {
        char[] array = argument.Value.ToCharArray();
        Array.Reverse(array);
        string reversed = new String(array);
        return new Alphanumeric(reversed, argument.stringLength);
    }

    public static void SECONDS_FROM_FORMATTED_TIME(Alphanumeric format, Alphanumeric time)
    {
        // TODO: Implement SECONDS-FROM-FORMATTED-TIME
    }

    public static void SECONDS_PAST_MIDNIGHT()
    {
        // TODO: Implement SECONDS-PAST-MIDNIGHT
    }

    public static void SIGN()
    {
        // TODO: Implement SIGN
    }

    public static void SIN()
    {
        // TODO: Implement SIN
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

    public static void SUBSTITUTE(Alphanumeric argument)
    {
        // TODO: Implement SUBSTITUTE
        // Should be possible with String.Replace()
    }

    public static Decimal128 SUM(Decimal128[] argument)
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

    public static void TRIM(Decimal128 argument)
    {
        // TODO: Implement TRIM
    }

    public static Alphabetic UPPER_CASE(Alphabetic argument, string? locale)
    {
        return new Alphabetic(argument.Value.ToUpper(), argument.stringLength);
    }

    public static void WHEN_COMPILED(Decimal128 argument)
    {
        // TODO: Implement WHEN-COMPILED
        // This is a compile time function instead of a runtime function
        // Might have to set a constant at compile time for this
    }

    public static void YEAR_TO_YYYY(Decimal128 argument)
    {
        // TODO: Implement YEAR-TO-YYYY
    }
}
