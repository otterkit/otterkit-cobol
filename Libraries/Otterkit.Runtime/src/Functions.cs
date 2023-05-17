using System.Text.RegularExpressions;
using System.Text;
using Otterkit.Numerics;

namespace Otterkit.Runtime;

public static class Functions
{
    public static Temporary ABS<T>(T argument) where T : ICOBOLType
    {
        Span<byte> result = stackalloc byte[45];

        var d128 = new Decimal128(argument.Bytes);

        var abs = Decimal128.Abs(d128);

        var length = abs.AsSpan(result);

        return new Temporary(result.Slice(0, length));
    }

    public static Temporary ACOS<T>(T ratio) where T : ICOBOLType
    {
        Span<byte> result = stackalloc byte[45];

        var d128 = new Decimal128(ratio.Bytes);

        var abs = Decimal128.Acos(d128);

        var length = abs.AsSpan(result);

        return new Temporary(result.Slice(0, length));
    }

    public static Numeric ANNUITY(Numeric interest, Numeric periods)
    {
        Decimal128 Dinterest = interest;
        Decimal128 Dperiods = periods;
        if (Dinterest == Decimal128.Zero)
        {
            return Decimal128.One / Dperiods;
        }

        // (argument-1 / (1 – (1 + argument-1)** (– (argument-2))))
        Decimal128 output = Dinterest / (1 - Decimal128.Pow(Decimal128.One + Dinterest, -Dperiods));
        return output; //an annuity is always positive, right? 
    }

    public static Temporary ASIN<T>(T ratio) where T : ICOBOLType
    {
        Span<byte> result = stackalloc byte[45];

        var d128 = new Decimal128(ratio.Bytes);

        var abs = Decimal128.Asin(d128);

        var length = abs.AsSpan(result);

        return new Temporary(result.Slice(0, length));
    }

    public static Temporary ATAN<T>(T ratio) where T : ICOBOLType
    {
        Span<byte> result = stackalloc byte[45];

        var d128 = new Decimal128(ratio.Bytes);

        var abs = Decimal128.Atan(d128);

        var length = abs.AsSpan(result);

        return new Temporary(result.Slice(0, length));
    }

    public static void BASE_CONVERT(Numeric input, Numeric current, Numeric target)
    {
        // TODO BASE-CONVERT
    }

    public static Temporary BOOLEAN_OF_INTEGER(Numeric argument, Option<Numeric> length)
    {
        // TODO BOOLEAN-OF-INTEGER

        Decimal128 input = argument;
        // TODO: Return runtime error if input is not an integer
        Decimal128 two = 2;

        Stack<Decimal128> digits = new Stack<Decimal128>();
        
        Decimal128 current = Decimal128.Abs(input);

        while(current/two != 0){
            Decimal128 remainder = Decimal128.Remainder(current, 2); //Decimal128.RemainderNear outputs negative numbers, which breaks the calculation.
            digits.Push(remainder);
            current = Decimal128.Divide(current-remainder, 2);
        }

        var text = new StringBuilder();


        foreach(Decimal128 bit in digits){
            text.Append(bit.ToString());
        }

        //string trueText = text.ToString();

        string trueText;
        if (length.Exists)
        {
            var ulength = length.Unwrap();

            var len = int.Parse(ulength.Display);

            if(len < text.Length)
            {
                int difference = text.Length - len;
                trueText = text.ToString(difference, len);
            } 
            else if (len > text.Length)
            {
                int difference = len - text.Length;
                for(int i = 0; i < difference; i++)
                {
                    text.Insert(0 , "0");
                }

                trueText = text.ToString();

            } 
            else 
            {
                trueText = text.ToString();
            }

        } else 
        {
            trueText = text.ToString();
        }
        if(input < 0)
        {
            trueText = "-" + trueText;
        }

        return new Temporary(Encoding.UTF8.GetBytes(trueText));
       

    }

    public static Numeric BYTE_LENGTH(Numeric argument)
    {
        // Does not cover all BYTE-LENGTH functionality
        Decimal128 byteLength = 0;
        foreach (var _ in argument.Bytes)
        {
            byteLength++;
        }
        return byteLength;
    }

    public static Temporary CHAR(Numeric argument)
    {
        ReadOnlySpan<byte> bytes = argument.Bytes;
        int parseInt = int.Parse(Encoding.UTF8.GetString(bytes));
        ReadOnlySpan<byte> charAsByte = new(((byte)parseInt));

        return new Temporary(charAsByte);
    }

    public static string CHAR_NATIONAL(Numeric argument)
    {
        int parseInt = int.Parse(argument.Display);
        return ((char)parseInt).ToString();
    }

    public static Numeric COMBINED_DATETIME(Numeric date, Numeric time)
    {
        // TODO: implement other date time intrinsic functions
        return date;
    }

    public static Temporary CONCAT(params ICOBOLType[] inputs)
    {
        string[] strings = new String[inputs.Length];
        for (int i = 0; i < inputs.Length; i++)
        {
            strings[i] = inputs[i].Display;
        }
        string concat = String.Concat(strings);

        string returnType = inputs[1].GetType().Name.ToString();

        return new Temporary(Encoding.UTF8.GetBytes(concat));
    }

    public static void CONVERT(Numeric value, Numeric source, Numeric target)
    {
        // TODO: Need to implement other COBOL data types first
    }

    public static Temporary COS<T>(T ratio) where T : ICOBOLType
    {
        Span<byte> result = stackalloc byte[45];

        var d128 = new Decimal128(ratio.Bytes);

        var abs = Decimal128.Cos(d128);

        var length = abs.AsSpan(result);

        return new Temporary(result.Slice(0, length));
    }

    public static Temporary CURRENT_DATE()
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

        return new Temporary(Encoding.UTF8.GetBytes(DatePlusOffset));
    }

    public static Numeric DATE_OF_INTEGER(Numeric date)
    {
        DateTime date_object = RuntimeHelpers.New_date(date); //Linter not recognizing OtterkitHelpers.cs?

        String date_stamp = date_object.ToString("yyyyMMdd");

        Numeric output = new(Encoding.UTF8.GetBytes(date_stamp), 0, 8, 0, new byte[8]);

        return output;
    }

    public static Numeric DATE_TO_YYYYMMDD(Numeric date, Option<Numeric> window, Option<Numeric> Current)
    {
        Decimal128 ten_thousand = new(Encoding.UTF8.GetBytes("10000"));

        Decimal128 date_dec = new(Encoding.UTF8.GetBytes(date.Display));
        Decimal128 yy = date_dec / ten_thousand;
        //TODO: replace INTEGER_PART with proper INTEGER
        Numeric yy_num = INTEGER_PART(yy);

        Decimal128 mmdd = date_dec % ten_thousand;

        Numeric yyyy = YEAR_TO_YYYY(yy_num, window, Current);
        Decimal128 year = new(Encoding.UTF8.GetBytes(yyyy.Display));
        Decimal128 result = (year * ten_thousand) + mmdd;

        return result;

    }

    public static Numeric DAY_OF_INTEGER(Numeric date)
    {
        DateTime date_object = RuntimeHelpers.New_date(date);
        int day_of_year = date_object.DayOfYear;

        String date_stamp = date_object.ToString("yyyy" + day_of_year);

        Numeric output = new(Encoding.UTF8.GetBytes(date_stamp), 0, 7, 0, new byte[7]);

        return output;
    }

    public static Numeric DAY_TO_YYYYDDD(Numeric date, Option<Numeric> window, Option<Numeric> Current)
    {
        Decimal128 thousand = new(Encoding.UTF8.GetBytes("1000"));

        Decimal128 date_dec = new(Encoding.UTF8.GetBytes(date.Display));
        Decimal128 yy = date_dec / thousand;
        //TODO: replace INTEGER_PART with proper INTEGER
        Numeric yy_num = INTEGER_PART(yy);

        Decimal128 nnn = date_dec % thousand;

        Numeric yyyy = YEAR_TO_YYYY(yy_num, window, Current);
        Decimal128 year = new(Encoding.UTF8.GetBytes(yyyy.Display));
        Decimal128 result = (year * thousand) + nnn;

        return result;
    }

    public static void DISPLAY_OF(Numeric date)
    {
        // TODO: implement National type
    }

    public static Temporary E()
    {
        Span<byte> result = stackalloc byte[45];

        var length = Decimal128.E.AsSpan(result);

        return new Temporary(result.Slice(0, length));
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
        return Decimal128.Exp(exponent);
    }

    public static Numeric EXP10(Numeric exponent)
    {
        return Decimal128.Pow(10, exponent);
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
        // convert the bytes value from the Decimal128 argument
        // into an int value that can be used for the switch statement
        // 48 is the position of 0 in UTF-8 encoded bytes
        ReadOnlySpan<byte> normalized = argument.Bytes[0] == 43 ? argument.Bytes[1..] : argument.Bytes;

        int firstInteger = normalized.Length == 2 ? ((normalized[0] - 48) * 10) : normalized[0] - 48;
        int secondInteger = normalized.Length == 2 ? normalized[1] - 48 : 0;

        int integer = firstInteger + secondInteger;

        return Decimal128.Factorial(integer);
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

    public static void FORMATTED_TIME(string format, Decimal128 seconds, int offset)
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

    public static Numeric INTEGER_OF_BOOLEAN(Bit argument)
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

    public static Numeric LENGTH(ICOBOLType argument)//Technically, this is supposed to only be national, alphanumeric, or bit/boolean
    {
        // Does not cover all LENGTH functionality
        string inside = argument.Display;
        int len = inside.Length;
        return new Numeric(Encoding.UTF8.GetBytes(len.ToString()), 0, len.ToString().Length, 0, new byte[len.ToString().Length]);
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
        return Decimal128.Ln(argument);
    }

    public static Numeric LOG10(Numeric argument)
    {
        return Decimal128.Log10(argument);
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

    public static Numeric MAX(Numeric[] argument)
    {
        Numeric max = new Numeric("-9999999999999999999999999999999999"u8, 0, 34, 0, new byte[35]);
        foreach(Numeric element in argument){
            if (element > max){
                max = element;
            }
        }
        return max;
    }

    public static Numeric MEAN(Numeric[] argument)
    {
        Numeric sum = new Numeric("+0"u8, 0, 1, 0, new byte[2]);
        foreach (Numeric element in argument){
            sum = sum + element;
        }
        Decimal128 d_len = new Decimal128(Encoding.UTF8.GetBytes(argument.Length.ToString()));
        Numeric len = d_len;
        Numeric avg = sum/len;

        return avg;

    }

    public static Numeric MEDIAN(Numeric[] argument)
    {
        Numeric[] copy = new Numeric[argument.Length];
        Numeric two = new Numeric("2"u8, 0, 1, 0, new byte[1]);
        Numeric result;
        Array.Copy(argument, copy, argument.Length);
        Array.Sort(copy);
        if ((copy.Length % 2) == 0 ){
            int left = copy.Length/2-1;
            int right = ((copy.Length)/2);
            Numeric lower = copy[left];
            Numeric upper = copy[right];
            result = ((lower+upper)/two);
        } else {
            int index = (int)Math.Ceiling((double)copy.Length/2)-1;
            result = copy[index];
        }
        return result;
    }

    public static Numeric MIDRANGE(Numeric[] argument)
    {
        Numeric two = new Numeric("2"u8, 0, 1, 0, new byte[1]);
        Numeric min = MIN(argument);
        Numeric max = MAX(argument);
        return ((min + max)/two);
    }

    public static Numeric MIN(Numeric[] argument)
    {
        Numeric min = new Numeric("9999999999999999999999999999999999"u8, 0, 34, 0, new byte[34]);
        foreach(Numeric element in argument){
            if (element < min){
                min = element;
            }
        }
        return min;

    }

    public static Numeric MOD(Numeric left, Numeric right)
    {
        Numeric rem = REM(left, right);
        Decimal128 mod = rem;
        if (mod < 0)
        {
            mod = (Decimal128)right < 0 ? mod - (Decimal128)right : mod + (Decimal128)right;
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

    public static Temporary NUMVAL<T>(T argument) where T : ICOBOLType
    {
        // Needs further testing, might not work properly
        // and doesn't implement all NUMVAL functionality
        Span<byte> span = stackalloc byte[45];

        var toNum = new Decimal128(argument.Bytes);

        var length = toNum.AsSpan(span);

        return new Temporary(span.Slice(0, length));
    }

    public static void NUMVAL_C(Alphanumeric argument)
    {
        // TODO: Implement NUMVAL-C
    }

    public static void NUMVAL_F(Alphanumeric argument)
    {
        // TODO: Implement NUMVAL-F
    }

    public static Numeric ORD(ICOBOLType argument) //recieves an alphanumeric, alphabetic, or national
    {
        // TODO: return error if argument is not alphabetic, alphanumeric, or national
        // TODO: return error if argument is not a single character

        uint one = 1;
        uint value = argument.Bytes[0] + one; 
        //should this be signed?
        //there might be a more efficient conversion here, just wanted to play it safe

        Decimal128 DecValue = new(Encoding.UTF8.GetBytes(value.ToString()));

        return DecValue;
    }

    public static void ORD_MAX(Alphanumeric argument)
    {
        // TODO: Implement ORD-MAX
    }

    public static void ORD_MIN(Alphanumeric argument)
    {
        // TODO: Implement ORD-MIN
    }

    public static Temporary PI()
    {
        Span<byte> result = stackalloc byte[45];

        var length = Decimal128.Pi.AsSpan(result);

        return new Temporary(result.Slice(0, length));
    }

    public static void PRESENT_VALUE()
    {
        // TODO: Implement PRESENT-VALUE
    }

    public static Numeric RANDOM(Option<Numeric> argument)
    {
        Random random;
        Encoding encoding = Encoding.UTF8;

        if (!argument.Exists)
        {
            random = new();
        }
        else
        {
            var uargument = argument.Unwrap();

            var range = (Decimal128)uargument.Bytes[..Math.Min(9, uargument.Bytes.Length)];

            int int_arg = int.Parse(range.ToString()); // The range variable above makes sure that it fits inside of an Int32
            random = new(int_arg);//what about fractional arguments? Accoridng to the standard, those are not supported
        }

        double output = random.NextDouble();
        string s_output = output.ToString();
        Decimal128 d_output = new(encoding.GetBytes(s_output));//since the string type is UTF-16, would this work?
        return d_output;
    }

    public static void RANGE(Numeric? argument)
    {
        // TODO: Implement RANGE
        // This function depends on the functionality of MAX and MIN
    }

    public static Numeric REM(Numeric left, Numeric right)
    {
        // The COBOL standard suggested this calculation:
        Decimal128 subsidiaryQuotient = left / right;
        // if (subsidiaryQuotient.Bytes.IndexOf("."u8) > -1)
        //     subsidiaryQuotient = subsidiaryQuotient.Bytes[..^1];
        Decimal128 output = (Decimal128)left - (subsidiaryQuotient * (Decimal128)right);
        return output;
    }

    public static Temporary REVERSE<T>(T input) where T : ICOBOLType
    {
        var bytes = input.Bytes;

        var length = bytes.Length;

        Span<byte> reverse = stackalloc byte[length];

        var forwardIndex = 0;

        for (var index = length - 1; index >= 0; index--)
        {
            reverse[forwardIndex] = bytes[index];

            forwardIndex++;
        }

        return new Temporary(reverse);
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
        Decimal128 temporary = argument;
        if (temporary < 0)
            return new Numeric("-1"u8, 0, 2, 0, new byte[3]);
        if (temporary == 0)
            return new Numeric("0"u8, 0, 1, 0, new byte[1]);

        return new Numeric("1"u8, 0, 1, 0, new byte[1]);
    }

    public static Temporary SIN<T>(T argument) where T : ICOBOLType
    {
        Span<byte> result = stackalloc byte[45];

        var d128 = new Decimal128(argument.Bytes);

        var abs = Decimal128.Sin(d128);

        var length = abs.AsSpan(result);

        return new Temporary(result.Slice(0, length));
    }

    public static void SMALLEST_ALGEBRAIC()
    {
        // TODO: Implement SMALLEST-ALGEBRAIC
    }

    public static Temporary SQRT<T>(T argument) where T : ICOBOLType
    {
        Span<byte> result = stackalloc byte[45];

        var d128 = new Decimal128(argument.Bytes);

        var abs = Decimal128.Sqrt(d128);

        var length = abs.AsSpan(result);

        return new Temporary(result.Slice(0, length));
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
        Decimal128 sum = 0;
        foreach (Numeric dec in argument)
        {
            sum += (Decimal128)dec;
        }

        return sum;
    }

    public static Temporary TAN<T>(T argument) where T : ICOBOLType
    {
        Span<byte> result = stackalloc byte[45];

        var d128 = new Decimal128(argument.Bytes);

        var abs = Decimal128.Tan(d128);

        var length = abs.AsSpan(result);

        return new Temporary(result.Slice(0, length));
    }

    public static Numeric TEST_DATE_YYYYMMDD(Numeric argument)
    {
        Decimal128 input = new(argument.Bytes);
        Decimal128 ten_thousand = new("10000"u8);
        Decimal128 tweleve_ninety_nine = new("1299"u8);
        Decimal128 hundred = new("100"u8);
        Decimal128 year_lower_bound = new("1601000"u8);
        Decimal128 year_upper_bound = new("99999999"u8);

        Decimal128 month_check = input % ten_thousand;
        Decimal128 day_check_1 = input % hundred;
        Numeric day_check_2 = INTEGER_PART(day_check_1);
        int day_check_3 = int.Parse(day_check_2.Display);

        Decimal128 day_m_check_1 = (input % ten_thousand) / hundred;
        //TODO: Replace INTEGER_PART with proper INTEGER
        Numeric day_m_check_2 = INTEGER_PART(day_m_check_1);
        int day_m_check_3 = int.Parse(day_m_check_2.Display);

        Decimal128 day_y_check_1 = input / ten_thousand;
        Numeric day_y_check_2 = INTEGER_PART(day_y_check_1);
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
        Decimal128 input = argument;
        Decimal128 thousand = new("1000"u8);
        Decimal128 year_lower_bound = new("1601000"u8);
        Decimal128 year_upper_bound = new("9999999"u8);

        Decimal128 day_check_1 = input % thousand;
        Numeric day_check_2 = day_check_1;
        int day_check_3 = int.Parse(day_check_2.Display);

        Decimal128 y_check_1 = input / thousand;
        Numeric y_check_2 = INTEGER_PART(y_check_1);
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

    public static Numeric YEAR_TO_YYYY(Numeric yy, Option<Numeric> window, Option<Numeric> current)
    {
        //TODO: Implement runtime exceptions for invalid inputs
        if (!window.Exists) window = new Numeric("50"u8, false);
        
        if (!current.Exists)
        {
            //TODO: make sure NUMVAL works fully
            var slice = CURRENT_DATE();

            // TODO: Turn this method generic
            // current = NUMVAL(slice);
        }
        
        var uwindow = window.Unwrap();

        Decimal128 yy_num = new(Encoding.UTF8.GetBytes(yy.Display));
        Decimal128 window_num = new(Encoding.UTF8.GetBytes(uwindow.Display));
        Decimal128 current_num = new(Encoding.UTF8.GetBytes(uwindow.Display));

        Decimal128 max_year = window_num + current_num;
        Decimal128 hundred = new(Encoding.UTF8.GetBytes("100"));
        Decimal128 one = new(Encoding.UTF8.GetBytes("1"));
        //TODO: replace with true INTEGER function
        if ((max_year % hundred) >= yy_num)
        {
            Numeric part = INTEGER_PART(max_year / hundred);
            Decimal128 coefficient = new(Encoding.UTF8.GetBytes(part.Display));
            Decimal128 result = (yy_num + hundred * coefficient);
            return result;
        }
        else
        {
            Numeric part = INTEGER_PART(max_year / hundred);
            Decimal128 coefficient = new(Encoding.UTF8.GetBytes(part.Display));
            coefficient -= one;
            Decimal128 result = (yy_num + hundred * coefficient);
            return result;
        }
    }
}
