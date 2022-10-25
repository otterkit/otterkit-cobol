namespace OtterkitLibrary;

public struct Numeric
{
    private Decimal128 internalNumber;
    public int integerLength;
    public int fractionalLength;
    public bool isSigned = false;
    private bool isNegative = false;

    public Numeric(Decimal128 number, int integerLength, int fractionalLength, bool signed)
    {
        this.internalNumber = number;
        this.integerLength = integerLength;
        this.fractionalLength = fractionalLength;
        this.isSigned = signed;
        if (number < 0 && isSigned)
        {
            isNegative = true;
        }
    }

    private string Formatter(Decimal128 number)
    {
        if (fractionalLength != 0)
        {
            // TODO: This looks like spaghetti, refactor later
            string[] numericString = number.ToString().Split(".");
            string padInteger = numericString[0].PadLeft(integerLength, '0');
            string padFraction = numericString[1].PadRight(fractionalLength, '0');
            padInteger = padInteger.Substring(padInteger.Length - integerLength, padInteger.Length);
            padFraction = padFraction.Substring(0, fractionalLength);
            return string.Concat(padInteger, ".", padFraction);

        }
        // TODO: This one as well, refactor later
        // The decimal point floats, might not work correctly
        return number.ToString().PadLeft(integerLength, '0').Substring(number.ToString().Length - integerLength, number.ToString().Length);
    }

    public Decimal128 Value
    {
        get
        {
            return (isNegative && isSigned)
                    ? -internalNumber
                    : internalNumber;
        }
        set
        {
            if (value < 0 && isSigned)
            {
                isNegative = true;
            }

            if (value >= 0 && isSigned)
            {
                isNegative = false;
            }

            internalNumber = value;
        }
    }

    public string DisplayValue
    {
        get
        {
            if (isNegative && isSigned)
            {
                return String.Format("-{0}", Formatter(internalNumber));
            }

            if (!isNegative && isSigned)
            {
                return String.Format("+{0}", Formatter(internalNumber));
            }

            return Formatter(internalNumber);
        }
    }
}

public struct Alphanumeric
{
    private string internalString;
    public int stringLength;

    public Alphanumeric(string str, int length)
    {
        this.stringLength = length;
        this.internalString = str.PadRight(stringLength).Substring(0, stringLength);
    }

    public string Value
    {
        get
        {
            return internalString;
        }
        set
        {
            internalString = value.PadRight(stringLength).Substring(0, stringLength);
        }
    }

}

public struct Alphabetic
{
    private string internalString;
    public int stringLength;

    public Alphabetic(string str, int length)
    {
        if (str.Any(char.IsDigit))
        {
            throw new ArgumentException("Alphabetic type cannot contain numeric values", str);
        }
        this.stringLength = length;
        this.internalString = str.PadRight(stringLength).Substring(0, stringLength);
    }

    public string Value
    {
        get
        {
            return internalString;
        }
        set
        {
            if (value.Any(char.IsDigit))
            {
                throw new ArgumentException("Alphabetic type cannot contain numeric values", value);
            }
            internalString = value.PadRight(stringLength).Substring(0, stringLength);
        }
    }

}
