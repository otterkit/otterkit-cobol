using System.Text.RegularExpressions;

namespace OtterkitLibrary;

interface IDataItem<Type>
{
    bool isNumeric();
    bool isAlphanumeric();
    bool isAlphabetic();
    bool isNational();
    bool isBoolean();
    string Formatted();
}

public abstract class DataItem<Type>
{
    public Type? dataItem;
    public int length;
}

public class Numeric : DataItem<Decimal128>, IDataItem<Decimal128>
{
    public int fractionalLength;
    public bool isSigned = false;
    private bool isNegative = false;

    public Numeric(Decimal128 value, int length, int fractionalLength, bool signed)
    {
        this.dataItem = value;
        this.length = length;
        this.fractionalLength = fractionalLength;
        this.isSigned = signed;
        if (value < 0 && isSigned)
        {
            isNegative = true;
        }
    }

    public bool isNumeric()
    {
        // return Regex.IsMatch(dataItem.Value, @"^([+-]?)(\.\d|\d\.|\d)(\d+)*$", RegexOptions.Compiled | RegexOptions.NonBacktracking);
        return true;
    }

    public bool isAlphanumeric()
    {
        return true;
    }

    public bool isAlphabetic()
    {
        return false;
    }

    public bool isNational()
    {
        return true;
    }

    public bool isBoolean()
    {
        return Regex.IsMatch(dataItem.Value, @"^([01]+)$", RegexOptions.Compiled | RegexOptions.NonBacktracking);
    }

    public string Formatted()
    {
        if (fractionalLength != 0)
        {
            // Split at decimal point if Numeric item has a fractional value
            string[] splitDecimal = dataItem.Value.Split(".");

            // Fill both sides missing spaces with zeros 
            string padLeft = splitDecimal[0].PadLeft(length, '0');
            string padRight = splitDecimal[1].PadRight(fractionalLength, '0');

            // Remove overflow from both sides and contatenate with the decimal point in between
            return new String(padLeft.Substring(padLeft.Length - length) + "." + padRight.Substring(0, fractionalLength));
        }
        
        // If Numeric item doesn't have a fractional value, pad missing zeros and remove overflow
        return dataItem.Value.PadLeft(length, '0').Substring(dataItem.Value.Length - length);
    }

    public Decimal128 Value
    {
        get
        {
            if (isNegative && isSigned)
            {
                return String.Format("-{0}", Formatted());
            }

            if (!isNegative && isSigned)
            {
                return String.Format("+{0}", Formatted());
            }

            return Formatted();
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

            dataItem = value;
        }
    }
}

public class Alphanumeric : DataItem<String>, IDataItem<String>
{
    public Alphanumeric(string value, int length)
    {
        this.length = length;
        this.dataItem = value;
    }

    public bool isNumeric()
    {
        return false;
    }

    public bool isAlphanumeric()
    {
        return true;
    }

    public bool isAlphabetic()
    {
        return !Formatted().Any(char.IsDigit);
    }

    public bool isNational()
    {
        return true;
    }

    public bool isBoolean()
    {
        return false;
    }

    public string Formatted()
    {
        return String.IsNullOrEmpty(dataItem) 
            ? " ".PadRight(length).Substring(0, length)
            : dataItem.PadRight(length).Substring(0, length);
    }

    public string Value
    {
        get
        {
            return Formatted();
        }
        set
        {
            dataItem = String.IsNullOrEmpty(value) ? " " : value;
        }
    }

}

public class Alphabetic : DataItem<String>, IDataItem<String>
{
    public Alphabetic(string value, int length)
    {
        if (value.Any(char.IsDigit))
        {
            throw new ArgumentException("Alphabetic type cannot contain numeric values", value);
        }
        this.length = length;
        this.dataItem = value;
    }

    public bool isNumeric()
    {
        return false;
    }

    public bool isAlphanumeric()
    {
        return false;
    }

    public bool isAlphabetic()
    {
        return true;
    }

    public bool isNational()
    {
        return false;
    }

    public bool isBoolean()
    {
        return false;
    }

    public string Formatted()
    {
        return String.IsNullOrEmpty(dataItem) 
            ? " ".PadRight(length).Substring(0, length)
            : dataItem.PadRight(length).Substring(0, length);
    }

    public string Value
    {
        get
        {
            return Formatted();
        }
        set
        {
            if (value.Any(char.IsDigit))
            {
                throw new ArgumentException("Alphabetic type cannot contain numeric values", value);
            }
            dataItem = String.IsNullOrEmpty(value) ? " " : value;
        }
    }

}

public class National : DataItem<String>, IDataItem<String>
{
    public National(string value, int length)
    {
        this.length = length;
        this.dataItem = value;
    }

    public bool isNumeric()
    {
        return false;
    }

    public bool isAlphanumeric()
    {
        return true;
    }

    public bool isAlphabetic()
    {
        return !Formatted().Any(char.IsDigit);
    }

    public bool isNational()
    {
        return true;
    }

    public bool isBoolean()
    {
        return false;
    }

    public string Formatted()
    {
        return String.IsNullOrEmpty(dataItem) 
            ? " ".PadRight(length).Substring(0, length)
            : dataItem.PadRight(length).Substring(0, length);
    }

    public string Value
    {
        get
        {
            return Formatted();
        }
        set
        {
            dataItem = String.IsNullOrEmpty(value) ? " " : value;
        }
    }

}

public class Boolean : DataItem<String>, IDataItem<String>
{
    public Boolean(string value, int length)
    {
        if (!Regex.IsMatch(value, @"^([01]+)$", RegexOptions.Compiled | RegexOptions.NonBacktracking))
        {
            throw new ArgumentException("Boolean type can only contain 1s and 0s", value);
        }
        this.length = length;
        this.dataItem = value;
    }

    public bool isNumeric()
    {
        return false;
    }

    public bool isAlphanumeric()
    {
        return true;
    }

    public bool isAlphabetic()
    {
        return false;
    }

    public bool isNational()
    {
        return true;
    }

    public bool isBoolean()
    {
        return true;
    }

    public string Formatted()
    {
        return String.IsNullOrEmpty(dataItem) 
            ? "0".PadRight(length, '0').Substring(0, length)
            : dataItem.PadRight(length, '0').Substring(0, length);
    }

    public string Value
    {
        get
        {
            return Formatted();
        }
        set
        {
            if (!Regex.IsMatch(value, @"^([01]+)$", RegexOptions.Compiled | RegexOptions.NonBacktracking))
            {
                throw new ArgumentException("Boolean type can only contain 1s and 0s", value);
            }
            dataItem = String.IsNullOrEmpty(value) ? "0" : value;
        }
    }

}