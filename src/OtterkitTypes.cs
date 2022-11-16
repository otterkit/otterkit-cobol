using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using System.Text;

namespace OtterkitLibrary;

interface IDataItem<TItemType>
{
    bool isNumeric();
    bool isAlphanumeric();
    bool isAlphabetic();
    bool isNational();
    bool isBoolean();
    string Formatted();
}

public abstract class DataItem<TItemType>
{
    public int length;
}

public class Numeric
{
    public Decimal128 dataItem;
    public int length;
    public int fractionalLength;
    public bool isSigned = false;

    public Numeric(string value, int length, int fractionalLength, bool signed)
    {
        this.dataItem = new Decimal128(value);
        this.length = length;
        this.fractionalLength = fractionalLength;
        this.isSigned = signed;
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
        string abs = Decimal128.Abs(dataItem).Value;
        int indexOfDecimal = abs.IndexOf('.');

        if (indexOfDecimal < 0 && fractionalLength != 0)
            abs += ".0";

        if (indexOfDecimal >= 0 && fractionalLength == 0)
        {
            dataItem.Value = dataItem.Value.Substring(0, indexOfDecimal);
        }

        if (fractionalLength != 0)
        {
            int startIndex = (indexOfDecimal - length) < 0 ? 0 : indexOfDecimal - length;
            int endIndex = Math.Min(abs.Length, indexOfDecimal + fractionalLength + 1 - startIndex);
            int offset = length - indexOfDecimal < 0 ? 0 : length - indexOfDecimal;

            return String.Create(length + fractionalLength + 1, abs, (span, value) =>
            {
                ReadOnlySpan<char> temporary = value.AsSpan(startIndex, endIndex);
                span.Fill('0');
                temporary.CopyTo(span.Slice(offset));
            });
        }

        string padInt = abs.PadLeft(length, '0');
        // If Numeric item doesn't have a fractional value, pad missing zeros and remove overflow
        return padInt.Substring(padInt.Length - length);
    }

    public string DisplayValue
    {
        get
        {
            if (dataItem < 0 && isSigned)
            {
                return "-" + Formatted();
            }

            if (dataItem >= 0 && isSigned)
            {
                return "+" + Formatted();
            }

            return Formatted();
        }
    }
}

public sealed class Alphanumeric
{
    public Memory<byte> Bytes { get; init; }
    public int Length { get; init; }
    Encoding encoding = Encoding.UTF8;

    public Alphanumeric(ReadOnlySpan<char> value, int length, Memory<byte> memory)
    {
        this.Bytes = memory;
        this.Length = length;
        Bytes.Span.Fill(32);

        int byteDifference = (encoding.GetByteCount(value) - value.Length) * 2;
        int byteLength;

        if (byteDifference == 0)
        {
            byteLength = Length < value.Length
            ? Length
            : value.Length;
            encoding.GetBytes(value.Slice(0, byteLength), Bytes.Span);
        }

        if (byteDifference != 0)
        {
            byteLength = Length < value.Length - (byteDifference + 2)
            ? Length
            : value.Length - (byteDifference + 2);
            encoding.GetBytes(value.Slice(0, byteLength), Bytes.Span);
        }
    }

    public ReadOnlySpan<char> CharValue
    {
        get
        {
            return MemoryMarshal.Cast<byte, char>(Bytes.Span);
        }
        set
        {
            Bytes.Span.Fill(32);

            int byteDifference = (encoding.GetByteCount(value) - value.Length) * 2;
            int byteLength;

            if (byteDifference == 0)
            {
                byteLength = Length < value.Length
                ? Length
                : value.Length;
                encoding.GetBytes(value.Slice(0, byteLength), Bytes.Span);
            }

            if (byteDifference != 0)
            {
                byteLength = Length < value.Length - (byteDifference + 2)
                ? Length
                : value.Length - (byteDifference + 2);
                encoding.GetBytes(value.Slice(0, byteLength), Bytes.Span);
            }
        }
    }

    public ReadOnlySpan<byte> ByteValue
    {
        get
        {
            return Bytes.Span;
        }
        set
        {
            Bytes.Span.Fill(32);
            
            int limitLength = Length < value.Length
            ? Length
            : value.Length;

            value.Slice(0, limitLength).CopyTo(Bytes.Span);
        }
    }

    public string DisplayValue
    {
        get
        {
            return encoding.GetString(Bytes.Span);
        }
    }
}

public class Alphabetic
{
    public string dataItem;
    public int length;
    public Alphabetic(string value, int length)
    {
        if (value.Any(char.IsDigit))
        {
            throw new ArgumentException("Alphabetic type cannot contain numeric values", value);
        }
        this.length = length;
        this.dataItem = value == string.Empty ? " " : value;
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
        return String.Create(length, dataItem, (span, value) =>
        {
            int MaxSize = dataItem.Length < length ? dataItem.Length : length;
            value.AsSpan(0, MaxSize).CopyTo(span);
            span[MaxSize..].Fill(' ');
        });
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
            dataItem = value == string.Empty ? " " : value;
        }
    }

}

public class National
{
    public string dataItem;
    public int length;
    public National(string value, int length)
    {
        this.length = length;
        this.dataItem = value == string.Empty ? " " : value;
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
        return String.Create(length, dataItem, (span, value) =>
        {
            int MaxSize = dataItem.Length < length ? dataItem.Length : length;
            value.AsSpan(0, MaxSize).CopyTo(span);
            span[MaxSize..].Fill(' ');
        });
    }

    public string Value
    {
        get
        {
            return Formatted();
        }
        set
        {
            dataItem = value == string.Empty ? " " : value;
        }
    }

}

public class Boolean
{
    public string dataItem;
    public int length;
    public Boolean(string value, int length)
    {
        if (!Regex.IsMatch(value, @"^([01]+)$", RegexOptions.Compiled | RegexOptions.NonBacktracking))
        {
            throw new ArgumentException("Boolean type can only contain 1s and 0s", value);
        }
        this.length = length;
        this.dataItem = value == string.Empty ? "0" : value;
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
        return String.Create(length, dataItem, (span, value) =>
        {
            int MaxSize = dataItem.Length < length ? dataItem.Length : length;
            value.AsSpan(0, MaxSize).CopyTo(span);
            span[MaxSize..].Fill('0');
        });
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
            dataItem = value == string.Empty ? "0" : value;
        }
    }

}