using System.Numerics;

namespace OtterkitLibrary.Numerics;

public static class DecimalMath
{
    public static BigDecimal Abs(BigDecimal argument)
    {
        BigInteger mantissa = argument.Mantissa;
        if (mantissa <= 0)
        {
            mantissa = -mantissa;
            return argument;
        }
        return argument;
    }
}

// public struct OtterkitDPD: IComparable, IComparable<OtterkitDPD>
// TODO: Implement Densely Packed Decimal.

/// <summary>
/// Arbitrary Precision Decimal, used by Otterkit COBOL for fixed-point decimal arithmetic.
/// <para>All operations are exact, except for division. Division never determines more digits than the given precision.</para>
/// <para>This is a slightly modified version of Jan Christoph Bernack's public domain BigDecimal implementation.</para>
/// <para>Original Author: Jan Christoph Bernack (contact: jc.bernack at gmail.com)</para>
/// <para>Source: https://gist.github.com/JcBernack/0b4eef59ca97ee931a2f45542b9ff06d</para>
/// <para>Based on: https://stackoverflow.com/a/4524254</para>
/// <para>License: CC0 Public Domain</para>
/// </summary>
public struct BigDecimal: IComparable, IComparable<BigDecimal>
{
    /// <summary>
    /// Specifies whether the significant digits should be Rounded to the given precision after each operation.
    /// </summary>
    public static bool AlwaysRound = true;

    /// <summary>
    /// Sets the maximum precision of division operations.
    /// If AlwaysRound is set to true all operations are affected.
    /// </summary>
    public static int Precision = 34;

    public BigInteger Mantissa { get; set; }
    public int Exponent { get; set; }

    public BigDecimal(BigInteger mantissa, int exponent)
    {
        this.Mantissa = mantissa;
        this.Exponent = exponent;
        Normalize();
        if (AlwaysRound)
        {
            Round();
        }
    }

    /// <summary>
    /// Removes trailing zeros on the mantissa
    /// </summary>
    public void Normalize()
    {
        if (Exponent == 0) return;
        if (Mantissa.IsZero)
        {
            Exponent = 0;
        }
        else
        {
            BigInteger remainder = 0;
            while (remainder == 0)
            {
                var shortened = BigInteger.DivRem(Mantissa, 10, out remainder);
                if (remainder == 0)
                {
                    Mantissa = shortened;
                    Exponent++;
                }
            }
        }
    }

    /// <summary>
    /// Round the number to the given precision by removing the least significant digits.
    /// </summary>
    /// <returns>The Rounded number</returns>
    public BigDecimal Round(int precision)
    {
        // copy this instance (remember it's a struct)
        var shortened = this;
        // save some time because the number of digits is not needed to remove trailing zeros
        shortened.Normalize();
        // remove the least significant digits, as long as the number of digits is higher than the given Precision
        int Digits = NumberOfDigits(shortened.Mantissa);
        int DigitsToRemove = Math.Max(Digits - precision, 0);
        shortened.Mantissa /= BigInteger.Pow(10, DigitsToRemove);
        shortened.Exponent += DigitsToRemove;
        // normalize again to make sure there are no trailing zeros left
        shortened.Normalize();
        return shortened;
    }

    public BigDecimal Round()
    {
        return Round(Precision);
    }

    public BigDecimal Truncate()
    {
        return Round(BigDecimal.NumberOfDigits(Mantissa) + Exponent);
    }

    public static int NumberOfDigits(BigInteger value)
    {
        if (value.IsZero)
        {
            return 1;
        }
        double NumberOfDigits = BigInteger.Log10(value * value.Sign);
        if(NumberOfDigits % 1 == 0)
        {
            return (int)NumberOfDigits + 1;
        }
        return (int)Math.Ceiling(NumberOfDigits);
    }

    #region Conversions

    public static implicit operator BigDecimal(int value)
    {
        return new BigDecimal(value, 0);
    }

    public static implicit operator BigDecimal(string value)
    {
        bool isDecimal = value.Contains(".");
        if (isDecimal)
        {
            int exponent = value.Length - (value.IndexOf(".") + 1);
            BigInteger mantissa = BigInteger.Parse(value.Replace(".", ""));
            return new BigDecimal(mantissa, -exponent);
        }
        return new BigDecimal(BigInteger.Parse(value), 0);
    }

    public static implicit operator BigDecimal(double value)
    {
        var mantissa = (BigInteger) value;
        var exponent = 0;
        double scaleFactor = 1;
        while (Math.Abs(value * scaleFactor - (double)mantissa) > 0)
        {
            exponent -= 1;
            scaleFactor *= 10;
            mantissa = (BigInteger)(value * scaleFactor);
        }
        return new BigDecimal(mantissa, exponent);
    }

    public static implicit operator BigDecimal(decimal value)
    {
        var mantissa = (BigInteger)value;
        var exponent = 0;
        decimal scaleFactor = 1;
        while ((decimal)mantissa != value * scaleFactor)
        {
            exponent -= 1;
            scaleFactor *= 10;
            mantissa = (BigInteger)(value * scaleFactor);
        }
        return new BigDecimal(mantissa, exponent);
    }

    public static explicit operator double(BigDecimal value)
    {
        return (double)value.Mantissa * Math.Pow(10, value.Exponent);
    }

    public static explicit operator float(BigDecimal value)
    {
        return Convert.ToSingle((double)value);
    }

    public static explicit operator decimal(BigDecimal value)
    {
        return (decimal)value.Mantissa * (decimal)Math.Pow(10, value.Exponent);
    }

    public static explicit operator int(BigDecimal value)
    {
        return (int)(value.Mantissa * BigInteger.Pow(10, value.Exponent));
    }

    public static explicit operator uint(BigDecimal value)
    {
        return (uint)(value.Mantissa * BigInteger.Pow(10, value.Exponent));
    }

    public static explicit operator BigInteger(BigDecimal value)
    {
        BigDecimal truncated = value.Truncate();
        return truncated.Mantissa * BigInteger.Pow(10, truncated.Exponent);
    }

    #endregion

    #region Operators

    public static BigDecimal operator +(BigDecimal value)
    {
        return value;
    }

    public static BigDecimal operator -(BigDecimal value)
    {
        value.Mantissa *= -1;
        return value;
    }

    public static BigDecimal operator ++(BigDecimal value)
    {
        return value + 1;
    }

    public static BigDecimal operator --(BigDecimal value)
    {
        return value - 1;
    }

    public static BigDecimal operator +(BigDecimal left, BigDecimal right)
    {
        return Add(left, right);
    }

    public static BigDecimal operator -(BigDecimal left, BigDecimal right)
    {
        return Add(left, -right);
    }

    private static BigDecimal Add(BigDecimal left, BigDecimal right)
    {
        return left.Exponent > right.Exponent
            ? new BigDecimal(AlignExponent(left, right) + right.Mantissa, right.Exponent)
            : new BigDecimal(AlignExponent(right, left) + left.Mantissa, left.Exponent);
    }

    public static BigDecimal operator *(BigDecimal left, BigDecimal right)
    {
        return new BigDecimal(left.Mantissa * right.Mantissa, left.Exponent + right.Exponent);
    }
    
    public static BigDecimal operator ^(BigDecimal left, BigDecimal right)
    {
        return Pow((double)left, (double)right);
    }

    public static BigDecimal operator /(BigDecimal dividend, BigDecimal divisor)
    {
        var exponentChange = Precision - (NumberOfDigits(dividend.Mantissa) - NumberOfDigits(divisor.Mantissa));
        if (exponentChange < 0)
        {
            exponentChange = 0;
        }
        dividend.Mantissa *= BigInteger.Pow(10, exponentChange);
        return new BigDecimal(dividend.Mantissa / divisor.Mantissa, dividend.Exponent - divisor.Exponent - exponentChange);
    }
        
    public static BigDecimal operator %(BigDecimal left, BigDecimal right)
    {
        return left - right * (left / right).Truncate();
    }

    public static bool operator ==(BigDecimal left, BigDecimal right)
    {
        return left.Exponent == right.Exponent && left.Mantissa == right.Mantissa;
    }

    public static bool operator !=(BigDecimal left, BigDecimal right)
    {
        return left.Exponent != right.Exponent || left.Mantissa != right.Mantissa;
    }

    public static bool operator <(BigDecimal left, BigDecimal right)
    {
        return left.Exponent > right.Exponent ? AlignExponent(left, right) < right.Mantissa : left.Mantissa < AlignExponent(right, left);
    }

    public static bool operator >(BigDecimal left, BigDecimal right)
    {
        return left.Exponent > right.Exponent ? AlignExponent(left, right) > right.Mantissa : left.Mantissa > AlignExponent(right, left);
    }

    public static bool operator <=(BigDecimal left, BigDecimal right)
    {
        return left.Exponent > right.Exponent ? AlignExponent(left, right) <= right.Mantissa : left.Mantissa <= AlignExponent(right, left);
    }

    public static bool operator >=(BigDecimal left, BigDecimal right)
    {
        return left.Exponent > right.Exponent ? AlignExponent(left, right) >= right.Mantissa : left.Mantissa >= AlignExponent(right, left);
    }

    /// <summary>
    /// Returns the mantissa of value, aligned to the exponent of reference.
    /// Assumes the exponent of value is larger than of reference.
    /// </summary>
    private static BigInteger AlignExponent(BigDecimal value, BigDecimal reference)
    {
        return value.Mantissa * BigInteger.Pow(10, value.Exponent - reference.Exponent);
    }

    #endregion

    #region Additional mathematical functions

    public static BigDecimal Exp(double exponent)
    {
        var tmp = (BigDecimal)1;
        while (Math.Abs(exponent) > 100)
        {
            var diff = exponent > 0 ? 100 : -100;
            tmp *= Math.Exp(diff);
            exponent -= diff;
        }
        return tmp * Math.Exp(exponent);
    }

    public static BigDecimal Pow(double basis, double exponent)
    {
        var tmp = (BigDecimal)1;
        while (Math.Abs(exponent) > 100)
        {
            var diff = exponent > 0 ? 100 : -100;
            tmp *= Math.Pow(basis, diff);
            exponent -= diff;
        }
        return tmp * Math.Pow(basis, exponent);
    }

    #endregion

    public override string ToString()
    {
        return string.Concat(Mantissa.ToString(), "E", Exponent);
    }

    public string ToString(string format)
    {
        if (Exponent == 0) return Mantissa.ToString();
        int length = Mantissa.ToString().Length;
        return format == "decimal"
            ? Mantissa.ToString().Insert(length -(-Exponent), ".")
            : ToString();
    }

    public bool Equals(BigDecimal other)
    {
        return other.Mantissa.Equals(Mantissa) && other.Exponent == Exponent;
    }

    public override bool Equals(object? obj)
    {
        if (ReferenceEquals(null, obj))
        {
            return false;
        }
        return obj is BigDecimal && Equals((BigDecimal) obj);
    }

    public override int GetHashCode()
    {
        unchecked
        {
            return (Mantissa.GetHashCode()*397) ^ Exponent;
        }
    }

    public int CompareTo(object? obj)
    {
        if (ReferenceEquals(obj, null) || !(obj is BigDecimal))
        {
            throw new ArgumentException();
        }
        return CompareTo((BigDecimal) obj);
    }

    public int CompareTo(BigDecimal other)
    {
        return this < other ? -1 : (this > other ? 1 : 0);
    }
}