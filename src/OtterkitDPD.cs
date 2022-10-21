using System.Numerics;
namespace OtterkitLibrary;

public static class DPDConstants
{
    public static readonly DPD120 PI = (Int128)3141592653589793238;
    public static readonly DPD120 E = (Int128)2718281828459045235;
}

public static class DPDEncoding
{
    public static int Encode(int integer)
    {
        if (integer < 0 || integer > 999)
        {
            throw new ArgumentOutOfRangeException("Densely Packed Decimal encoding can only accept values between 0 and 999", integer.ToString());
        }
        int DPD = DPDLUT.DPDValues[integer];
        return DPD;
    }

    public static int Decode(int DPD)
    {
        if (DPDLUT.DPDValues.Contains(DPD) == false)
        {
            throw new ArgumentException("Argument is not a valid Densely Packed Decimal value", DPD.ToString());
        }
        int integer = Array.IndexOf(DPDLUT.DPDValues, DPD);
        return integer;
    }
}

/// <summary>
/// Otterkit DPD120 Struct
/// <para>120-bit Fixed-Point Densely Packed Decimal number, custom built for the Otterkit compiler.</para>
/// <para>This fixed-point number is stored internally as 12 units of 10-bit DPD declets</para>
/// <para>Max Value: +999999999.999999999</para>
/// <para>Min Value: -999999999.999999999</para>
/// <para>Author: Gabriel Duarte Gonçalves (Contact: KTSnowy@outlook.com)</para>
/// <para>License: Apache 2.0</para>
/// </summary>
public struct DPD120
{
    // Sign bit, 0 = positive, 1 = negative
    private int sign = 0;

    // Series of DPD encoded declets, initialized with zeros.
    // Equivalent to: 000 000 000 000 000 000 . 000 000 000 000 000 000
    private int[] Declets =
    {
        0000000000,
        0000000000,
        0000000000,
        0000000000,
        0000000000,
        0000000000,
        // . fixed decimal point
        0000000000,
        0000000000,
        0000000000,
        0000000000,
        0000000000,
        0000000000
    };

    public DPD120(int sign, int[] declets)
    {
        this.sign = sign;
        this.Declets = declets;
    }

    public DPD120(int sign, int integer)
    {
        this.sign = sign;
        ToDeclets(integer);
    }

    public DPD120(int sign, long integer)
    {
        this.sign = sign;
        ToDeclets(integer);
    }

    public DPD120(int sign, Int128 integer)
    {
        this.sign = sign;
        ToDeclets(integer);
    }

    public DPD120(int sign, double number)
    {
        this.sign = sign;
        ToDeclets(number);
    }

    public static implicit operator DPD120(int value)
    {
        if (value < 0)
        {
            return new DPD120(1, value);
        }
        return new DPD120(0, value);
    }

    public static implicit operator DPD120(long value)
    {
        if (value < 0)
        {
            return new DPD120(1, value);
        }
        return new DPD120(0, value);
    }

    public static implicit operator DPD120(Int128 value)
    {
        if (value < 0)
        {
            return new DPD120(1, value);
        }
        return new DPD120(0, value);
    }

    public static implicit operator DPD120(double value)
    {
        if (value < 0)
        {
            return new DPD120(1, value);
        }
        return new DPD120(0, value);
    }

    private static DPD120 Add(DPD120 left, DPD120 right)
    {
        Int128 leftInteger = Int128.Parse(left.ToInt128String());
        Int128 rightInteger = Int128.Parse(left.ToInt128String());
        Int128 sum = leftInteger + rightInteger;

        return new DPD120(0, sum);
    }

    private static DPD120 Subtract(DPD120 left, DPD120 right)
    {
        Int128 leftInteger = Int128.Parse(left.ToInt128String());
        Int128 rightInteger = Int128.Parse(left.ToInt128String());
        Int128 difference = leftInteger - rightInteger;

        return new DPD120(0, difference);
    }

    private static DPD120 Multiply(DPD120 left, DPD120 right)
    {
        Int128 leftNum = Int128.Parse(left.ToInt128String());
        Int128 rightNum = Int128.Parse(right.ToInt128String());
        leftNum = leftNum / 1000000000000000000;
        Int128 product = (leftNum * rightNum);

        return new DPD120(0, product);
    }

    private static DPD120 Divide(DPD120 left, DPD120 right)
    {
        Int128 leftNum = Int128.Parse(left.ToInt128String());
        Int128 rightNum = Int128.Parse(right.ToInt128String());
        leftNum = leftNum * 1000000000000000000;
        Int128 quotient = (leftNum / rightNum);

        return new DPD120(0, quotient);
    }

    public static DPD120 Pow(double basis, double exponent)
    {
        return Math.Pow(basis, exponent);
    }

    public static DPD120 Exp(double exponent)
    {
        return Math.Exp(exponent);
    }

    public static DPD120 Log(double logarithm)
    {
        return Math.Log(logarithm);
    }

    public static DPD120 Log2(double logarithm2)
    {
        return Math.Log2(logarithm2);
    }

    public static DPD120 Log10(double logarithm10)
    {
        return Math.Log10(logarithm10);
    }

    public static DPD120 Acos(double arccosine)
    {
        if (arccosine >= 1 || arccosine <= -1) return 0; 
        return Math.Acos(arccosine);
    }

    public static DPD120 Acosh(double acosh)
    {
        return Math.Acosh(acosh);
    }

    public static DPD120 Cosh(double cosh)
    {
        return Math.Cosh(cosh);
    }

    public static DPD120 Cos(double cosine)
    {
        return Math.Cos(cosine);
    }

    public static DPD120 Asin(double arcsine)
    {
        if (arcsine >= 1 || arcsine <= -1) return 0; 
        return Math.Asin(arcsine);
    }

    public static DPD120 Asinh(double asinh)
    {
        return Math.Asinh(asinh);
    }

    public static DPD120 Sinh(double sinh)
    {
        return Math.Sinh(sinh);
    }

    public static DPD120 Sin(double sin)
    {
        return Math.Sin(sin);
    }

    public static DPD120 Atan(double arctangent)
    {
        if (arctangent >= 1 || arctangent <= -1) return 0; 
        return Math.Atan(arctangent);
    }

    public static DPD120 Atan2(double quotient1, double quotient2)
    {
        return Math.Atan2(quotient1, quotient2);
    }

    public static DPD120 Atanh(double atanh)
    {
        return Math.Atanh(atanh);
    }

    public static DPD120 Tanh(double tanh)
    {
        return Math.Tanh(tanh);
    }

    public static DPD120 Tan(double tan)
    {
        return Math.Tan(tan);
    }

    public static DPD120 Sqrt(double sqrt)
    {
        return Math.Sqrt(sqrt);
    }

    public static DPD120 operator +(DPD120 left, DPD120 right)
    {
        return Add(left, right);
    }

    public static DPD120 operator -(DPD120 left, DPD120 right)
    {
        return Subtract(left, right);
    }

    public static DPD120 operator *(DPD120 left, DPD120 right)
    {
        return Multiply(left, right);
    }

    public static DPD120 operator /(DPD120 left, DPD120 right)
    {
        return Divide(left, right);
    }

    public static DPD120 operator %(DPD120 left, DPD120 right)
    {
        return left - right * (left / right);
    }

    public static bool operator ==(DPD120 left, DPD120 right)
    {
        for (int index = 11; index > 0; index--)
        {
            if (left.Declets[index] != right.Declets[index])
                return false;
        }
        return left.sign == right.sign;
    }

    public static bool operator !=(DPD120 left, DPD120 right)
    {
        for (int index = 11; index > 0; index--)
        {
            if (left.Declets[index] == right.Declets[index])
                return false;
        }
        return left.sign != right.sign;
    }

    public bool Equals(DPD120 other)
    {
        return other == this;
    }

    public override bool Equals(object? o)
    {
        if (ReferenceEquals(null, o))
        {
            return false;
        }
        return o is DPD120 && Equals((DPD120)o);
    }

    public override int GetHashCode()
    {
        return Declets.GetHashCode() ^ sign;
    }

    // Int32 to 10-bit DPD values
    private void ToDeclets(int number)
    {
        if (int.IsNegative(number))
        {
            sign = 1;
            number = -number;
        }
        
        int overflow = 0;
        for (int index = 5; index > 0; index--)
        {
            Declets[index] = DPDEncoding.Encode(number % 1000);
            number /= 1000;
            overflow = number;
            if (overflow == 0)
            {
                break;
            }
        }
    }

    // Int64 to 10-bit DPD values
    private void ToDeclets(long number)
    {
        if (long.IsNegative(number))
        {
            sign = 1;
            number = -number;
        }

        long overflow = 0;
        for (int index = 5; index > 0; index--)
        {
            int declet = (int)(number % 1000);
            Declets[index] = DPDEncoding.Encode(declet);
            number /= 1000;
            overflow = number;
            if (overflow == 0)
            {
                break;
            }
        }
    }

    // Int128 to 10-bit DPD values
    private void ToDeclets(Int128 number)
    {
        Int128 overflow = 0;
        if (Int128.IsNegative(number))
        {
            sign = 1;
            number = -number;
        }

        for (int index = 11; index > 0; index--)
        {
            int declet = (int)(number % 1000);
            Declets[index] = DPDEncoding.Encode(declet);
            number /= 1000;
            overflow = number;
            if (overflow == 0)
            {
                break;
            }
        }
    }

    // Float64 to 10-bit DPD values
    private void ToDeclets(double number)
    {
        if (double.IsNegative(number))
        {
            sign = 1;
            number = -number;
        }

        string doubleToString = number.ToString();
        if (!double.IsInteger(number))
        {
            // If double has decimal digits then:
            string[] split = doubleToString.Split('.');
            split[1] = split[1].PadRight(18, '0');
            doubleToString = String.Concat(split[0], split[1]);
        }

        if (double.IsInteger(number))
        {
            // If double doesn't have any decimal digits:
            doubleToString = doubleToString.PadRight(19, '0');
        }

        Int128 parseDouble = Int128.Parse(doubleToString);
        Int128 overflow = 0;
        for (int index = 11; index > 0; index--)
        {
            int declet = (int)(parseDouble % 1000);
            Declets[index] = DPDEncoding.Encode(declet);
            parseDouble /= 1000;
            overflow = parseDouble;
            if (overflow == 0)
            {
                break;
            }
        }
    }

    public string DPDToString()
    {
        string isPositive = sign == 0 ? "+" : "-";
        List<string> decletList = new();
        foreach (int declet in Declets)
        {
            decletList.Add(declet.ToString().PadLeft(10, '0'));
        }
        string decletString = String.Join(" ", decletList);
        return String.Concat(isPositive, decletString);
    }

    public string ToInt128String()
    {
        string isPositive = sign == 0 ? "+" : "-";
        string decimalString = ToString();
        string int128 = decimalString.Replace(".", "");
        return String.Concat(isPositive, int128);
    }

    public override string ToString()
    {
        string isPositive = sign == 0 ? "+" : "-";
        List<int> intList = new();
        foreach (int declet in Declets)
        {
            int decoded = DPDEncoding.Decode(declet);
            intList.Add(decoded);
        }

        List<string> stringList = new();
        foreach (int declet in intList)
        {
            string padded = declet.ToString().PadLeft(3, '0');
            stringList.Add(padded);
        }
        return isPositive + String.Concat(stringList).Insert(18, ".");
    }

}

public static class DPDLUT
{
    public static readonly int[] DPDValues = {
        0000000000,
        0000000001,
        0000000010,
        0000000011,
        0000000100,
        0000000101,
        0000000110,
        0000000111,
        0000001000,
        0000001001,
        0000010000,
        0000010001,
        0000010010,
        0000010011,
        0000010100,
        0000010101,
        0000010110,
        0000010111,
        0000011000,
        0000011001,
        0000100000,
        0000100001,
        0000100010,
        0000100011,
        0000100100,
        0000100101,
        0000100110,
        0000100111,
        0000101000,
        0000101001,
        0000110000,
        0000110001,
        0000110010,
        0000110011,
        0000110100,
        0000110101,
        0000110110,
        0000110111,
        0000111000,
        0000111001,
        0001000000,
        0001000001,
        0001000010,
        0001000011,
        0001000100,
        0001000101,
        0001000110,
        0001000111,
        0001001000,
        0001001001,
        0001010000,
        0001010001,
        0001010010,
        0001010011,
        0001010100,
        0001010101,
        0001010110,
        0001010111,
        0001011000,
        0001011001,
        0001100000,
        0001100001,
        0001100010,
        0001100011,
        0001100100,
        0001100101,
        0001100110,
        0001100111,
        0001101000,
        0001101001,
        0001110000,
        0001110001,
        0001110010,
        0001110011,
        0001110100,
        0001110101,
        0001110110,
        0001110111,
        0001111000,
        0001111001,
        0000001010,
        0000001011,
        0000101010,
        0000101011,
        0001001010,
        0001001011,
        0001101010,
        0001101011,
        0001001110,
        0001001111,
        0000011010,
        0000011011,
        0000111010,
        0000111011,
        0001011010,
        0001011011,
        0001111010,
        0001111011,
        0001011110,
        0001011111,
        0010000000,
        0010000001,
        0010000010,
        0010000011,
        0010000100,
        0010000101,
        0010000110,
        0010000111,
        0010001000,
        0010001001,
        0010010000,
        0010010001,
        0010010010,
        0010010011,
        0010010100,
        0010010101,
        0010010110,
        0010010111,
        0010011000,
        0010011001,
        0010100000,
        0010100001,
        0010100010,
        0010100011,
        0010100100,
        0010100101,
        0010100110,
        0010100111,
        0010101000,
        0010101001,
        0010110000,
        0010110001,
        0010110010,
        0010110011,
        0010110100,
        0010110101,
        0010110110,
        0010110111,
        0010111000,
        0010111001,
        0011000000,
        0011000001,
        0011000010,
        0011000011,
        0011000100,
        0011000101,
        0011000110,
        0011000111,
        0011001000,
        0011001001,
        0011010000,
        0011010001,
        0011010010,
        0011010011,
        0011010100,
        0011010101,
        0011010110,
        0011010111,
        0011011000,
        0011011001,
        0011100000,
        0011100001,
        0011100010,
        0011100011,
        0011100100,
        0011100101,
        0011100110,
        0011100111,
        0011101000,
        0011101001,
        0011110000,
        0011110001,
        0011110010,
        0011110011,
        0011110100,
        0011110101,
        0011110110,
        0011110111,
        0011111000,
        0011111001,
        0010001010,
        0010001011,
        0010101010,
        0010101011,
        0011001010,
        0011001011,
        0011101010,
        0011101011,
        0011001110,
        0011001111,
        0010011010,
        0010011011,
        0010111010,
        0010111011,
        0011011010,
        0011011011,
        0011111010,
        0011111011,
        0011011110,
        0011011111,
        0100000000,
        0100000001,
        0100000010,
        0100000011,
        0100000100,
        0100000101,
        0100000110,
        0100000111,
        0100001000,
        0100001001,
        0100010000,
        0100010001,
        0100010010,
        0100010011,
        0100010100,
        0100010101,
        0100010110,
        0100010111,
        0100011000,
        0100011001,
        0100100000,
        0100100001,
        0100100010,
        0100100011,
        0100100100,
        0100100101,
        0100100110,
        0100100111,
        0100101000,
        0100101001,
        0100110000,
        0100110001,
        0100110010,
        0100110011,
        0100110100,
        0100110101,
        0100110110,
        0100110111,
        0100111000,
        0100111001,
        0101000000,
        0101000001,
        0101000010,
        0101000011,
        0101000100,
        0101000101,
        0101000110,
        0101000111,
        0101001000,
        0101001001,
        0101010000,
        0101010001,
        0101010010,
        0101010011,
        0101010100,
        0101010101,
        0101010110,
        0101010111,
        0101011000,
        0101011001,
        0101100000,
        0101100001,
        0101100010,
        0101100011,
        0101100100,
        0101100101,
        0101100110,
        0101100111,
        0101101000,
        0101101001,
        0101110000,
        0101110001,
        0101110010,
        0101110011,
        0101110100,
        0101110101,
        0101110110,
        0101110111,
        0101111000,
        0101111001,
        0100001010,
        0100001011,
        0100101010,
        0100101011,
        0101001010,
        0101001011,
        0101101010,
        0101101011,
        0101001110,
        0101001111,
        0100011010,
        0100011011,
        0100111010,
        0100111011,
        0101011010,
        0101011011,
        0101111010,
        0101111011,
        0101011110,
        0101011111,
        0110000000,
        0110000001,
        0110000010,
        0110000011,
        0110000100,
        0110000101,
        0110000110,
        0110000111,
        0110001000,
        0110001001,
        0110010000,
        0110010001,
        0110010010,
        0110010011,
        0110010100,
        0110010101,
        0110010110,
        0110010111,
        0110011000,
        0110011001,
        0110100000,
        0110100001,
        0110100010,
        0110100011,
        0110100100,
        0110100101,
        0110100110,
        0110100111,
        0110101000,
        0110101001,
        0110110000,
        0110110001,
        0110110010,
        0110110011,
        0110110100,
        0110110101,
        0110110110,
        0110110111,
        0110111000,
        0110111001,
        0111000000,
        0111000001,
        0111000010,
        0111000011,
        0111000100,
        0111000101,
        0111000110,
        0111000111,
        0111001000,
        0111001001,
        0111010000,
        0111010001,
        0111010010,
        0111010011,
        0111010100,
        0111010101,
        0111010110,
        0111010111,
        0111011000,
        0111011001,
        0111100000,
        0111100001,
        0111100010,
        0111100011,
        0111100100,
        0111100101,
        0111100110,
        0111100111,
        0111101000,
        0111101001,
        0111110000,
        0111110001,
        0111110010,
        0111110011,
        0111110100,
        0111110101,
        0111110110,
        0111110111,
        0111111000,
        0111111001,
        0110001010,
        0110001011,
        0110101010,
        0110101011,
        0111001010,
        0111001011,
        0111101010,
        0111101011,
        0111001110,
        0111001111,
        0110011010,
        0110011011,
        0110111010,
        0110111011,
        0111011010,
        0111011011,
        0111111010,
        0111111011,
        0111011110,
        0111011111,
        1000000000,
        1000000001,
        1000000010,
        1000000011,
        1000000100,
        1000000101,
        1000000110,
        1000000111,
        1000001000,
        1000001001,
        1000010000,
        1000010001,
        1000010010,
        1000010011,
        1000010100,
        1000010101,
        1000010110,
        1000010111,
        1000011000,
        1000011001,
        1000100000,
        1000100001,
        1000100010,
        1000100011,
        1000100100,
        1000100101,
        1000100110,
        1000100111,
        1000101000,
        1000101001,
        1000110000,
        1000110001,
        1000110010,
        1000110011,
        1000110100,
        1000110101,
        1000110110,
        1000110111,
        1000111000,
        1000111001,
        1001000000,
        1001000001,
        1001000010,
        1001000011,
        1001000100,
        1001000101,
        1001000110,
        1001000111,
        1001001000,
        1001001001,
        1001010000,
        1001010001,
        1001010010,
        1001010011,
        1001010100,
        1001010101,
        1001010110,
        1001010111,
        1001011000,
        1001011001,
        1001100000,
        1001100001,
        1001100010,
        1001100011,
        1001100100,
        1001100101,
        1001100110,
        1001100111,
        1001101000,
        1001101001,
        1001110000,
        1001110001,
        1001110010,
        1001110011,
        1001110100,
        1001110101,
        1001110110,
        1001110111,
        1001111000,
        1001111001,
        1000001010,
        1000001011,
        1000101010,
        1000101011,
        1001001010,
        1001001011,
        1001101010,
        1001101011,
        1001001110,
        1001001111,
        1000011010,
        1000011011,
        1000111010,
        1000111011,
        1001011010,
        1001011011,
        1001111010,
        1001111011,
        1001011110,
        1001011111,
        1010000000,
        1010000001,
        1010000010,
        1010000011,
        1010000100,
        1010000101,
        1010000110,
        1010000111,
        1010001000,
        1010001001,
        1010010000,
        1010010001,
        1010010010,
        1010010011,
        1010010100,
        1010010101,
        1010010110,
        1010010111,
        1010011000,
        1010011001,
        1010100000,
        1010100001,
        1010100010,
        1010100011,
        1010100100,
        1010100101,
        1010100110,
        1010100111,
        1010101000,
        1010101001,
        1010110000,
        1010110001,
        1010110010,
        1010110011,
        1010110100,
        1010110101,
        1010110110,
        1010110111,
        1010111000,
        1010111001,
        1011000000,
        1011000001,
        1011000010,
        1011000011,
        1011000100,
        1011000101,
        1011000110,
        1011000111,
        1011001000,
        1011001001,
        1011010000,
        1011010001,
        1011010010,
        1011010011,
        1011010100,
        1011010101,
        1011010110,
        1011010111,
        1011011000,
        1011011001,
        1011100000,
        1011100001,
        1011100010,
        1011100011,
        1011100100,
        1011100101,
        1011100110,
        1011100111,
        1011101000,
        1011101001,
        1011110000,
        1011110001,
        1011110010,
        1011110011,
        1011110100,
        1011110101,
        1011110110,
        1011110111,
        1011111000,
        1011111001,
        1010001010,
        1010001011,
        1010101010,
        1010101011,
        1011001010,
        1011001011,
        1011101010,
        1011101011,
        1011001110,
        1011001111,
        1010011010,
        1010011011,
        1010111010,
        1010111011,
        1011011010,
        1011011011,
        1011111010,
        1011111011,
        1011011110,
        1011011111,
        1100000000,
        1100000001,
        1100000010,
        1100000011,
        1100000100,
        1100000101,
        1100000110,
        1100000111,
        1100001000,
        1100001001,
        1100010000,
        1100010001,
        1100010010,
        1100010011,
        1100010100,
        1100010101,
        1100010110,
        1100010111,
        1100011000,
        1100011001,
        1100100000,
        1100100001,
        1100100010,
        1100100011,
        1100100100,
        1100100101,
        1100100110,
        1100100111,
        1100101000,
        1100101001,
        1100110000,
        1100110001,
        1100110010,
        1100110011,
        1100110100,
        1100110101,
        1100110110,
        1100110111,
        1100111000,
        1100111001,
        1101000000,
        1101000001,
        1101000010,
        1101000011,
        1101000100,
        1101000101,
        1101000110,
        1101000111,
        1101001000,
        1101001001,
        1101010000,
        1101010001,
        1101010010,
        1101010011,
        1101010100,
        1101010101,
        1101010110,
        1101010111,
        1101011000,
        1101011001,
        1101100000,
        1101100001,
        1101100010,
        1101100011,
        1101100100,
        1101100101,
        1101100110,
        1101100111,
        1101101000,
        1101101001,
        1101110000,
        1101110001,
        1101110010,
        1101110011,
        1101110100,
        1101110101,
        1101110110,
        1101110111,
        1101111000,
        1101111001,
        1100001010,
        1100001011,
        1100101010,
        1100101011,
        1101001010,
        1101001011,
        1101101010,
        1101101011,
        1101001110,
        1101001111,
        1100011010,
        1100011011,
        1100111010,
        1100111011,
        1101011010,
        1101011011,
        1101111010,
        1101111011,
        1101011110,
        1101011111,
        1110000000,
        1110000001,
        1110000010,
        1110000011,
        1110000100,
        1110000101,
        1110000110,
        1110000111,
        1110001000,
        1110001001,
        1110010000,
        1110010001,
        1110010010,
        1110010011,
        1110010100,
        1110010101,
        1110010110,
        1110010111,
        1110011000,
        1110011001,
        1110100000,
        1110100001,
        1110100010,
        1110100011,
        1110100100,
        1110100101,
        1110100110,
        1110100111,
        1110101000,
        1110101001,
        1110110000,
        1110110001,
        1110110010,
        1110110011,
        1110110100,
        1110110101,
        1110110110,
        1110110111,
        1110111000,
        1110111001,
        1111000000,
        1111000001,
        1111000010,
        1111000011,
        1111000100,
        1111000101,
        1111000110,
        1111000111,
        1111001000,
        1111001001,
        1111010000,
        1111010001,
        1111010010,
        1111010011,
        1111010100,
        1111010101,
        1111010110,
        1111010111,
        1111011000,
        1111011001,
        1111100000,
        1111100001,
        1111100010,
        1111100011,
        1111100100,
        1111100101,
        1111100110,
        1111100111,
        1111101000,
        1111101001,
        1111110000,
        1111110001,
        1111110010,
        1111110011,
        1111110100,
        1111110101,
        1111110110,
        1111110111,
        1111111000,
        1111111001,
        1110001010,
        1110001011,
        1110101010,
        1110101011,
        1111001010,
        1111001011,
        1111101010,
        1111101011,
        1111001110,
        1111001111,
        1110011010,
        1110011011,
        1110111010,
        1110111011,
        1111011010,
        1111011011,
        1111111010,
        1111111011,
        1111011110,
        1111011111,
        0000001100,
        0000001101,
        0100001100,
        0100001101,
        1000001100,
        1000001101,
        1100001100,
        1100001101,
        0000101110,
        0000101111,
        0000011100,
        0000011101,
        0100011100,
        0100011101,
        1000011100,
        1000011101,
        1100011100,
        1100011101,
        0000111110,
        0000111111,
        0000101100,
        0000101101,
        0100101100,
        0100101101,
        1000101100,
        1000101101,
        1100101100,
        1100101101,
        0100101110,
        0100101111,
        0000111100,
        0000111101,
        0100111100,
        0100111101,
        1000111100,
        1000111101,
        1100111100,
        1100111101,
        0100111110,
        0100111111,
        0001001100,
        0001001101,
        0101001100,
        0101001101,
        1001001100,
        1001001101,
        1101001100,
        1101001101,
        1000101110,
        1000101111,
        0001011100,
        0001011101,
        0101011100,
        0101011101,
        1001011100,
        1001011101,
        1101011100,
        1101011101,
        1000111110,
        1000111111,
        0001101100,
        0001101101,
        0101101100,
        0101101101,
        1001101100,
        1001101101,
        1101101100,
        1101101101,
        1100101110,
        1100101111,
        0001111100,
        0001111101,
        0101111100,
        0101111101,
        1001111100,
        1001111101,
        1101111100,
        1101111101,
        1100111110,
        1100111111,
        0000001110,
        0000001111,
        0100001110,
        0100001111,
        1000001110,
        1000001111,
        1100001110,
        1100001111,
        0001101110,
        0001101111,
        0000011110,
        0000011111,
        0100011110,
        0100011111,
        1000011110,
        1000011111,
        1100011110,
        1100011111,
        0001111110,
        0001111111,
        0010001100,
        0010001101,
        0110001100,
        0110001101,
        1010001100,
        1010001101,
        1110001100,
        1110001101,
        0010101110,
        0010101111,
        0010011100,
        0010011101,
        0110011100,
        0110011101,
        1010011100,
        1010011101,
        1110011100,
        1110011101,
        0010111110,
        0010111111,
        0010101100,
        0010101101,
        0110101100,
        0110101101,
        1010101100,
        1010101101,
        1110101100,
        1110101101,
        0110101110,
        0110101111,
        0010111100,
        0010111101,
        0110111100,
        0110111101,
        1010111100,
        1010111101,
        1110111100,
        1110111101,
        0110111110,
        0110111111,
        0011001100,
        0011001101,
        0111001100,
        0111001101,
        1011001100,
        1011001101,
        1111001100,
        1111001101,
        1010101110,
        1010101111,
        0011011100,
        0011011101,
        0111011100,
        0111011101,
        1011011100,
        1011011101,
        1111011100,
        1111011101,
        1010111110,
        1010111111,
        0011101100,
        0011101101,
        0111101100,
        0111101101,
        1011101100,
        1011101101,
        1111101100,
        1111101101,
        1110101110,
        1110101111,
        0011111100,
        0011111101,
        0111111100,
        0111111101,
        1011111100,
        1011111101,
        1111111100,
        1111111101,
        1110111110,
        1110111111,
        0010001110,
        0010001111,
        0110001110,
        0110001111,
        1010001110,
        1010001111,
        1110001110,
        1110001111,
        0011101110,
        0011101111,
        0010011110,
        0010011111,
        0110011110,
        0110011111,
        1010011110,
        1010011111,
        1110011110,
        1110011111,
        0011111110,
        0011111111
    };
}
