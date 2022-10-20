using System.Numerics;
namespace OtterkitLibrary;

public static class DPDEncoding
{
    public static int Encode(int integer)
    {
        if (integer < 0 || integer > 999)
        {
            throw new ArgumentOutOfRangeException("Densely Packed Decimal encoding only accepts values between 0 and 999");
        }
        int DPD = LUTS.DPDLUT[integer];
        return DPD;
    }

    public static int Decode(int DPD)
    {
        if (LUTS.DPDLUT.Contains(DPD) == false)
        {
            throw new ArgumentException("Argument is not a valid Densely Packed Decimal value");
        }
        int integer = Array.IndexOf(LUTS.DPDLUT, DPD);
        return integer;
    }  
}

public struct OtterkitDPD
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

    public OtterkitDPD(int sign, int[] declets)
    {
        this.sign = sign;
        this.Declets = declets;
    }

    public OtterkitDPD(int sign, int integer)
    {
        this.sign = sign;
        ToDeclets(integer);
    }

    public OtterkitDPD(int sign, long integer)
    {
        this.sign = sign;
        ToDeclets(integer);
    }

    public OtterkitDPD(int sign, Int128 integer)
    {
        this.sign = sign;
        ToDeclets(integer);
    }

    public static implicit operator OtterkitDPD(int value)
    {
        if (value < 0)
        {
            return new OtterkitDPD(1, value);
        }
        return new OtterkitDPD(0, value);
    }

    public static implicit operator OtterkitDPD(long value)
    {
        if (value < 0)
        {
            return new OtterkitDPD(1, value);
        }
        return new OtterkitDPD(0, value);
    }

    public static implicit operator OtterkitDPD(Int128 value)
    {
        if (value < 0)
        {
            return new OtterkitDPD(1, value);
        }
        return new OtterkitDPD(0, value);
    }

    private static OtterkitDPD Add(OtterkitDPD left, OtterkitDPD right)
    {
        Int128 leftInteger = Int128.Parse(left.ToInt128String());
        Int128 rightInteger = Int128.Parse(left.ToInt128String());
        Int128 sum =  leftInteger + rightInteger;

        return new OtterkitDPD(0, sum);
    }

    private static OtterkitDPD Subtract(OtterkitDPD left, OtterkitDPD right)
    {
        Int128 leftInteger = Int128.Parse(left.ToInt128String());
        Int128 rightInteger = Int128.Parse(left.ToInt128String());
        Int128 difference =  leftInteger - rightInteger;

        return new OtterkitDPD(0, difference);
    }

    private static OtterkitDPD Multiply(OtterkitDPD left, OtterkitDPD right)
    {
        BigInteger leftNum = BigInteger.Parse(left.ToInt128String());
        BigInteger rightNum = BigInteger.Parse(right.ToInt128String());
        leftNum = leftNum / 1000000000000000000;
        Int128 product = (Int128)(leftNum * rightNum);

        return new OtterkitDPD(0, product);
    }

    private static OtterkitDPD Divide(OtterkitDPD left, OtterkitDPD right)
    {
        BigInteger leftNum = BigInteger.Parse(left.ToInt128String());
        BigInteger rightNum = BigInteger.Parse(right.ToInt128String());
        leftNum = leftNum * 1000000000000000000;
        Int128 quotient = (Int128)(leftNum / rightNum);

        return new OtterkitDPD(0, quotient);
    }

    public static OtterkitDPD operator +(OtterkitDPD left, OtterkitDPD right)
    {
        return Add(left, right);
    }

    public static OtterkitDPD operator -(OtterkitDPD left, OtterkitDPD right)
    {
        return Subtract(left, right);
    }

    public static OtterkitDPD operator *(OtterkitDPD left, OtterkitDPD right)
    {
        return Multiply(left, right);
    }

    public static OtterkitDPD operator /(OtterkitDPD left, OtterkitDPD right)
    {
        return Divide(left, right);
    }

    private void ToDeclets(int number)
    {
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

    private void ToDeclets(long number)
    {
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

    private void ToDeclets(Int128 number)
    {
        Int128 overflow = 0;
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
    
    public string DPDToString()
    {
        List<string> decletList = new();
        foreach (int declet in Declets)
        {
            decletList.Add(declet.ToString().PadLeft(10, '0'));
        }
        return String.Join(" ",decletList);
    }

    public string ToInt128String()
    {
        string decimalString = ToString();
        string Int128 = decimalString.Replace(".", "");
        return Int128;
    }

    public override string ToString()
    {
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
        return String.Concat(stringList).Insert(18, ".");
    }

}