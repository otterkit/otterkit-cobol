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

    public OtterkitDPD()
    {

    }

    public void ToDeclets(int number)
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
    
    public string DecletsToString()
    {
        List<string> decletList = new();
        foreach (int declet in Declets)
        {
            decletList.Add(declet.ToString().PadLeft(10, '0'));
        }
        return String.Join(" ",decletList);
    }

    public override string ToString()
    {
        List<int> uintList = new();
        foreach (int declet in Declets)
        {
            int decoded = DPDEncoding.Decode(declet);
            uintList.Add(decoded);
        }

        List<string> stringList = new();
        foreach (int declet in uintList)
        {
            string padded = declet.ToString().PadLeft(3, '0');
            stringList.Add(padded);
        }
        return String.Concat(stringList).Insert(18, ".");
    }

}