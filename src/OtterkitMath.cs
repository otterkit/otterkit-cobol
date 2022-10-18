using System.Numerics;
using OtterkitLibrary.Numerics;

namespace OtterkitLibrary;

public static class DecMath
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