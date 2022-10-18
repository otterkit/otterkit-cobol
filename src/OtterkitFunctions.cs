using static OtterkitLibrary.OtterkitTypes;
using OtterkitLibrary.Numerics;
using System.Numerics;

namespace OtterkitLibrary;
public static class Functions
{
    public static BigDecimal ABS(Numeric argument)
    {
        return Math.Abs(argument.Value);
    }

}
