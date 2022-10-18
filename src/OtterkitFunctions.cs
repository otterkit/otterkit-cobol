using static OtterkitLibrary.OtterkitTypes;
using OtterkitLibrary.Numerics;

namespace OtterkitLibrary;
public static class Functions
{
    public static BigDecimal ABS(Numeric argument)
    {
        return DecMath.Abs(argument.Value);
    }

}
