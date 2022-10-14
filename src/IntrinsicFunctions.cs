using static OtterkitLibrary.OtterkitTypes;
namespace OtterkitLibrary;
public static class IntrinsicFunctions
{
    public static decimal ABS(Numeric argument)
    {
        return Math.Abs(argument.Value);
    }
}
