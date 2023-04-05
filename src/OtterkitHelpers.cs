namespace Otterkit.Library;

public static class RuntimeHelpers
{
    public static ICOBOLType GenericTest<T>(T argument) where T : ICOBOLType
    {
        if (argument is Alphanumeric)
        {
            Console.WriteLine("IS ALPHANUMERIC");
        }

        if (argument is National)
        {
            Console.WriteLine("IS NATIONAL");

            int value = argument.Bytes[0];

            Console.WriteLine(value);
        }

        if (argument is not Alphanumeric and not National and not Numeric)
        {
            throw new EcArgumentFunction("GenericTest function argument must be Alphanumeric, National, Numeric");
        }

        return new Numeric("1.55"u8, true);
    }
    
    public static DateTime New_date(Numeric int_fmt)
    {
        DateTime Y1600 = new(1600,12,31);
        int days = int.Parse(int_fmt.Display);

        DateTime integer_date = Y1600.AddDays(days);

        return integer_date;
    }
}

