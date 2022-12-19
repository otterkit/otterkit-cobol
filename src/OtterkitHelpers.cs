namespace OtterkitLibrary;

public static class RuntimeHelpers{
    public static DateTime new_date(Numeric int_fmt)
    {
        DateTime Y1600 = new(1600,12,31);
        int days = int.Parse(int_fmt.Display);

        DateTime integer_date = Y1600.AddDays(days);

        return integer_date;
    }
}

