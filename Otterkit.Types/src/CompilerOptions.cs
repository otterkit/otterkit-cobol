namespace Otterkit.Types;

public static class CompilerOptions
{
    public static string Name = "OtterkitExport";
    public static string Main = "main.cob";
    public static OutputType Output = OutputType.Application;
    public static SourceFormat Format = SourceFormat.Auto;
    public static BuildType Mode = BuildType.BuildOnly;
    public static int Columns = 80;

    public static void Initialize(string name, string main)
    {
        Name = name;
        Main = main;
    }

    public static void SetOptions(OutputType output, SourceFormat format, int columns)
    {
        Output = output;
        Format = format;
        Columns = columns;
    }
}
