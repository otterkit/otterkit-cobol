namespace Otterkit;

public static class CompilerOptions
{
    public static string Name = "OtterkitExport";
    public static string EntryPoint = "main.cob";
    public static int ColumnLength = 80;
    public static SourceFormat SourceFormat = SourceFormat.Fixed;
    public static OutputType Output = OutputType.Application;
    public static BuildType BuildMode = BuildType.BuildOnly;
    public static List<Token> SourceTokens = new();
    public static List<string> FileNames = new();
}
