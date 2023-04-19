namespace Otterkit.Types;

public enum SourceFormat
{
    Auto,
    Fixed,
    Free,
}

public enum BuildType
{
    ParseOnly,
    PrintTokens,
    PrintSymbols,
    BuildOnly,
    BuildAndRun,
}

public enum OutputType
{
    Application,
    Library,
}
