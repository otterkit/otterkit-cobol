using System.Text.Json.Serialization;

namespace Otterkit.Types;

[JsonConverter(typeof(JsonStringEnumConverter))]
public enum SourceFormat
{
    Auto,
    Fixed,
    Free,
}

[JsonConverter(typeof(JsonStringEnumConverter))]
public enum BuildType
{
    ParseOnly,
    PrintTokens,
    PrintSymbols,
    BuildOnly,
    BuildAndRun,
}

[JsonConverter(typeof(JsonStringEnumConverter))]
public enum OutputType
{
    Application,
    Library,
}
