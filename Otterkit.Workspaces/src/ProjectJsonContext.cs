using System.Text.Json.Serialization;

namespace Otterkit.Workspaces;

[JsonSerializable(typeof(Otterproj))]
[JsonSourceGenerationOptions(
    GenerationMode = JsonSourceGenerationMode.Default,
    PropertyNamingPolicy = JsonKnownNamingPolicy.CamelCase, 
    DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull,
    WriteIndented = true
)]
public partial class ProjectJsonContext : JsonSerializerContext { }
