namespace Otterkit;

public record SourceUnitSignature
{
    public string Identifier = string.Empty;
    public SourceUnit SourceType;
    public List<string> Parameters = new();
    public List<int> ParameterSizes = new();
    public List<bool> IsOptional = new();
    public List<bool> IsByRef = new();
    public string Returning = string.Empty;
}
