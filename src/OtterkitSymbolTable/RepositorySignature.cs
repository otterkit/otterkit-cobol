namespace Otterkit;

public record RepositorySignature
{
    public string Identifier = string.Empty;
    public SourceUnit SourceType;
    public string ExternalizedIdentifier = string.Empty;
    public string Expands = string.Empty;
    public List<string>? Using;
}
