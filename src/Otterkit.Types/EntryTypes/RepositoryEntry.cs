using Otterkit.Types;

namespace Otterkit;

public class RepositoryEntry : AbstractEntry
{
    public SourceUnit SourceType;
    public string Expands = string.Empty;
    public List<string>? Using;

    public RepositoryEntry(Token identifier, EntryType entryType) 
        : base(identifier, entryType) { }
}
