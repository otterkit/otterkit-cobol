using Otterkit.Types;

namespace Otterkit;

public class RepositoryEntry
{
    public Option<Token> Identifier;
    public EntryType EntryType;
    public string ExternalizedName = "";

    public SourceUnit SourceType;
    public string Expands = string.Empty;
    public List<string>? Using;

    public RepositoryEntry(Token identifier, EntryType entryType) 
    {
        Identifier = identifier;
        EntryType = entryType;
    }
}
