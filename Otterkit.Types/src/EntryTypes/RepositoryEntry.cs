using Otterkit.Types;

namespace Otterkit;

public class RepositoryEntry
{
    public Option<Token> Identifier;
    public EntryType EntryType;
    public Option<string> ExternalizedName;

    public SourceUnit SourceType;

    public RepositoryEntry(Token identifier, EntryType entryType) 
    {
        Identifier = identifier;
        EntryType = entryType;
    }
}
