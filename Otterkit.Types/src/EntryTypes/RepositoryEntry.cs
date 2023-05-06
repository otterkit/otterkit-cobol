using Otterkit.Types;

namespace Otterkit;

public class RepositoryEntry
{
    public Option<Token> Identifier;
    public EntryKind EntryType;
    public Option<string> ExternalizedName;

    public SourceUnit SourceType;

    public RepositoryEntry(Token identifier, EntryKind entryType) 
    {
        Identifier = identifier;
        EntryType = entryType;
    }
}
