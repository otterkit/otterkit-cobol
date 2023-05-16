using Otterkit.Types;

namespace Otterkit;

public class RepositoryEntry : AbstractEntry
{
    public UnitKind SourceType;

    public RepositoryEntry(Token identifier, EntryKind entryKind)
        : base (identifier, entryKind) { }

    public bool this[RepositoryClause clauseName]
    {
        get => GetBit((int)clauseName);

        set => SetBit((int)clauseName, value);
    }
}
