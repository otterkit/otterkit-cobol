using Otterkit.Types;

namespace Otterkit;

public class RepositoryEntry : AbstractEntry
{
    public UnitKind SourceType;
    public int DeclarationIndex;

    public RepositoryEntry(Token identifier, EntryKind entryKind)
        : base (identifier, entryKind) { }
}
