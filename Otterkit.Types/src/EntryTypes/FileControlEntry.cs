using Otterkit.Types;

namespace Otterkit;

public class FileControlEntry : AbstractEntry
{
    public Option<string> ExternalizedName;
    public int DeclarationIndex;

    public FileControlEntry(Token identifier, EntryKind entryKind)
        : base (identifier, entryKind) { }
}
