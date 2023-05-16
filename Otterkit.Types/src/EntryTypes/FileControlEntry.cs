using Otterkit.Types;

namespace Otterkit;

public class FileControlEntry : AbstractEntry
{
    public Option<string> ExternalizedName;

    public FileControlEntry(Token identifier, EntryKind entryKind)
        : base (identifier, entryKind) { }
}
