using Otterkit.Types;

namespace Otterkit;

public class FileControlEntry : AbstractEntry
{
    public Option<string> ExternalizedName;

    public SourceScope Section;
    public List<Token> Assign;
    public bool HasUsing;
    public Option<string> Organization;

    public FileControlEntry(Token identifier, EntryKind entryType, bool hasUsing)
        : base (identifier, entryType)
    {
        Assign = new(1);
        HasUsing = hasUsing;
    }
}
