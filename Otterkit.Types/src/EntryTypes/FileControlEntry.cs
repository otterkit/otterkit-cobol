using Otterkit.Types;

namespace Otterkit;

public class FileControlEntry
{
    public Option<Token> Identifier;
    public EntryKind EntryType;
    public Option<string> ExternalizedName;

    public SourceScope Section;
    public List<Token> Assign;
    public bool HasUsing;
    public Option<string> Organization;

    public FileControlEntry(Token identifier, EntryKind entryType, bool hasUsing) 
    {
        Identifier = identifier;
        EntryType = entryType;
        Assign = new(1);
        HasUsing = hasUsing;
    }
}
