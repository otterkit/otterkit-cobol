using Otterkit.Types;

namespace Otterkit;

public class FileControlEntry
{
    public Option<Token> Identifier;
    public EntryType EntryType;
    public string ExternalizedName = "";

    public CurrentScope Section;
    public List<Token> Assign;
    public bool HasUsing;
    public string Organization = "";

    public FileControlEntry(Token identifier, EntryType entryType, bool hasUsing) 
    {
        Identifier = identifier;
        EntryType = entryType;
        Assign = new(1);
        HasUsing = hasUsing;
    }
}
