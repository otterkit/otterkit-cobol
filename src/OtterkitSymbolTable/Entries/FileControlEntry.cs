namespace Otterkit;

public class FileControlEntry : AbstractEntry
{
    public CurrentScope Section;
    public List<Token> Assign;
    public bool HasUsing;
    public string Organization = "";

    public FileControlEntry(Token identifier, EntryType entryType, bool hasUsing) 
        : base(identifier, entryType)
    {
        Assign = new(1);
        HasUsing = hasUsing;
    }
}
