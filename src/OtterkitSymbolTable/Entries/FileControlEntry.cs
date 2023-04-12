namespace Otterkit;

public class FileControlEntry : AbstractEntry
{
    public CurrentScope Section;
    public Token AssignTo;
    public Token Using;
    public string Organization = "";

    public FileControlEntry(Token identifier, EntryType entryType, Token assignTo, Token _using) 
        : base(identifier, entryType)
    {
        AssignTo = assignTo;
        Using = _using;
    }
}
