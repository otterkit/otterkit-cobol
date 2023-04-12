namespace Otterkit;

public enum EntryType
{
    FileControl,
    FileDescription,
    ScreenDescription,
    DataDescription,
}

public abstract class AbstractEntry
{
    public Token Identifier;
    public EntryType EntryType;
    public string ExternalizedName = "";

    protected AbstractEntry(Token identifier, EntryType entryType)
    {
        Identifier = identifier;
        EntryType = entryType;
    }
}
