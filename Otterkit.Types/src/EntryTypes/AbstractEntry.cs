namespace Otterkit.Types;

public abstract class AbstractEntry
{
    public Option<Token> Identifier;
    public EntryKind EntryKind;

    protected AbstractEntry(Token identifier, EntryKind kind)
    {
        Identifier = identifier;
        EntryKind = kind;
    }
}
