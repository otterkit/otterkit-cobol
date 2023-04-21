namespace Otterkit.Types;

public abstract class AbstractPrototype
{
    public Token Identifier;
    public SourceUnit SourceType;
    public Option<string> ExternalizedName;

    protected AbstractPrototype(Token identifier, SourceUnit sourcetype)
    {
        Identifier = identifier;
        SourceType = sourcetype;
    }
}
