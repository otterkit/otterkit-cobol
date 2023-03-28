namespace Otterkit;

public abstract class AbstractSignature
{
    public Token Identifier;
    public SourceUnit SourceType;

    protected AbstractSignature(Token identifier, SourceUnit sourcetype)
    {
        Identifier = identifier;
        SourceType = sourcetype;
    }
}
