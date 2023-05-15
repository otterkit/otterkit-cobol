namespace Otterkit.Types;

public abstract class AbstractPrototype
{
    public Token Identifier;
    public UnitKind SourceKind;
    public Option<string> ExternalizedName;

    protected AbstractPrototype(Token identifier, UnitKind sourceKind)
    {
        Identifier = identifier;
        SourceKind = sourceKind;
    }
}
