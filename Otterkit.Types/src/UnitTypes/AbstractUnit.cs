namespace Otterkit.Types;

public abstract class AbstractUnit
{
    public Token Identifier;
    public UnitKind SourceKind;
    public Option<string> ExternalizedName;

    protected AbstractUnit(Token identifier, UnitKind sourceKind)
    {
        Identifier = identifier;
        SourceKind = sourceKind;
    }
}
