namespace Otterkit.Types;

public class IntrinsicPrototype : AbstractPrototype
{
    public List<ElementaryType> Parameters = new();
    public List<bool> IsOptional = new();
    public Option<ElementaryType> Returning;

    public IntrinsicPrototype(Token identifier, SourceUnit sourcetype)
        : base (identifier, sourcetype) { }

    public (List<ElementaryType> Types, List<bool> IsOptional) GetParameters()
    {
        return (Parameters, IsOptional);
    }
}
