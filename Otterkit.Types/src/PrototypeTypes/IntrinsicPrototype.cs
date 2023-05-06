namespace Otterkit.Types;

public record struct IntrinsicType(Classes Class, Categories Category);

public class IntrinsicPrototype : AbstractPrototype
{
    public List<IntrinsicType> Parameters = new();
    public List<bool> IsOptional = new();
    public Option<IntrinsicType> Returning;

    public IntrinsicPrototype(Token identifier, SourceUnit sourcetype)
        : base (identifier, sourcetype) { }

    public (List<IntrinsicType> Types, List<bool> IsOptional) GetParameters()
    {
        return (Parameters, IsOptional);
    }
}
