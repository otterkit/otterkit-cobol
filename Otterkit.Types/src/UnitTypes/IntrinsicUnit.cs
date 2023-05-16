namespace Otterkit.Types;

public record struct IntrinsicType(Classes Class, Categories Category);

public class IntrinsicUnit : AbstractUnit
{
    public List<IntrinsicType> Parameters = new();
    public List<bool> IsOptional = new();
    public Option<IntrinsicType> Returning;

    public IntrinsicUnit(Token identifier, UnitKind sourceKind)
        : base (identifier, sourceKind) { }

    public (List<IntrinsicType> Types, List<bool> IsOptional) GetParameters()
    {
        return (Parameters, IsOptional);
    }
}
