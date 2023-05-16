namespace Otterkit.Types;

// Don't use a separate class or struct for this,
// a ValueTuple is enough, and gives us extra syntax sugar
using ParameterTuple = ValueTuple<DataEntry, bool, bool>;

public class CallableUnit : AbstractUnit
{
    public LocalNames<AbstractEntry> LocalNames = new();
    public DataNames<DataEntry> DataNames = new();
    public List<ParameterTuple> Parameters = new();
    public Option<DataEntry> Returning;
    public bool Override;
    public bool IsFinal;

    public CallableUnit(Token identifier, UnitKind sourceKind)
        : base (identifier, sourceKind) { }
}
