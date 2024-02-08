namespace Otterkit.Types;

public class InterfaceUnit : AbstractUnit
{
    public bool IsFinal;
    public List<string> Using = new();
    public List<ClassUnit> Inherits = new();

    // Interface prototype methods
    public List<CallableUnit> Methods = new();

    public InterfaceUnit(Token identifier, UnitKind sourceKind)
        : base (identifier, sourceKind) { }
}