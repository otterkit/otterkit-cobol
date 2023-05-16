namespace Otterkit.Types;

public class ClassUnit : AbstractUnit
{
    public bool IsFinal;
    public List<string> Using = new();
    public Option<ClassUnit> Inherits;

    public List<InterfaceUnit> FactoryImplements = new();
    public List<InterfaceUnit> ObjectImplements = new();

    public List<CallableUnit> FactoryMethods = new();    
    public List<CallableUnit> ObjectMethods = new();

    public ClassUnit(Token identifier, UnitKind sourceKind)
        : base (identifier, sourceKind) { }
}
