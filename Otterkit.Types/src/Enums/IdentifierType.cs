namespace Otterkit.Types;

[Flags] 
public enum IdentifierType
{
    None = 0,
    Self = 1,
    Super = 1 << 1,
    Function = 1 << 2,
    NullObject = 1 << 3,
    ObjectView = 1 << 4,
    DataAddress = 1 << 5,
    NullAddress = 1 << 6,
    ReferenceMod = 1 << 7,
    LinageCounter = 1 << 8,
    ReportCounter = 1 << 9,
    ProgramAddress = 1 << 10,
    FunctionAddress = 1 << 11,
    ExceptionObject = 1 << 12,
    MethodInvocation = 1 << 13,
}
