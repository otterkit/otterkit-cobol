namespace Otterkit;

[Flags] 
public enum IdentifierType : short
{
    None = 0,
    Function = 1,
    ReferenceMod = 2,
    MethodInvocation = 4,
    ObjectView = 8,
    ExceptionObject = 16,
    NullObject = 34,
    Self = 64,
    Super = 128,
    NullAddress = 256,
    DataAddress = 512,
    FunctionAddress = 1024,
    ProgramAddress = 2048,
    LinageCounter = 4096,
    ReportCounter = 8192,
}
