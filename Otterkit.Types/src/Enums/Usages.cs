namespace Otterkit.Types;

[Flags]
public enum Usages
{
    None,
    Binary,
    BinaryChar      = 1 << 1,
    BinaryShort     = 1 << 2,
    BinaryLong      = 1 << 3,
    BinaryDouble    = 1 << 4,
    Bit             = 1 << 5,
    Computational   = 1 << 6,
    Display         = 1 << 7,
    FloatBinary32   = 1 << 8,
    FloatBinary64   = 1 << 9,
    FloatBinary128  = 1 << 10,
    FloatDecimal16  = 1 << 11,
    FloatDecimal32  = 1 << 12,
    FloatExtended   = 1 << 13,
    FloatLong       = 1 << 14,
    FloatShort      = 1 << 15,
    Index           = 1 << 16,
    MessageTag      = 1 << 17,
    National        = 1 << 18,
    ObjectReference = 1 << 19,
    PackedDecimal   = 1 << 20,
    DataPointer     = 1 << 21,
    FunctionPointer = 1 << 22,
    ProgramPointer  = 1 << 23
}
