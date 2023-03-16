namespace Otterkit;

public enum UsageType
{
    None,
    //> Used only for identifier validation:
    Alphanumeric,
    Alphabetic,
    Boolean,
    Decimal,
    Integer,
    //< 
    Binary,
    BinaryChar,
    BinaryShort,
    BinaryLong,
    BinaryDouble,
    Bit,
    Computational,
    Display,
    FloatBinary32,
    FloatBinary64,
    FloatBinary128,
    FloatDecimal16,
    FloatDecimal32,
    FloatExtended,
    FloatLong,
    FloatShort,
    Index,
    MessageTag,
    National,
    ObjectReference,
    PackedDecimal,
    DataPointer,
    FunctionPointer,
    ProgramPointer
}
