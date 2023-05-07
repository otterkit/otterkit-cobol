namespace Otterkit.Types;

[Flags]
public enum Categories
{
    Invalid,
    Alphabetic,
    Alphanumeric            = 1 << 1,
    AlphanumericEdited      = 1 << 2,
    Boolean                 = 1 << 3,
    Index                   = 1 << 4,
    MessageTag              = 1 << 5,
    National                = 1 << 6,
    NationalEdited          = 1 << 7,
    Numeric                 = 1 << 8,
    NumericEdited           = 1 << 9,
    ObjectReference         = 1 << 10,
    DataPointer             = 1 << 11,
    FunctionPointer         = 1 << 12,
    ProgramPointer          = 1 << 13
}
