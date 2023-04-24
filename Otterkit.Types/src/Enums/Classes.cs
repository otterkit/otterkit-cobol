namespace Otterkit.Types;

[Flags]
public enum Classes
{
    Error,
    Alphabetic,
    Alphanumeric    = 1 << 1,
    Boolean         = 1 << 2,
    Index           = 1 << 3,
    MessageTag      = 1 << 4,
    National        = 1 << 5,
    Numeric         = 1 << 6,
    Object          = 1 << 7,
    Pointer         = 1 << 8
}
