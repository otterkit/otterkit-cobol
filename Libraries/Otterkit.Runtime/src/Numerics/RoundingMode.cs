namespace Otterkit.Numerics;

public enum RoundingMode 
{
    TowardGreater,
    AwayFromZero,
    NearestAwayFromZero,
    NearestEven,
    NearestTowardZero,
    Truncation,
    TowardLesser,
    RoundForReround, // Do not use for now, until we get confirmation that it's not patented
    RoundMax
};
