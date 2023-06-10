using System.Numerics;

namespace Otterkit.Numerics;

public interface IStandardDecimal<TSelf> :
    IComparable,
    IEquatable<TSelf>,
    IComparable<TSelf>,
    IMinMaxValue<TSelf>,
    IIncrementOperators<TSelf>,
    IDecrementOperators<TSelf>,
    IAdditiveIdentity<TSelf, TSelf>,
    IUnaryPlusOperators<TSelf, TSelf>,
    IUnaryNegationOperators<TSelf, TSelf>,
    IMultiplicativeIdentity<TSelf, TSelf>,
    IEqualityOperators<TSelf, TSelf, bool>,
    IModulusOperators<TSelf, TSelf, TSelf>,
    IAdditionOperators<TSelf, TSelf, TSelf>,
    IDivisionOperators<TSelf, TSelf, TSelf>,
    IMultiplyOperators<TSelf, TSelf, TSelf>,
    IComparisonOperators<TSelf, TSelf, bool>,
    ISubtractionOperators<TSelf, TSelf, TSelf>

    where TSelf : IStandardDecimal<TSelf>
{
    
}
