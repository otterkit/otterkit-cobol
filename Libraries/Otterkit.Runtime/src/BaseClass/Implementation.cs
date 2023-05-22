namespace Otterkit.Runtime;

public class BaseFactory : IUniversal, IBaseFactoryInterface
{
    public static TActiveClass New<TActiveClass>() where TActiveClass : BaseObject, new()
    {
        var outObject = new TActiveClass();

        return outObject;
    }
}

public class BaseObject : IUniversal, IBaseObjectInterface
{
    public TActiveClass FactoryObject<TActiveClass>() where TActiveClass: BaseFactory, new()
    {
        var outFactory = new TActiveClass();

        return outFactory;
    }
}
