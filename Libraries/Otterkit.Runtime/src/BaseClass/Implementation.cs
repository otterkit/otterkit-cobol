namespace Otterkit.Runtime;

public class BaseFactory : IUniversal, IBaseFactory
{
    public static T New<T>() where T : BaseObject, new()
    {
        var outObject = new T();

        return outObject;
    }
}

public class BaseObject : IUniversal, IBaseObject
{
    public T FactoryObject<T>() where T: BaseFactory, new()
    {
        var outFactory = new T();

        return outFactory;
    }
}
