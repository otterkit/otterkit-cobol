namespace Otterkit.Runtime;

public interface IBaseFactory
{
    static abstract T New<T>() where T : BaseObject, new();
}

public interface IBaseObject
{
    T FactoryObject<T>() where T: BaseFactory, new();
}
