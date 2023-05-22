namespace Otterkit.Runtime;

public interface IBaseFactoryInterface
{
    static abstract TActiveClass New<TActiveClass>() where TActiveClass : BaseObject, new();
}

public interface IBaseObjectInterface
{
    TActiveClass FactoryObject<TActiveClass>() where TActiveClass: BaseFactory, new();
}
