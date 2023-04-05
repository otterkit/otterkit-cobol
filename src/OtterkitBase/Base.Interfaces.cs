namespace Otterkit.Library;

public interface IBaseFactoryInterface
{
    static abstract TActiveClass New<TActiveClass>()
         where TActiveClass : Base.Object, new();
}

public interface IBaseObjectInterface
{
    TActiveClass FactoryObject<TActiveClass>()
        where TActiveClass: Base.Factory, new();
}
