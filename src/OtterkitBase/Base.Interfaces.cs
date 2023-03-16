namespace OtterkitLibrary;

public interface IBaseFactoryInterface
{
    Base.BaseObject New();
}

public interface IBaseObjectInterface
{
    Base.BaseFactory FactoryObject();
}
