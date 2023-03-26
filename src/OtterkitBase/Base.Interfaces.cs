namespace OtterkitLibrary;

public interface IBaseFactoryInterface
{
    Base.Object New();
}

public interface IBaseObjectInterface
{
    Base.Factory FactoryObject();
}
