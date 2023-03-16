namespace OtterkitLibrary;

public static class Base
{
    private static readonly BaseFactory FactoryInstance = new();

    protected interface IBaseFactory
    {
        protected static BaseObject New()
        {
            BaseObject outObject = new BaseObject();

            return outObject;
        }
    }

    public class BaseFactory : ICOBOLRoot, IBaseFactory, IBaseFactoryInterface
    {
        public BaseObject New() => IBaseFactory.New();
    }

    protected interface IBaseObject
    {
        protected static BaseFactory FactoryObject()
        {
            BaseFactory outFactory = FactoryInstance;

            return outFactory;
        }
    }

    public class BaseObject : ICOBOLRoot, IBaseObject, IBaseObjectInterface
    {
        public BaseFactory FactoryObject() => IBaseObject.FactoryObject();
    }
}
