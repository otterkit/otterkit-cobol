namespace OtterkitLibrary;

public static class Base
{
    private static readonly Factory FactoryInstance = new();

    public class Factory : ICOBOLUniversal, IBaseFactoryInterface
    {
        public Object New()
        {
            Object outObject = new Object();

            return outObject;
        }
    }

    public class Object : ICOBOLUniversal, IBaseObjectInterface
    {
        public Factory FactoryObject()
        {
            Factory outFactory = FactoryInstance;

            return outFactory;
        }
    }
}
