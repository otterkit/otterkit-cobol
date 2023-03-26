namespace OtterkitLibrary;

public static class Base
{
    public class Factory : IUniversal, IBaseFactoryInterface
    {
        public static TActiveClass New<TActiveClass>()
            where TActiveClass : Object, new()
        {
            var outObject = new TActiveClass();

            return outObject;
        }
    }

    public class Object : IUniversal, IBaseObjectInterface
    {
        public TActiveClass FactoryObject<TActiveClass>()
            where TActiveClass: Factory, new()
        {
            var outFactory = new TActiveClass();

            return outFactory;
        }
    }
}
