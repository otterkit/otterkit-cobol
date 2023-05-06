namespace Otterkit.Types;

public readonly struct ElementaryType
{
    public readonly Classes DataClass;
    public readonly Categories DataCategory;

    public ElementaryType(Classes dataClass, Categories dataCategory)
    {
        DataClass = dataClass;
        DataCategory = dataCategory;
    }
}
