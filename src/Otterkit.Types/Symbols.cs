namespace Otterkit.Types;

public static class Symbols
{
    public static readonly GlobalPrototypes SourceUnits = new();

    public static TPrototype GetPrototype<TPrototype>(string sourceUnitName)
        where TPrototype : AbstractPrototype
    {
        return SourceUnits.GetPrototype<TPrototype>(sourceUnitName);
    }

    public static bool TryAddName(string sourceUnitName, AbstractPrototype prototype)
    {
        return SourceUnits.TryAddPrototype(sourceUnitName, prototype);
    }
}
