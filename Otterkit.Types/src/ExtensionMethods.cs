namespace Otterkit.Types;

public static class ExtensionMethods
{
    public static bool IsEmpty<T>(this HashSet<T> hashSet)
    {
        return hashSet.Count == 0;
    }

    public static bool ContainsAny<T>(this HashSet<T> hashSet, params T[] TArray)
    {
        foreach (var TItem in TArray)
        {
            if (hashSet.Contains(TItem)) return true;
        }

        return false;
    }
}
