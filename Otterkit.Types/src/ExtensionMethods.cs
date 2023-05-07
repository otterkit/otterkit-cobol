namespace Otterkit.Types;

public static class ExtensionMethods
{
    public static T AwaitResult<T>(this ValueTask<T> valueTask)
    {
        return valueTask.GetAwaiter().GetResult();
    }

    public static bool IsEmpty<T>(this HashSet<T> hashSet)
    {
        return hashSet.Count == 0;
    }

    public static bool IsOneOf(this char value, ReadOnlySpan<char> span)
    {
        foreach (var item in span)
        {
            if (value == item) return true;
        }

        return false;
    }

    public static bool ContainsAny<T>(this HashSet<T> hashSet, ReadOnlySpan<T> span)
    {
        foreach (var item in span)
        {
            if (hashSet.Contains(item)) return true;
        }

        return false;
    }
}
