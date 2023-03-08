namespace Otterkit;

public static class Extensions
{
    public static bool IsEmpty(this HashSet<char> hashSet)
    {
        return hashSet.Count == 0;
    }

    public static bool ContainsAny(this HashSet<char> hashSet, params char[] characters)
    {
        foreach (var character in characters)
        {
            if (hashSet.Contains(character)) return true;
        }

        return false;
    }
}