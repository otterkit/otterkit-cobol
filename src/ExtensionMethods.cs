namespace Otterkit;

public static class Extensions
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

    public static T PeekBehind<T>(this Stack<T> stack)
    {
        var currentT = stack.Pop();
        var previousT = stack.Peek();

        stack.Push(currentT);

        return previousT;
    }
}