namespace Otterkit.Types;

public static class Optimization
{
    public static bool SpaceSeparatedSearch(ReadOnlySpan<char> words, ReadOnlySpan<char> word)
    {
        Span<char> parsed = stackalloc char[64];

        var index = 0;

        foreach (var character in words)
        {
            if (character is ' ')
            {
                if (word.Equals(parsed[..index], StringComparison.OrdinalIgnoreCase)) return true;

                index = 0;

                continue;
            }

            parsed[index] = character;

            index++;
        }

        return false;
    }
}
