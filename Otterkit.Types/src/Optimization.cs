namespace Otterkit.Types;

public static class Optimization
{
    public static bool SpaceSeparatedSearch(ReadOnlySpan<char> words, ReadOnlySpan<char> word)
    {
        Span<char> buffer = stackalloc char[64];

        var length = words.Length;

        var parsed = 0;

        for (var index = 0; index < length; index++)
        {
            var character = words[index];

            if (character is ' ')
            {
                if (word.Equals(buffer[..parsed], StringComparison.OrdinalIgnoreCase)) return true;

                parsed = 0;

                continue;
            }

            if (index + 1 == length)
            {
                buffer[parsed] = character;

                return word.Equals(buffer[..(parsed + 1)], StringComparison.OrdinalIgnoreCase);
            }

            buffer[parsed] = character;

            parsed++;
        }

        return false;
    }
}
