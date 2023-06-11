using System.Text;

namespace Otterkit;

public static partial class Tools
{
    public static void GenerateSwitchLookup(StringBuilder builder, params string[] strings)
    {
        Span<byte> span = stackalloc byte[64];

        span.Fill(0);

        for (int i = 0; i < strings.Length; i++)
        {
            ReadOnlySpan<char> name = strings[i].ToUpperInvariant();

            var length = Encoding.UTF8.GetBytes(name, span);

            builder.Append("            [");

            for (int j = 0; j < length; j++)
            {
                var _byte = span[j];

                if (_byte == 0) break;

                if (_byte > 96 && _byte < 123) span[j] ^= 0x20;

                if (j == length - 1)
                {
                    builder.Append($"{_byte}");
                    break;
                }

                builder.Append($"{_byte},");
            }

            builder.AppendLine($"] => {i},");

            span.Fill(0);
        }
    }
}
