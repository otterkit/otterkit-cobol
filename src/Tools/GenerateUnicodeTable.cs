using System.Runtime.InteropServices;
using System.Text;

namespace Otterkit;

[StructLayout(LayoutKind.Explicit)]
public unsafe struct u8Char
{
    [FieldOffset(0)]
    public fixed byte bytes[4];

    [FieldOffset(0)]
    public uint codepoint;
}

public static partial class Tools
{
    public static string FetchUnicodeData()
    {
        return File.ReadAllText("../Libraries/Otterkit.Native/nativelib/u8strings/data/UnicodeData.txt");
    }

    public static void GenerateUnicodeData()
    {
        var unicodeData = FetchUnicodeData();
        var lines = unicodeData.Split("\n");
        var unicodeTable = new List<string>(0x10FFFF);

        for (int i1 = 0; i1 < lines.Length; i1++)
        {
            if (lines[i1] is "" or " ") continue;

            var parts = lines[i1].Split(";");

            for (int i = 0; i < parts.Length; i++)
            {
                if (parts[i] is "" or " " or null) parts[i] = "0";
            }

            // String format:
            // codepoint, category, combining, bidi, uppercase, lowercase
            // { 0x0000, Cc, 0, BN, 0, 0 }
            var formatted = $"{{ 0x{parts[0]}, {parts[2]}, {parts[3]}, {parts[4]}, 0x{parts[12]}, 0x{parts[13]} }}";

            unicodeTable.Add(formatted);
        }

        StringBuilder builder = new();

        builder.AppendLine("// Path: src/Tools/GenerateUnicodeTable.cs");
        builder.AppendLine("// This file is generated by Otterkit, do not edit manually!");
        builder.AppendLine("#include \"u8types.h\"\n");

        builder.AppendLine("#ifndef U8DATABASE_H\n#define U8DATABASE_H");
        builder.AppendLine("static u8data const u8database[] = \n{");

        for (int i = 0; i < unicodeTable.Count; i++)
        {
            var line = unicodeTable[i];

            if (i == unicodeTable.Count - 1)
            {
                builder.AppendLine($"    {line}");
            }
            else
            {
                builder.AppendLine($"    {line},");
            }
        }

        builder.AppendLine("};");
        builder.AppendLine("#endif // U8DATABASE_H");

        File.WriteAllText("../Libraries/Otterkit.Native/nativelib/u8strings/u8database.h", builder.ToString());
    }

    public static void GenerateUnicodeLookup()
    {
        var unicodeData = FetchUnicodeData();
        var lines = unicodeData.Split("\n");
        var unicodeSwitch = new List<string>(0x10FFFF);

        for (int i = 0; i < lines.Length; i++)
        {
            if (lines[i] is "" or " ") continue;

            var parts = lines[i].Split(";");

            // String format:
            // case 0x0000: return 0; break;
            var formatted = $"case 0x{parts[0]}: return {i}; break;";

            unicodeSwitch.Add(formatted);
        }

        StringBuilder builder = new();

        builder.AppendLine("#include <stdint.h>");
        builder.AppendLine("#include \"common.h\"");
        builder.AppendLine("\n_export int32_t u8data_lookup(uint32_t codepoint)");
        builder.AppendLine("{");
        builder.AppendLine("    switch(codepoint)\n    {");

        for (int i = 0; i < unicodeSwitch.Count; i++)
        {
            var line = unicodeSwitch[i];

            if (i == unicodeSwitch.Count - 1)
            {
                builder.AppendLine($"        {line}");
            }
            else
            {
                builder.AppendLine($"        {line}");
            }
        }

        builder.AppendLine("    default: return -1;");
        builder.AppendLine("    }\n}");

        File.WriteAllText("../Libraries/Otterkit.Native/nativelib/u8strings/u8data_lookup.c", builder.ToString());
    }

    public static void GenerateFormatConversion(bool toCodepoint = true)
    {
        var unicodeData = FetchUnicodeData();
        var lines = unicodeData.Split("\n");
        var unicodeSwitch = new List<string>(0x10FFFF);

        Span<byte> bytes = stackalloc byte[4];

        for (int i = 0; i < lines.Length; i++)
        {
            if (lines[i] is "" or " ") continue;

            var parts = lines[i].Split(";");

            var codepoint = Convert.ToUInt32(parts[0], 16);

            var character = u8char_from_codepoint(codepoint);

            var formatted = "";

            if (toCodepoint)
            {
                formatted = $"case {character.codepoint}U: return 0x{parts[0]}; break;";
            }
            else
            {
                formatted = $"case 0x{parts[0]}: return {character.codepoint}U; break;";
            }

            unicodeSwitch.Add(formatted);
        }

        StringBuilder builder = new();

        if (toCodepoint)
        {
            builder.AppendLine("#include <stdint.h>");
            builder.AppendLine("#include \"common.h\"");
            builder.AppendLine("\n_export uint32_t u8char_to_codepoint_lookup(uint32_t encoded)");
            builder.AppendLine("{");
            builder.AppendLine("    switch(encoded)\n    {");
        }
        else
        {
            builder.AppendLine("\n_export uint32_t u8char_from_codepoint_lookup(uint32_t codepoint)");
            builder.AppendLine("{");
            builder.AppendLine("    switch(codepoint)\n    {");
        }

        for (int i = 0; i < unicodeSwitch.Count; i++)
        {
            var line = unicodeSwitch[i];

            if (i == unicodeSwitch.Count - 1)
            {
                builder.AppendLine($"        {line}");
            }
            else
            {
                builder.AppendLine($"        {line}");
            }
        }

        builder.AppendLine("    default: return -1;");
        builder.AppendLine("    }\n}");

        if (toCodepoint)
        {
            File.WriteAllText("../Libraries/Otterkit.Native/nativelib/u8strings/u8conversion.c", builder.ToString());
        }
        else
        {
            File.AppendAllText("../Libraries/Otterkit.Native/nativelib/u8strings/u8conversion.c", builder.ToString());
        }
    }

    public static u8Char u8char_from_codepoint(uint codepoint)
    {
        u8Char character = new();

        character.codepoint = codepoint;

        if (codepoint > 0x7F) 
        {
            character.codepoint = (codepoint & 0x000003F) | (codepoint & 0x0000FC0) << 2 | (codepoint & 0x003F000) << 4 | (codepoint & 0x01C0000) << 6;

            if (codepoint < 0x0000800)
            {
                character.codepoint |= 0x0000C080;
                return character;
            }

            if (codepoint < 0x0010000)
            {
                character.codepoint |= 0x00E08080;
                return character;
            }
            
            character.codepoint |= 0xF0808080;
            return character;
        }

        return character;
    }
}