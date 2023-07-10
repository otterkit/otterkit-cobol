using System.Globalization;
using System.Runtime.InteropServices;
using System.Text;
using Otterkit.Runtime;

namespace Otterkit;

[StructLayout(LayoutKind.Explicit)]
public unsafe struct u8Char
{
    [FieldOffset(0)]
    public fixed byte Bytes[4];

    [FieldOffset(0)]
    public uint Codepoint;

    public static implicit operator u8Char(byte[] bytes)
    {
        var u8char = new u8Char();

        if (bytes.Length is 1)
        {
            u8char.Bytes[0] = bytes[0];
            u8char.Bytes[1] = 0;
            u8char.Bytes[2] = 0;
            u8char.Bytes[3] = 0;
        }
        else if (bytes.Length is 2)
        {
            u8char.Bytes[0] = bytes[0];
            u8char.Bytes[1] = bytes[1];
            u8char.Bytes[2] = 0;
            u8char.Bytes[3] = 0;
        }
        else if (bytes.Length is 3)
        {
            u8char.Bytes[0] = bytes[0];
            u8char.Bytes[1] = bytes[1];
            u8char.Bytes[2] = bytes[2];
            u8char.Bytes[3] = 0;
        }
        else if (bytes.Length is 4)
        {
            u8char.Bytes[0] = bytes[0];
            u8char.Bytes[1] = bytes[1];
            u8char.Bytes[2] = bytes[2];
            u8char.Bytes[3] = bytes[3];
        }

        return u8char;
    }
}

public static partial class Tools
{
    public static Dictionary<uint, (uint Uppercase, uint Casefolded)> FetchUnicodeData(string toolsPath)
    {
        Directory.SetCurrentDirectory(toolsPath);

        var lines = File.ReadAllText("UnicodeData.txt").Split("\n");

        var casefoldLines = File.ReadAllText("CaseFolding.txt").Split("\n");

        var unicodeData = new Dictionary<uint, (uint Uppercase, uint Casefolded)>();

        for (uint i = 0x0000; i <= 0x10FFFF; i++)
        {
            unicodeData.Add(i, (i, i));
        }

        var casefoldData = File.CreateText("u8CaseFolding.h");

        casefoldData.AutoFlush = true;

        Span<byte> bytes = stackalloc byte[4];
        Span<byte> casefoldBytes = stackalloc byte[4];

        var differences = new HashSet<int>();

        foreach (var casefold in casefoldLines)
        {
            if (casefold.Length < 3) continue;

            if (casefold[0] is '#') continue;

            var data = casefold.Split(";");

            if (data[1][1..] is "C" or "S")
            {
                var codepoint = uint.Parse(data[0], NumberStyles.HexNumber);

                if (codepoint > 0x10FFFF) continue;

                var casefolded = uint.Parse(data[2][1..], NumberStyles.HexNumber);

                if (casefolded > 0x10FFFF) continue;

                unicodeData[codepoint] = (unicodeData[codepoint].Uppercase, casefolded);

                Unicode.FromCodepoint(codepoint, bytes);

                Unicode.FromCodepoint(casefolded, casefoldBytes);

                differences.Add((int)casefolded - (int)codepoint);

                var bytesDiff = $"{casefoldBytes[0]-bytes[0]:X2} {casefoldBytes[1]-bytes[1]:X2} {casefoldBytes[2]-bytes[2]:X2} {casefoldBytes[3]-bytes[3]:X2}";

                casefoldData.WriteLine($"// 0x{codepoint:X4}; {data[1][1..]}; 0x{casefolded:X4}; DIFF[{bytesDiff}] ;{data[3]}");
            }
            else
            {
                var codepoint = uint.Parse(data[0], NumberStyles.HexNumber);

                unicodeData[codepoint] = (unicodeData[codepoint].Uppercase, codepoint);
            }
        }

        Console.WriteLine($"Differences: {differences.Count}");

        var uppercaseData = File.CreateText("UpperCase.txt");

        uppercaseData.AutoFlush = true;

        Span<byte> uppercase = stackalloc byte[4];

        foreach (var line in lines)
        {
            var data = line.Split(";");

            if (data.Length < 14) continue;

            var codepoint = uint.Parse(data[0], NumberStyles.HexNumber);

            if (codepoint > 0x10FFFF) continue;

            if (data[12] is not "")
            {
                Unicode.FromCodepoint(codepoint, bytes);

                Unicode.FromCodepoint(uint.Parse(data[12], NumberStyles.HexNumber), uppercase);

                uppercaseData.WriteLine($"{bytes[0]:X2} {bytes[1]:X2} {bytes[2]:X2} {bytes[3]:X2}; U; {uppercase[0]:X2} {uppercase[1]:X2} {uppercase[2]:X2} {uppercase[3]:X2}; # {data[1]}");
            }

            bytes.Clear();
            uppercase.Clear();

            // unicodeData[codepoint] = (uppercase, unicodeData[codepoint].Casefolded);
        }

        return unicodeData;
    }

    public static Dictionary<uint, uint> FetchCaseFoldData(string toolsPath)
    {
        Directory.SetCurrentDirectory(toolsPath);

        var casefoldLines = File.ReadAllText("CaseFolding.txt").Split("\n");

        var casefoldData = new Dictionary<uint, uint>();

        foreach (var casefold in casefoldLines)
        {
            if (casefold.Length < 3) continue;

            if (casefold[0] is '#') continue;

            var data = casefold.Split(";");

            if (data[1][1..] is "C" or "S")
            {
                var codepoint = uint.Parse(data[0], NumberStyles.HexNumber);

                var casefolded = uint.Parse(data[2][1..], NumberStyles.HexNumber);

                casefoldData[codepoint] = casefolded;
            }
        }

        return casefoldData;
    }

    public static void GenerateBasicMultilingualPlane(string toolsPath)
    {
        var unicodeData = FetchUnicodeData(toolsPath);

        StringBuilder builder = new();

        builder.AppendLine("// Tool: src/Tools/GenerateUnicodeTable.cs");
        builder.AppendLine("// This file is generated by Otterkit, do not edit manually!");
        builder.AppendLine("// Generated at: " + DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss"));
        builder.AppendLine("// Unicode version: 15.0.0");
        builder.AppendLine();

        builder.AppendLine("#include \"u8unicode.h\"\n");

        builder.AppendLine("// Unicode Codepoint Database (U+0000 ... U+FFFF)");
        builder.AppendLine("const UnicodeTableEntry u8Unicode[0x10000] =");
        builder.AppendLine("{");

        for (uint index = 0x0000; index <= 0xFFFF; index++)
        {
            var line = $"{{ 0x{index:X4}, 0x{(unicodeData[index].Uppercase):X4}, 0x{(unicodeData[index].Casefolded):X4} }}";

            AppendCodepoint(builder, line, index, index == 0xFFFF);
        }

        builder.AppendLine("};");

        if (!Directory.Exists("Unicode"))
        {
            Directory.CreateDirectory("Unicode");
        }

        File.WriteAllText("Unicode/u8unicode.c", builder.ToString());
    }

    private static void AppendCodepoint(StringBuilder builder, string codepoint, uint index, bool isEnd = false)
    {
        if (isEnd)
        {
            builder.AppendLine($"    {codepoint} // [{index}]");
        }
        else
        {
            builder.AppendLine($"    {codepoint}, // [{index}]");
        }
    }
}
