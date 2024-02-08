using Otterkit.Runtime;
using Otterkit.Native;

namespace Otterkit;

public static unsafe partial class Tools
{
    public static void TestCaseFolding(string toolsPath)
    {
        var data = Tools.FetchCaseFoldData(toolsPath);

        var _string = stackalloc byte[5] { 0x00, 0x00, 0x00, 0x00, 0x00 };

        var _string2 = stackalloc byte[5] { 0x00, 0x00, 0x00, 0x00, 0x00 };

        foreach (var casefold in data)
        {
            Unicode.FromCodepoint(casefold.Key, new(_string, 4));

            Unicode.FromCodepoint(casefold.Value, new(_string2, 4));

            var result = Unicode.Casefold(_string, 4);

            Console.WriteLine($"Testing: {casefold.Key:X4} -> {casefold.Value:X4}");

            if (result[0] != _string2[0] || result[1] != _string2[1] || result[2] != _string2[2] || result[3] != _string2[3])
            {
                Console.WriteLine($"Casefold failed: {casefold.Key:X4} -> {casefold.Value:X4}");
                Console.WriteLine($"Input: {_string[0]:X2} {_string[1]:X2} {_string[2]:X2} {_string[3]:X2}");
                Console.WriteLine($"Expected: {_string2[0]:X2} {_string2[1]:X2} {_string2[2]:X2} {_string2[3]:X2}");
                Console.WriteLine($"Got: {result[0]:X2} {result[1]:X2} {result[2]:X2} {result[3]:X2}");
                Console.WriteLine();

                Allocator.Dealloc(result);

                return;
            }

            Allocator.Dealloc(result);
            
            _string[0] = 0x00;
            _string[1] = 0x00;
            _string[2] = 0x00;
            _string[3] = 0x00;

            _string2[0] = 0x00;
            _string2[1] = 0x00;
            _string2[2] = 0x00;
            _string2[3] = 0x00;
        }

        Console.WriteLine("Casefold test passed!");
    }
}
