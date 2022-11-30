using System.Text;
using OtterkitLibrary;
namespace OtterkitExport;

// PROGRAM-ID. _HELLO_WORLD.
public class _HELLO_WORLD
{
    private static readonly Encoding encoding = Encoding.UTF8;

    // WORKING-STORAGE SECTION.
    private static Numeric _WS_TEST = new("+123.456"u8, 0, 5, 5, new byte[12]); 
    private static Numeric _WS_TEST_1 = new("123.456"u8, 0, 5, 5, new byte[11]); 
    private static Alphabetic _WS_TEST_2 = new("WOOOW"u8, 0, 10, new byte[10]); 
    private static National _WS_NAME = new(" "u8, 0, 20, new byte[20]); 
    private static OtterkitBoolean _WS_BOOL = new("01"u8, 0, 2, new byte[2]); 
    private static readonly Constant _WS_CONST = new("Constant"u8); 

    // LOCAL-STORAGE SECTION.
    private static readonly Constant _WS_CONST_LOCAL = new("Constant"u8); 
    private static National _WS_NAME_LOCAL = new(" "u8, 0, 20, new byte[20]); 

    // PROCEDURE DIVISION.
    public void Procedure()
    {
        // RESET LOCAL-STORAGE.
       _WS_NAME_LOCAL.Bytes = ""u8;

        // PROCEDURE STATEMENTS.
        Console.WriteLine("TEST");
    }
}
