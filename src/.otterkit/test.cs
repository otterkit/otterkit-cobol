using System.Text;
using OtterkitLibrary;
namespace OtterkitExport;

// PROGRAM-ID. _HELLO_WORLD.
public class _HELLO_WORLD
{
    private static readonly Encoding encoding = Encoding.UTF8;

    // WORKING-STORAGE SECTION.
    private static Numeric _WS_TEST = new("+123.456"u8, 0, 5, 5, new byte[12]);
    private static Alphanumeric _WS_NAME = new(" "u8, 0, 20, new byte[20]);

    // LOCAL-STORAGE SECTION.
    private readonly Constant _WS_CONST_LOCAL = new("Constant"u8);
    private National _WS_NAME_LOCAL = new(" "u8, 0, 20, new byte[20]);

    // PROCEDURE DIVISION.
    public void Procedure()
    {
        // PROCEDURE STATEMENTS.
        Statements.ACCEPT(_WS_NAME, "DATE", "YYYYMMDD");
        Statements.DISPLAY(" ", true, _WS_NAME.Display, String.Empty);
        Statements.STOP();

    }
}
