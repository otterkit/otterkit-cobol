using System.Text;
using OtterkitLibrary;
namespace OtterkitExport;

// PROGRAM-ID. HELLO-WORLD.
public class _HELLO_WORLD
{
    private static readonly Encoding encoding = Encoding.UTF8;

    // WORKING-STORAGE SECTION.
    private static DataItem __WS_EXT_TEST = new(External.Resolver("WS-EXT-TEST", 10));
    private static Alphanumeric _WS_EXT_TEST = new(__WS_EXT_TEST.Memory, 0, 10);

    // LOCAL-STORAGE SECTION.

    // PROCEDURE DIVISION.
    public void Procedure()
    {
        // PROCEDURE STATEMENTS.
        Statements.ACCEPT(_WS_EXT_TEST, "DATE", "YYYYMMDD");
        Statements.DISPLAY(" ", true, _WS_EXT_TEST.Display, String.Empty);
        _EXT_TEST_PROGRAM _EXT_TEST_PROGRAM = new();
        Statements.CALL(() => _EXT_TEST_PROGRAM.Procedure());
        _EXT_TEST_PROGRAM_2 _EXT_TEST_PROGRAM_2 = new();
        Statements.CALL(() => _EXT_TEST_PROGRAM_2.Procedure());
        Statements.STOP();

    }
}
