using System.Text;
using OtterkitLibrary;
namespace OtterkitExport;

// PROGRAM-ID. EXT-TEST-PROGRAM.
public class _EXT_TEST_PROGRAM
{
    private static readonly Encoding encoding = Encoding.UTF8;

    // WORKING-STORAGE SECTION.
    private static DataItem __WS_EXT_TEST = new(External.Resolver("WS-EXT-TEST", 10));
    private static Numeric _WS_EXT_TEST = new(__WS_EXT_TEST.Memory, 0, 10, 0, false);

    // LOCAL-STORAGE SECTION.

    // PROCEDURE DIVISION.
    public void Procedure()
    {
        // PROCEDURE STATEMENTS.
        Statements.DISPLAY(" ", true, _WS_EXT_TEST.Display, String.Empty);

    }
}
