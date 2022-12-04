using System.Text;
using OtterkitLibrary;
namespace OtterkitExport;

// PROGRAM-ID. EXT-TEST-PROGRAM-2.
public class _EXT_TEST_PROGRAM_2
{
    private static readonly Encoding encoding = Encoding.UTF8;

    // WORKING-STORAGE SECTION.
    private static DataItem __WS_EXT_TEST = new(External.Resolver("WS-EXT-TEST", 5));
    private static Alphanumeric _WS_EXT_TEST = new(__WS_EXT_TEST.Memory, 0, 5);

    // LOCAL-STORAGE SECTION.

    // PROCEDURE DIVISION.
    public void Procedure()
    {
        // PROCEDURE STATEMENTS.
        Statements.DISPLAY(" ", true, _WS_EXT_TEST.Display, String.Empty);

    }
}
