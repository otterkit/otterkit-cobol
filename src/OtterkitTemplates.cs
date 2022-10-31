using OtterkitLibrary;
namespace OtterkitModule;

// PROGRAM-ID. HELLO-WORLD.
public class _HELLO_WORLD
{
    // WORKING-STORAGE SECTION
    // 01 WS-CONST CONSTANT AS 5.
    static string _WS_ACCEPT = "";

    static Decimal128 _WS_RESULT = Decimal128.Zero;

    // PROCEDURE DIVISION
    public static void procedure()
    {
        Statements.COMPUTE(() => 1 + 2 * 4 / (Decimal128)123, () => 
        {
            Statements.DISPLAY("", true, _WS_RESULT.ToString());
        }, () => 
        {
            Statements.DISPLAY("", true, _WS_RESULT.ToString());
        }, _WS_RESULT, _WS_RESULT);
        // STOP RUN.
        Statements.STOP(false);
    }
}