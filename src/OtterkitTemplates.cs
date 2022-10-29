using OtterkitLibrary;
namespace OtterkitModule;

// PROGRAM-ID. HELLO-WORLD.
public class _HELLO_WORLD
{
    // WORKING-STORAGE SECTION
    // 01 WS-CONST CONSTANT AS 5.
    static string _WS_ACCEPT = "";

    // PROCEDURE DIVISION
    public static void procedure()
    {
        // DISPLAY WITH NO ADVANCING.
        Statements.DISPLAY("", false, "Test no advancing: ");
        // ACCEPT WS-ACCEPT FROM COMMAND-LINE.
        _WS_ACCEPT = Statements.ACCEPT("COMMAND-LINE");
        // DISPLAY WS-ACCEPT.
        Statements.DISPLAY("", true, "Result is: ", _WS_ACCEPT);
        // STOP RUN.
        Statements.STOP(false);
    }
}