namespace Otterkit;

public static class analyzerScopes
{
    public static readonly List<string> statements = new()
    {
        "ACCEPT", "ADD", "ALLOCATE", "CALL", "CANCEL", "CLOSE", "COMMIT", "COMPUTE", "CONTINUE", "DELETE",
        "DISPLAY", "DIVIDE", "EVALUATE", "EXIT", "FREE", "GENERATE", "GO TO", "GOBACK", "IF", "INITIALIZE",
        "INITIATE", "INSPECT", "INVOKE", "MERGE", "MOVE", "MULTIPLY", "OPEN", "PERFORM", "RAISE", "READ",
        "RECEIVE", "RELEASE", "RESUME", "RETURN", "REWRITE", "ROLLBACK", "SEARCH", "SEND", "SET", "SORT",
        "START", "STOP", "STRING", "SUBTRACT", "SUPPRESS", "TERMINATE", "UNLOCK", "UNSTRING", "USE", "VALIDATE",
        "WRITE"
    };
    public static readonly List<string> allowScopeChange = new()
    {
        "FALSE", "ON-ERROR", "FALSE", "ON-EXCEPTION", "FALSE", "FALSE", "FALSE", "ON-ERROR", "FALSE", "ON-INVALID-OR-ON-EXCEPTION",
        "FALSE", "ON-ERROR", "TRUE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "TRUE", "FALSE",
        "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "ON-ERROR", "FALSE", "TRUE", "FALSE", "AT-END-OR-ON-INVALID",
        "ON-EXCEPTION", "FALSE", "FALSE", "AT-END", "ON-INVALID", "FALSE", "AT-END-OR-WHEN-CONDITION", "ON-EXCEPTION", "FALSE", "FALSE",
        "ON-INVALID", "FALSE", "ON-OVERFLOW", "ON-ERROR", "FALSE", "FALSE", "FALSE", "ON-OVERFLOW", "FALSE", "FALSE",
        "ON-INVALID-OR-AT-END-OF-PAGE"
    };
}


public static class OtterkitAnalyzer
{

    public static List<Token> Analyze(List<Token> tokenList)
    {
        string topLevelScope = "";
        List<string> previousScope = new();
        string currentScore = "";

        List<Token> analyzed = new();


        return analyzed;
    }
}