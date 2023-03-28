namespace Otterkit;

public static class CompilerContext
{
    public static readonly List<string> FileNames = new();
    public static readonly List<Token> SourceTokens = new();
    public static readonly Compact<CallableSignature> CurrentCallable = new(1);
}
