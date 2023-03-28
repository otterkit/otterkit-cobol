namespace Otterkit; 

public sealed partial record Token
{
    public int Line;
    public int Column;
    public string Value;
    public string Mangled;
    public int FileIndex;
    public TokenType Type;
    public TokenScope? Scope;
    public TokenContext? Context;

    public Token(string value, TokenType type, int line, int column)
    {
        Line = line;
        Column = column;
        Value = value;
        Type = type;

        Mangled = string.Empty;
    }

    public Token(string value, TokenType type)
    {
        Value = value;
        Type = type;

        Mangled = string.Empty;
    }

}
