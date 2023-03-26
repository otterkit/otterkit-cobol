namespace Otterkit; 

public sealed partial record Token
{
    public int Line;
    public int Column;
    public string Value;
    public int FileIndex;
    public TokenType Type;
    public TokenScope? Scope;
    public TokenContext? Context;

    public Token(string value, TokenType type, int line, int column)
    {
        this.Line = line;
        this.Column = column;
        this.Value = value;
        this.Type = type;
    }

    public Token(string value, TokenType type)
    {
        this.Value = value;
        this.Type = type;
    }

}
