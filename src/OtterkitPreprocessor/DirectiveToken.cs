namespace Otterkit; 

public sealed partial record DirectiveToken
{
    public int line;
    public string value;

    public DirectiveToken(string value, int line)
    {
        this.value = value;
        this.line = line;
    }
}
