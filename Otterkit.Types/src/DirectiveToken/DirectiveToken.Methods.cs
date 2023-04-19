using System.Text;

namespace Otterkit.Types;

public sealed partial record DirectiveToken
{
    public override sealed string ToString()
    {
        StringBuilder stringBuilder = new();
        stringBuilder.Append("DirectiveToken");
        stringBuilder.Append(" { ");
        
        stringBuilder.Append($"Ln: {line, -6},");
        stringBuilder.Append($"Value: {value}");

        stringBuilder.Append(" }");
        return stringBuilder.ToString();
    }
}
