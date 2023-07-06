namespace Otterkit.Runtime;

public readonly struct CobolException
{
    public readonly bool Active;
    public readonly bool Checked; //yes, you can turn checking for default exceptions off.
    public readonly Severity Severity;

    public CobolException(bool isActivated, bool isChecked, Severity severity)
    {
        Active = isActivated;
        Checked = isChecked;
        Severity = severity;
    }
}

public class RuntimeException : Exception
{
    public RuntimeException(string message) : base(message)
    {

    }
}