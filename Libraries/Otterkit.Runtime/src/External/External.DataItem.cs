namespace Otterkit.Runtime;

readonly struct ExternalDataItem
{
    public readonly Memory<byte> ExternalMemory;
    public readonly string ExternalDefault;
    public readonly int Length;

    public ExternalDataItem(Memory<byte> memory, int length)
    {
        this.ExternalMemory = memory;
        this.Length = length;
        this.ExternalDefault = string.Empty;
    }
}