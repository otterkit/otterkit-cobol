using System.Text;

namespace OtterkitLibrary;

struct ExternalDataItem
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

public sealed class External
{
    private static Dictionary<string, ExternalDataItem> ExternalMetadata = new();

    public static Memory<byte> Resolver(string dataItemName, int bytes)
    {
        ExternalDataItem ExternalDataItem;
        bool AlreadyExists = ExternalMetadata.TryGetValue(dataItemName, out ExternalDataItem);

        if (AlreadyExists && ExternalDataItem.Length == bytes)
            return ExternalDataItem.ExternalMemory;

        if (!AlreadyExists)
        {
            ExternalDataItem NewExternalItem = new(new byte[bytes], bytes);
            ExternalMetadata.Add(dataItemName, NewExternalItem);

            return Resolver(dataItemName, bytes);
        }

        throw new EcExternalFormatConflict();
    }

}