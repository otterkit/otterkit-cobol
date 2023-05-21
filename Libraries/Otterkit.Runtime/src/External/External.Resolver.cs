namespace Otterkit.Runtime;

public static partial class External
{
    private static readonly Dictionary<string, ExternalDataItem> ExternalDataItems = new();

    public static Memory<byte> Resolver(string dataItemName, int bytes)
    {
        bool AlreadyExists = ExternalDataItems.TryGetValue(dataItemName, out ExternalDataItem ExternalDataItem);

        if (AlreadyExists && ExternalDataItem.Length == bytes)
            return ExternalDataItem.ExternalMemory;

        if (!AlreadyExists)
        {
            ExternalDataItem NewExternalItem = new(new byte[bytes], bytes);
            ExternalDataItems.Add(dataItemName, NewExternalItem);

            return Resolver(dataItemName, bytes);
        }

        ExceptionRegistry.ActivateException("EC-EXTERNAL-FORMAT-CONFLICT"u8);

        // We should probably crash here...
        return Array.Empty<byte>();
    }
}
