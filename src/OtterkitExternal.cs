namespace OtterkitLibrary;

public sealed class External
{
    private static List<Memory<byte>> ExternalSpace = new();
    private static List<string> ExternalDataItems = new();
    private static List<int> ExternalItemsLength = new();

    public static Memory<byte> Resolver(string dataItemName, int bytes)
    {
        int IndexOfDataItem = ExternalDataItems.IndexOf(dataItemName);

        if (IndexOfDataItem > -1 && ExternalItemsLength[IndexOfDataItem] == bytes)
            return ExternalSpace[IndexOfDataItem];

        if (IndexOfDataItem == -1)
        {
            ExternalSpace.Add(new byte[bytes]);
            ExternalDataItems.Add(dataItemName);
            ExternalItemsLength.Add(bytes);

            return Resolver(dataItemName, bytes);
        }

        throw new ArgumentException("Name of external data item exists, but the byte length does not match");
    }
}