using System.Text;

namespace OtterkitLibrary;

public sealed class External
{
    private static List<Memory<byte>> ExternalSpace = new();
    private static List<string> ExternalDataItems = new();
    private static List<int> ExternalItemsLength = new();
    private static List<string> ExternalDefaultValues = new();

    public static Memory<byte> Resolver(string dataItemName, int bytes, string defaultValue)
    {
        int IndexOfDataItem = ExternalDataItems.IndexOf(dataItemName);

        if (IndexOfDataItem > -1 && ExternalItemsLength[IndexOfDataItem] == bytes && ExternalDefaultValues[IndexOfDataItem] == defaultValue)
            return ExternalSpace[IndexOfDataItem];

        if (IndexOfDataItem == -1)
        {
            ExternalSpace.Add(new byte[bytes]);
            ExternalDataItems.Add(dataItemName);
            ExternalItemsLength.Add(bytes);
            ExternalDefaultValues.Add(defaultValue);
            Encoding.UTF8.GetBytes(defaultValue).CopyTo(ExternalSpace[ExternalSpace.Count].Span);

            return Resolver(dataItemName, bytes, defaultValue);
        }

        throw new ArgumentException("Name of external data item exists, but the byte length does not match");
    }
}