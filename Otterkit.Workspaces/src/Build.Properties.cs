using Otterkit.Types;

namespace Otterkit.Workspaces;

public class Build
{
    public string Main { get; set; }
    public OutputType Output { get; set; }
    public SourceFormat Format { get; set; }
    public int Columns { get; set; }

    public Build()
    {
        // Setup project defaults
        Main = "main.cob";
        Output = OutputType.Application;
        Format = SourceFormat.Auto;
        Columns = 80;
    }
}
