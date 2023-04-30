using Otterkit.Types;

namespace Otterkit.Workspaces;

public class BuildOptions
{
    public string Main { get; set; }
    public int Columns { get; set; }
    public OutputType Output { get; set; }
    public SourceFormat Format { get; set; }
    public BuildType Build { get; set; }

    public BuildOptions()
    {
        // Setup project defaults
        Output = OutputType.Application;
        Main = "main.cob";
        Columns = 80;
        Format = SourceFormat.Auto;
        Build = BuildType.BuildOnly;
    }
}
