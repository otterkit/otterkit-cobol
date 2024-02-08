using Otterkit.Types;

namespace Otterkit.Workspaces;

public class Project
{
    public string Sdk { get; set; }
    public string Target { get; set; }
    public string Standard { get; set; }


    public Project()
    {
        // Setup project defaults
        Sdk = "main.cob";
        Target = "net7.0";
        Standard = "2023";
    }
}
