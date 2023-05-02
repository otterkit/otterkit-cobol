namespace Otterkit.Workspaces;

public class Otterproj
{
    public string Name { get; set; }
    public string? PackageId { get; set; }
    public string? Description { get; set; }
    public string? Authors { get; set; }
    public string? License { get; set; }
    public Project Project { get; set; }
    public Build Build { get; set; }

    public Otterproj()
    {
        // Setup project defaults
        Name = "Program";
        Project = new();
        Build = new();
    }
}
