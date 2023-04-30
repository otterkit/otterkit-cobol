namespace Otterkit.Workspaces;

public class CobolProject
{
    public string Name { get; set; }
    public string Standard { get; set; }
    public string? PackageId { get; set; }
    public string? Description { get; set; }
    public string? Authors { get; set; }
    public BuildOptions BuildOptions { get; set; }
    public string? License { get; set; }

    public CobolProject()
    {
        // Setup project defaults
        Name = "Program";
        Standard = "2023";
        BuildOptions = new();
    }
}
