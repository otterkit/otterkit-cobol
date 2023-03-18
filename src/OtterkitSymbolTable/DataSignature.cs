namespace Otterkit;

public record DataSignature
{
    public CurrentScope Section;
    public string Parent = string.Empty;
    public DataSignature[] Fields = Array.Empty<DataSignature>();
    public int LevelNumber;
    public string Identifier = string.Empty;
    public string Type = string.Empty;
    public string PictureString = string.Empty;
    public int PictureLength;
    public string ExternalName = string.Empty;
    public string DefaultValue = string.Empty;
    public UsageType UsageType;
    public string UsageContext = string.Empty;
    public bool IsExternal;
    public bool IsElementary;
    public bool IsGroup;
    public bool IsConstant;
    public bool IsGlobal;
    public bool IsBased;
    public bool IsTypedef;
    public bool IsAnyLength;
    public bool IsDynamicLength;
    public bool IsRedefines;
    public bool IsRenames;
    public bool IsBlank;
    public bool IsAligned;
    public bool IsConstantRecord;
    public bool IsProperty;
    public bool HasPicture;
    public bool HasValue;
}
