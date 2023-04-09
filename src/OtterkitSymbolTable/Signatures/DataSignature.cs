namespace Otterkit;

public record DataSignature
{
    public DataSignature? Parent;
    public CurrentScope Section;
    public int LevelNumber;
    public string Identifier = "";
    public string Type = "";
    public string PictureString = "";
    public int PictureLength;
    public string ExternalName = "";
    public string DefaultValue = "";
    public UsageType UsageType;
    public string UsageContext = "";
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
    public bool HasUsage;
    public bool HasPicture;
    public bool HasValue;
}
