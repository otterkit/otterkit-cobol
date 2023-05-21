namespace Otterkit.Runtime;

public enum Severity
{ 
    //why an enum instead of a bool? So that other programmers can understand what this actually means
    Fatal,
    NonFatal,
    Other //exceptions with labels greater than three do not seem to be set as either fata or nonfatal
}
