namespace Otterkit.Library;

// FAQ 1: Why is this in its own file?
// It allows for more comments and documentation without poluting other files.

// As of this comment: Procedure pointer has an out parameter for the return item,
// and an ICOBOLType params for the using items. Needs further performance and usability tests.

public delegate bool ProcedurePointer(out ICOBOLType? returning, params ICOBOLType?[] parameters);
