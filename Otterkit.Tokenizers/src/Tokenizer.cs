using Otterkit.Types;

namespace Otterkit.Tokenizers;

public static partial class Tokenizer
{
    internal static DirectiveType LastDirective;
    internal static bool HasDetectedSourceFormat;
    internal static string Workspace => Directory.GetCurrentDirectory(); 

    public static List<Token> Tokenize(string entryPoint)
    {
        if (!File.Exists(entryPoint))
        {
            ErrorHandler
            .Build(ErrorType.Compilation, ConsoleColor.Red, 950, """
                Entry point file not found.
                """)
            .WithStartingError($"""
                Unable to find the entry point file specified: {entryPoint}
                """)
            .CloseError();

            Environment.Exit(1);
        }

        var relativeEntryPoint = Path.GetRelativePath(Workspace, entryPoint);

        CompilerOptions.Main = relativeEntryPoint;

        var allSourceFiles = Directory.EnumerateFiles(Workspace, "*.cob", SearchOption.AllDirectories)
            .Select(static path => Path.GetRelativePath(Workspace, path));
        
        CompilerContext.FileNames.Add(relativeEntryPoint);

        var tokens = ReadSourceFile(relativeEntryPoint).Result;

        foreach (var file in allSourceFiles)
        {
            if (file.Equals(relativeEntryPoint)) continue;

            CompilerContext.FileNames.Add(file);

            tokens = ReadSourceFile(file).Result;
        }

        PreprocessCopybooks(CompilerContext.SourceTokens);

        return CompilerContext.SourceTokens;
    }
}
