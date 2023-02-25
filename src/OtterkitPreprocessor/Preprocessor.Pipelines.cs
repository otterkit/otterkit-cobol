using System.Buffers;
using System.IO.Pipelines;

namespace Otterkit;

public static partial class Preprocessor
{
    internal static readonly List<Token> SourceTokens = new();
    private static readonly ArrayPool<byte> ArrayPool = ArrayPool<byte>.Shared;
    private static int LineCount = 1;

    public static async Task<List<Token>> ReadSourceFile(string sourceFile)
    {
        using var sourceStream = File.OpenRead(sourceFile);
        
        var pipeReader = PipeReader.Create(sourceStream);

        var readAsync = await pipeReader.ReadAsync();

        var buffer = readAsync.Buffer;

        while (SourceLineExists(ref buffer, out ReadOnlySequence<byte> line))
        {
            var lineLength = (int)line.Length;
            var sharedArray = ArrayPool.Rent(lineLength);
        
            line.CopyTo(sharedArray);
        
            Lexer.TokenizeLine(SourceTokens, sharedArray.AsSpan().Slice(0, lineLength), LineCount);
        
            ArrayPool.Return(sharedArray);
            LineCount++;
        }

        pipeReader.AdvanceTo(buffer.End);

        LineCount = 1;
        SourceTokens.Add(new Token("EOF", TokenType.EOF, -5, -5){ context = TokenContext.IsEOF });

        return SourceTokens;
    }

    public static async Task<List<Token>> ReadCopybook(string copybookFile)
    {
        using var copybookStream = File.OpenRead(copybookFile);
        
        var pipeReader = PipeReader.Create(copybookStream);

        var readAsync = await pipeReader.ReadAsync();

        var buffer = readAsync.Buffer;

        List<Token> copybookTokens = new();

        while (SourceLineExists(ref buffer, out ReadOnlySequence<byte> line))
        {
            var lineLength = (int)line.Length;
            var sharedArray = ArrayPool.Rent(lineLength);
        
            line.CopyTo(sharedArray);
        
            Lexer.TokenizeLine(copybookTokens, sharedArray.AsSpan().Slice(0, lineLength), LineCount);
        
            ArrayPool.Return(sharedArray);
            LineCount++;
        }

        pipeReader.AdvanceTo(buffer.End);

        LineCount = 1;

        return copybookTokens;
    }

    private static bool SourceLineExists(ref ReadOnlySequence<byte> buffer, out ReadOnlySequence<byte> line)
    {
        var positionOfNewLine = buffer.PositionOf((byte)'\n');

        if (positionOfNewLine is not null)
        {
            line = buffer.Slice(0, positionOfNewLine.Value);
            buffer = buffer.Slice(buffer.GetPosition(1, positionOfNewLine.Value));

            return true;
        }

        if (!buffer.IsEmpty)
        {
            line = buffer.Slice(0, buffer.Length - 1);
            buffer = buffer.Slice(buffer.End);

            return true;
        }

        line = ReadOnlySequence<byte>.Empty;
        
        return false;
    }
}
