using System.Buffers;
using System.IO.Pipelines;

namespace Otterkit;

public static partial class Preprocessor
{
    private static readonly ArrayPool<byte> ArrayPool = ArrayPool<byte>.Shared;

    public static async ValueTask<List<Token>> ReadSourceFile(string sourceFile)
    {
        using var sourceStream = File.OpenRead(sourceFile);
        
        var pipeReader = PipeReader.Create(sourceStream);

        var readAsync = await pipeReader.ReadAsync();

        var buffer = readAsync.Buffer;

        Lexer.LineIndex = 1;

        while (SourceLineExists(ref buffer, out ReadOnlySequence<byte> line))
        {
            var lineLength = (int)line.Length;
            var sharedArray = ArrayPool.Rent(lineLength);
        
            line.CopyTo(sharedArray);
       
            Lexer.TokenizeLine(Options.SourceTokens, sharedArray.AsSpan().Slice(0, lineLength));
        
            ArrayPool.Return(sharedArray);
            
            Lexer.LineIndex++;
        }

        pipeReader.AdvanceTo(buffer.End);

        Options.SourceTokens.Add(new Token("EOF", TokenType.EOF, -5, -5){ Context = TokenContext.IsEOF });

        return Options.SourceTokens;
    }

    public static async ValueTask<List<Token>> ReadCopybook(string copybookFile)
    {
        using var copybookStream = File.OpenRead(copybookFile);
        
        var pipeReader = PipeReader.Create(copybookStream);

        var readAsync = await pipeReader.ReadAsync();

        var buffer = readAsync.Buffer;

        Lexer.LineIndex = 1;

        List<Token> copybookTokens = new();

        while (SourceLineExists(ref buffer, out ReadOnlySequence<byte> line))
        {
            var lineLength = (int)line.Length;
            var sharedArray = ArrayPool.Rent(lineLength);
        
            line.CopyTo(sharedArray);
        
            Lexer.TokenizeLine(copybookTokens, sharedArray.AsSpan().Slice(0, lineLength));
        
            ArrayPool.Return(sharedArray);

            Lexer.LineIndex++;
        }

        pipeReader.AdvanceTo(buffer.End);

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
            line = buffer.Slice(0, buffer.Length);
            buffer = buffer.Slice(buffer.End);

            return true;
        }

        line = ReadOnlySequence<byte>.Empty;
        
        return false;
    }
}
