using System.Buffers;
using System.IO.Pipelines;

namespace Otterkit;

public static partial class Preprocessor
{
    private static readonly ArrayPool<byte> ArrayPool = ArrayPool<byte>.Shared;
    internal static readonly List<Token> SourceTokens = new();
    private static int LineCount = 1;

    public static async Task<List<Token>> ReadSourceFile(string sourceFile)
    {
        using var sourceStream = File.OpenRead(sourceFile);
        
        PipeReader pipeReader = PipeReader.Create(sourceStream);

        var read = await pipeReader.ReadAsync();

        ReadOnlySequence<byte> buffer = read.Buffer;

        while (SourceLineExists(ref buffer, out ReadOnlySequence<byte> sequence))
        {
            TokenizeSequence(sequence);
            LineCount++;
        }

        pipeReader.AdvanceTo(buffer.End);

        LineCount = 1;
        SourceTokens.Add(new Token("EOF", TokenType.EOF, -5, -5){ context = TokenContext.IsEOF });
        return SourceTokens;
    }

    private static bool SourceLineExists(ref ReadOnlySequence<byte> buffer, out ReadOnlySequence<byte> line)
    {
        var newLine = buffer.PositionOf((byte)'\n');

        if (newLine is not null)
        {
            line = buffer.Slice(0, newLine.Value);
            buffer = buffer.Slice(buffer.GetPosition(1, newLine.Value));

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

    private static void TokenizeSequence(ReadOnlySequence<byte> sequence)
    {
        var sequenceLength = (int)sequence.Length;
        var sharedArray = ArrayPool.Rent(sequenceLength);
        
        try
        {
            sequence.CopyTo(sharedArray);
            Lexer.TokenizeLine(SourceTokens, sharedArray.AsSpan().Slice(0, sequenceLength), LineCount);
        }
        finally
        {
            ArrayPool.Return(sharedArray);
        }
    }
}
