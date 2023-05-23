using System.Buffers;
using System.IO.Pipelines;
using System.Text;
using Otterkit.Types;

namespace Otterkit.Tokenizers;

public static partial class Tokenizer
{
    private static readonly ArrayPool<byte> ArrayPool = ArrayPool<byte>.Shared;

    private static async ValueTask<List<Token>> ReadSourceFile(string sourceFile)
    {
        using var stream = File.OpenRead(sourceFile);

        var reader = PipeReader.Create(stream);

        var read = await reader.ReadAsync();

        while (!read.IsCompleted)
        {
            read = await reader.ReadAsync();

            reader.AdvanceTo(read.Buffer.Start, read.Buffer.End);
        }

        var buffer = read.Buffer;

        var lineIndex = 1;

        var tokens = CompilerContext.SourceTokens;

        while (SourceLineExists(ref buffer, out ReadOnlySequence<byte> line))
        {
            var length = (int)line.Length;
            var array = ArrayPool.Rent(length + 1);

            line.CopyTo(array);

            ProcessLinebreak(array, length);

            TokenizeLine(tokens, array.AsSpan(0, length + 1), lineIndex);

            ArrayPool.Return(array);

            lineIndex++;
        }

        reader.AdvanceTo(buffer.End);

        tokens.Add(new Token("EOF", TokenType.EOF, -5, -5) { Context = TokenContext.IsEOF });

        return tokens;
    }

    private static async ValueTask<List<Token>> ReadCopybook(string copybookFile)
    {
        using var stream = File.OpenRead(copybookFile);

        var reader = PipeReader.Create(stream);

        var read = await reader.ReadAsync();

        while (!read.IsCompleted)
        {
            read = await reader.ReadAsync();

            reader.AdvanceTo(read.Buffer.Start, read.Buffer.End);
        }

        var buffer = read.Buffer;

        var lineIndex = 1;

        List<Token> tokens = new();

        while (SourceLineExists(ref buffer, out ReadOnlySequence<byte> line))
        {
            var length = (int)line.Length;
            var array = ArrayPool.Rent(length + 1);

            line.CopyTo(array);

            ProcessLinebreak(array, length);

            TokenizeLine(tokens, array.AsSpan(0, length + 1), lineIndex);

            ArrayPool.Return(array);

            lineIndex++;
        }

        reader.AdvanceTo(buffer.End);

        return tokens;
    }

    private static bool SourceLineExists(ref ReadOnlySequence<byte> buffer, out ReadOnlySequence<byte> line)
    {
        // UTF-8 LINE FEED (LF) has value 10 (0x0A).
        var lineFeed = buffer.PositionOf<byte>(10);

        if (lineFeed is not null)
        {
            var length = lineFeed.Value;

            line = buffer.Slice(0, length);

            buffer = buffer.Slice(buffer.GetPosition(1, length));

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

    private static void ProcessLinebreak(Span<byte> line, int length)
    {
        if (length == 0) return;
        
        // We want to remove the CARRIAGE RETURN (CR) if present.
        var carriageReturn = line[length - 1];

        // We'll replace it with a space.
        // UTF-8 SPACE has value 32 (0x20).

        // UTF-8 CARRIAGE RETURN (CR) has value 13 (0x0D).
        if (carriageReturn == 13) line[length - 1] = 32;

        line[length] = 32;
    }
}
