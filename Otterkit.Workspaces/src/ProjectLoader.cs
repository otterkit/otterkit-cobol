using System.Buffers;
using System.IO.Pipelines;
using System.Text.Json;
using Otterkit.Types;

namespace Otterkit.Workspaces;

public static class ProjectLoader
{
    private static readonly ArrayPool<byte> ArrayPool = ArrayPool<byte>.Shared;

    public static async ValueTask<Option<Otterproj>> ReadOtterproj(string otterprojPath)
    {
        using var otterprojStream = File.OpenRead(otterprojPath);

        var pipeReader = PipeReader.Create(otterprojStream);

        ReadOnlySequence<byte> buffer;

        while (true)
        {
            var readAsync = await pipeReader.ReadAsync();

            if (readAsync.IsCompleted)
            {
                buffer = readAsync.Buffer;
                break;
            }

            pipeReader.AdvanceTo(readAsync.Buffer.Start, readAsync.Buffer.End);
        }

        var length = (int)buffer.Length;
        var sharedArray = ArrayPool.Rent(length);

        buffer.CopyTo(sharedArray);

        Option<Otterproj> otterproj;

        try
        {
            otterproj = JsonSerializer.Deserialize<Otterproj>(sharedArray.AsSpan(0, length), ProjectJsonContext.Default.Otterproj);
        }
        catch (JsonException exception)
        {
            var message = exception.Message;
            ErrorHandler.Build(ErrorType.Compilation, ConsoleColor.Red, 9555, """
                Failed to load .otterproj project file.
                """)
            .WithStartingException($"""
                {message.AsSpan(0, message.IndexOf('.') + 1)}
                """)
            .CloseError();

            otterproj = null;
        }
        finally
        {
            ArrayPool.Return(sharedArray);
        }

        return otterproj;
    }

    public static bool TryLoadProject(Option<Otterproj> otterproj)
    {
        if (!otterproj.Exists) return false;

        var unwrap = otterproj.Unwrap();

        var build = unwrap.Build;

        CompilerOptions.Initialize(unwrap.Name, build.Main);

        CompilerOptions.SetOptions(build.Output, build.Format, build.Columns);

        return true;
    }
}
