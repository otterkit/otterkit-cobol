using System.Buffers;
using System.Text;
using Otterkit.Numerics;
using Otterkit.Native;

namespace Otterkit.Runtime;

public static class Statement
{
    private static readonly ArrayPool<byte> Buffer = ArrayPool<byte>.Shared;
    private static readonly Encoding Encoding = Encoding.UTF8;

    public static void ACCEPT(Alphanumeric variable, ReadOnlySpan<char> from, ReadOnlySpan<char> format)
    {
        // ACCEPT Statement devices: STANDARD-INPUT, COMMAND-LINE.
        Span<char> chars = stackalloc char[16];

        var bytes = Buffer.Rent(4096);

        switch (from)
        {
            case "STANDARD-INPUT":
                u8Console.ReadLine(bytes);

                variable.Bytes = bytes;
                break;

            case "COMMAND-LINE":
                ReadOnlySpan<char> args = Environment.CommandLine;

                Encoding.GetBytes(args, bytes);

                variable.Bytes = bytes;
                break;

            case "DATE":
                if (format == "YYYYMMDD")
                {
                    DateTime.Now.TryFormat(chars, out _, "yyyyMMdd");

                    Encoding.GetBytes(chars, bytes);

                    variable.Bytes = bytes;
                    break;
                }

                // Default DATE value:
                DateTime.Now.TryFormat(chars, out _, "yyMMdd");

                Encoding.GetBytes(chars, bytes);

                variable.Bytes = bytes;
                break;

            case "DAY":
                DateTime.Now.Year.TryFormat(chars, out _, "yyyy");
                DateTime.Now.DayOfYear.TryFormat(chars[4..], out _, "ddd");

                if (format == "YYYYDDD")
                {
                    Encoding.GetBytes(chars, bytes);

                    variable.Bytes = bytes;
                    break;
                }

                // Default DAY value:
                Encoding.GetBytes(chars, bytes);

                variable.Bytes = bytes[2..];
                break;

            case "DAY-OF-WEEK":
                ((int)DateTime.Now.DayOfWeek).TryFormat(chars, out _, "D");

                Encoding.GetBytes(chars, bytes);

                variable.Bytes = bytes;
                break;

            case "TIME":
                DateTime.Now.TryFormat(chars, out _, "HHmmssff");

                Encoding.GetBytes(chars, bytes);

                variable.Bytes = bytes;
                break;

            default:
                u8Console.ReadLine(bytes);

                variable.Bytes = bytes;
                break;
        }

        Buffer.Return(bytes);
    }

    public static bool ADD(Span<Numeric> values, Span<Numeric> returning)
    {
        var result = Decimal128.Zero;

        foreach (Numeric value in values)
        {
            result += (Decimal128)value;
        }

        var buffer = Buffer.Rent(45);

        var error = false;

        for (int i = 0; i < returning.Length; i++)
        {
            var variable = returning[i];

            Decimal128 d128Var = variable;

            var compute = result + d128Var;

            using (var temporary = Functions.HIGHEST_ALGEBRAIC(variable))
            {
                if (compute > temporary) error = true;

                if (compute <= temporary) error = false;
            }

            var length = compute.AsSpan(buffer);

            variable.Bytes = buffer.AsSpan().Slice(0, length);
        }

        Buffer.Return(buffer);

        return error;
    }

    public static bool ADD(Span<Numeric> values, Numeric to, Span<Numeric> giving)
    {
        var result = Decimal128.Zero;

        foreach (Numeric value in values)
        {
            result += (Decimal128)value;
        }

        var buffer = Buffer.Rent(45);

        var error = false;

        for (int i = 0; i < giving.Length; i++)
        {
            var variable = giving[i];

            Decimal128 d128Var = variable;

            var compute = result + to;

            using (var temporary = Functions.HIGHEST_ALGEBRAIC(variable))
            {
                if (compute > temporary) error = true;

                if (compute <= temporary) error = false;
            }

            var length = compute.AsSpan(buffer);

            variable.Bytes = buffer.AsSpan().Slice(0, length);
        }

        Buffer.Return(buffer);

        return error;
    }

    public static void ALLOCATE()
    {
        // TODO: Reimplement based data item allocation
    }

    public static void CALL(Action procedure)
    {
        procedure();
    }

    public static void CANCEL()
    {
        // TODO: Implement CANCEL
    }

    public static void CLOSE()
    {
        // TODO: Implement CLOSE
    }

    public static void COMMIT()
    {
        // TODO: Implement COMMIT
    }

    public static bool COMPUTE(Decimal128 compute, Span<Numeric> returning)
    {
        var buffer = Buffer.Rent(45);

        var error = false;

        foreach (var variable in returning)
        {
            Decimal128 d128Var = variable;

            using (var temporary = Functions.HIGHEST_ALGEBRAIC(variable))
            {
                if (compute > temporary) error = true;

                if (compute <= temporary) error = false;
            }

            var length = compute.AsSpan(buffer);

            variable.Bytes = buffer.AsSpan().Slice(0, length);
        }

        Buffer.Return(buffer);

        return error;
    }

    public static void CONTINUE(int milliseconds)
    {
        if (milliseconds <= 0) return;

        Thread.Sleep(milliseconds);
    }

    public static void DELETE()
    {
        // TODO: Implement DELETE
    }

    public static void DISPLAY(ReadOnlySpan<byte> bytes, string upon, bool advancing)
    {
        switch (upon)
        {
            case "STANDARD-OUTPUT":
                u8Console.Write(bytes);
                break;

            case "STANDARD-ERROR":
                u8Console.Write(u8Text.Red, bytes);
                break;

            default:
                u8Console.Write(bytes);
                break;
        }

        if (advancing) u8Console.Write("\n"u8);
    }

    public static bool DIVIDE(Numeric value, Span<Numeric> returning)
    {
        var buffer = Buffer.Rent(45);

        var error = false;

        foreach (var variable in returning)
        {
            Decimal128 d128Var = variable;

            Decimal128 d128Value = value;

            var compute = d128Var / d128Value;

            using (var temporary = Functions.HIGHEST_ALGEBRAIC(variable))
            {
                if (compute > temporary) error = true;

                if (compute <= temporary) error = false;
            }

            var length = compute.AsSpan(buffer);

            variable.Bytes = buffer.AsSpan(0, length);
        }

        Buffer.Return(buffer);

        return error;
    }

    public static bool DIVIDE(bool by, Numeric value, Numeric into, Span<Numeric> giving)
    {
        var buffer = Buffer.Rent(45);

        var error = false;

        if (by)
        {
            Decimal128 d128Into = into;

            Decimal128 d128Value = value;

            var compute = d128Value / d128Into;

            foreach (var variable in giving)
            {
                using (var temporary = Functions.HIGHEST_ALGEBRAIC(variable))
                {
                    if (compute > temporary) error = true;

                    if (compute <= temporary) error = false;
                }

                var length = compute.AsSpan(buffer);

                variable.Bytes = buffer.AsSpan(0, length);
            }
        }
        else
        {
            Decimal128 d128Into = into;

            Decimal128 d128Value = value;

            var compute = d128Into / d128Value;

            foreach (var variable in giving)
            {
                using (var temporary = Functions.HIGHEST_ALGEBRAIC(variable))
                {
                    if (compute > temporary) error = true;

                    if (compute <= temporary) error = false;
                }

                var length = compute.AsSpan(buffer);

                variable.Bytes = buffer.AsSpan(0, length);
            }
        }

        Buffer.Return(buffer);

        return error;
    }

    public static bool DIVIDE(bool by, Numeric value, Numeric into, Numeric giving, Numeric remainder)
    {
        var buffer = Buffer.Rent(45);

        var error = false;

        Decimal128 d128Into = into;

        Decimal128 d128Value = value;

        Decimal128 compute;

        if (by)
        {
            compute = d128Value / d128Into;
        }
        else
        {
            compute = d128Into / d128Value;
        }

        using (var temporary = Functions.HIGHEST_ALGEBRAIC(giving))
        {
            if (compute > temporary) error = true;

            if (compute <= temporary) error = false;
        }

        var length = compute.AsSpan(buffer);

        giving.Bytes = buffer.AsSpan(0, length);

        remainder.Bytes = Functions.REM(value, into).Bytes;

        Buffer.Return(buffer);

        return error;
    }

    public static void EVALUATE()
    {
        // TODO: Implement EVALUTE
        // This is the COBOL switch statement, might need templating
    }

    public static void EXIT()
    {
        // TODO: Implement EXIT
    }

    public static void FREE()
    {
        // TODO: Reimplement based data item allocation
    }

    public static void GENERATE()
    {
        // TODO: Implement GENERATE
    }

    public static void GOTO()
    {
        // TODO: Implement GO TO
        // The standard recommends avoiding the use of GO TO
        // Compiler warnings when GO TOs are used?
        // Might have to use C# gotos
    }

    public static void GOBACK()
    {
        // TODO: Implement GOBACK
        // Sounds similar to GO TO, but it's actually really different
        // GOBACK is equivalent to C#'s return statement with additional features
    }

    // Implemented in the COBOL code generator
    public static void IF() { }

    public static void INITIALIZE()
    {
        // TODO: Implement INITIALIZE
        // Assigns values to variables
        // Basically, initializes them with a value
    }

    public static void INITIATE()
    {
        // TODO: Implement INITIARE
    }

    public static void INSPECT()
    {
        // TODO: Implement INSPECT
        // String inspect, awesome feature
    }

    public static void INVOKE()
    {
        // TODO: Implement INVOKE
        // COBOL OOP method call
    }

    public static void MERGE()
    {
        // TODO: Implement MERGE
    }

    public static void MOVE<TFrom, TTo>(TFrom variable, TTo to)
        where TFrom : ICOBOLType
        where TTo : ICOBOLType
    {
        to.Bytes = variable.Bytes;
    }

    public static bool MULTIPLY(Numeric value, Span<Numeric> returning)
    {
        var buffer = Buffer.Rent(45);

        var error = false;

        for (int i = 0; i < returning.Length; i++)
        {
            var variable = returning[i];

            Decimal128 d128Var = variable;

            Decimal128 d128Value = value;

            var compute = d128Var * d128Value;

            using (var temporary = Functions.HIGHEST_ALGEBRAIC(variable))
            {
                if (compute > temporary) error = true;

                if (compute <= temporary) error = false;
            }

            var length = compute.AsSpan(buffer);

            variable.Bytes = buffer.AsSpan(0, length);
        }

        Buffer.Return(buffer);

        return error;
    }

    public static bool MULTIPLY(Numeric value, Numeric by, Span<Numeric> giving)
    {
        var buffer = Buffer.Rent(45);

        var error = false;

        Decimal128 d128By = by;

        Decimal128 d128Value = value;

        var compute = d128Value * d128By;

        foreach (var variable in giving)
        {
            using (var temporary = Functions.HIGHEST_ALGEBRAIC(variable))
            {
                if (compute > temporary) error = true;

                if (compute <= temporary) error = false;
            }

            var length = compute.AsSpan(buffer);

            variable.Bytes = buffer.AsSpan(0, length);
        }

        Buffer.Return(buffer);

        return error;
    }

    public static void OPEN()
    {
        // TODO: Implement OPEN
    }

    public static void PERFORM()
    {
        // TODO: Implement PERFORM
        // This one is a bit complex
        //
        // PERFORM can be equivalent to:
        // C# function/method call,
        // Multiple methods calls,
        // A C# while loop,
        // A C# for loop,
        // An until loop,
        // and a loop with exception checking
    }

    public static void RAISE()
    {
        // TODO: Implement RAISE
        // Equivalent to C# throw Exception()
        // Need to implement COBOL exception handling first
    }

    public static void READ()
    {
        // TODO: Implement READ
    }

    public static void RECEIVE()
    {
        // TODO: Implement RECEIVE
        // New feature, async messaging
    }

    public static void RELEASE()
    {
        // TODO: Implement RELEASE
    }

    public static void RESUME()
    {
        // TODO: Implement RESUME
    }

    public static void RETURN()
    {
        // TODO: Implement RETURN
    }

    public static void REWRITE()
    {
        // TODO: Implement REWRITE
    }

    public static void ROLLBACK()
    {
        // TODO: Implement ROLLBACK
    }

    public static void SEARCH()
    {
        // TODO: Implement SEARCH
    }

    public static void SEND()
    {
        // TODO: Implement SEND
        // New feature, async messaging
    }

    public static void SET()
    {
        // TODO: Implement SET
    }

    public static void SORT()
    {
        // TODO: Implement SORT
    }

    public static void START()
    {
        // TODO: Implement START
    }

    public static void STOP()
    {
        Environment.Exit(0);
    }

    public static void STOP(bool error, ReadOnlySpan<byte> status)
    {
        if (error)
        {
            u8Console.Write(u8Text.Red, "Error termination with status: "u8);

            u8Console.WriteLine(status);

            Environment.Exit(1);
        }

        u8Console.Write(u8Text.Green, "Normal termination with status: "u8);

        u8Console.WriteLine(status);

        Environment.Exit(0);
    }

    public static void STRING()
    {
        // TODO: Implement STRING
        // String method, extremely useful
        // Similar to String.Join()
    }

    public static bool SUBTRACT(Span<Numeric> values, Span<Numeric> returning)
    {
        var result = Decimal128.Zero;

        var error = false;

        foreach (Numeric value in values) result += value;

        var buffer = Buffer.Rent(45);

        foreach (var variable in returning)
        {
            var compute = result - variable;

            using (var temporary = Functions.HIGHEST_ALGEBRAIC(variable))
            {
                if (compute > temporary) error = true;

                if (compute <= temporary) error = false;
            }

            var length = compute.AsSpan(buffer);

            variable.Bytes = buffer.AsSpan(0, length);
        }

        Buffer.Return(buffer);

        return error;
    }

    public static bool SUBTRACT(Span<Numeric> values, Numeric from, Span<Numeric> giving)
    {
        var result = Decimal128.Zero;

        var error = false;

        foreach (Numeric value in values) result += value;

        var buffer = Buffer.Rent(45);

        var compute = result - from;

        foreach (var variable in giving)
        {
            using (var temporary = Functions.HIGHEST_ALGEBRAIC(variable))
            {
                if (compute > temporary) error = true;

                if (compute <= temporary) error = false;
            }

            var length = compute.AsSpan(buffer);

            variable.Bytes = buffer.AsSpan(0, length);
        }

        Buffer.Return(buffer);

        return error;
    }

    public static void SUPPRESS()
    {
        // TODO: Implement SUPPRESS
    }

    public static void TERMINATE()
    {
        // TODO: Implement TERMINATE
    }

    public static void UNLOCK()
    {
        // TODO: Implement UNLOCK
    }

    public static void UNSTRING()
    {
        // TODO: Implement UNSTRING
        // String method, extremely useful
        // Similar to String.Split()
    }

    public static void USE()
    {
        // TODO: Implement USE
    }

    public static void VALIDATE()
    {
        // TODO: Implement VALIDATE
    }

    public static void WRITE()
    {
        // TODO: Implement WRITE
    }
}