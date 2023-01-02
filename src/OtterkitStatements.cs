using System.Text;

namespace OtterkitLibrary;

public static class Statements
{
    public static void ACCEPT(Alphanumeric dataItem, string from, string format = "")
    {
        // ACCEPT Statement devices: STANDARD-INPUT, COMMAND-LINE.
        Encoding encoding = Encoding.UTF8;
        Span<byte> bytes;

        switch (from)
        {
            case "STANDARD-INPUT":
                string? value = Console.ReadLine();
                bytes = encoding.GetBytes(value ?? " ");
                dataItem.Bytes = bytes;
                return;

            case "COMMAND-LINE":
                string CommandLine =  Environment.CommandLine;
                bytes = encoding.GetBytes(CommandLine);
                dataItem.Bytes = bytes;
                return;

            case "DATE":
                if (format == "YYYYMMDD")
                {
                    string YYYYMMDD = DateTime.Now.ToString("yyyyMMdd");
                    bytes = encoding.GetBytes(YYYYMMDD);
                    dataItem.Bytes = bytes;
                    return;
                }
                // Default DATE value:
                string YYMMDD = DateTime.Now.ToString("yyyyMMdd");
                bytes = encoding.GetBytes(YYMMDD);
                dataItem.Bytes = bytes;
                return;

            case "DAY":
                string day = DateTime.Now.Year.ToString() + DateTime.Now.DayOfYear.ToString();
                if (format == "YYYYDDD")
                {
                    bytes = encoding.GetBytes(day);
                    dataItem.Bytes = bytes;
                    return;
                }
                // Default DAY value:
                bytes = encoding.GetBytes(day);
                dataItem.Bytes = bytes[2..];
                return;

            case "DAY-OF-WEEK":
                string DayOfWeek = ((int)DateTime.Now.DayOfWeek).ToString();
                bytes = encoding.GetBytes(DayOfWeek);
                dataItem.Bytes = bytes;
                return;

            case "TIME":
                string time = DateTime.Now.ToString("HHmmssff");
                bytes = encoding.GetBytes(time);
                dataItem.Bytes = bytes;
                return;
        }

        string? _default = Console.ReadLine();
        bytes = encoding.GetBytes(_default ?? " ");
        dataItem.Bytes = bytes;
    }
    
    public static void ADD(Numeric[] values, Action OnSizeError, Action NotSizeError, params Numeric[] dataItems)
    {
        DecimalHolder result = "0"u8;
        foreach (Numeric value in values)
        {
            result += value.Bytes;
        }
        
        foreach (Numeric variable in dataItems)
        {
            if(result + variable.Bytes > Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                OnSizeError();

            if(result + variable.Bytes <= Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                NotSizeError();

            variable.Bytes = (variable.Bytes + result).Bytes;
        }
    }

    public static void ADD(Numeric[] values, Numeric to, Action OnSizeError, Action NotSizeError, params Numeric[] giving)
    {
        DecimalHolder result = "0"u8;
        foreach (Numeric value in values)
        {
            result += value.Bytes;
        }
        
        foreach (Numeric variable in giving)
        {
            if(result + to.Bytes > Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                OnSizeError();

            if(result + to.Bytes <= Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                NotSizeError();

            variable.Bytes = (result + to.Bytes).Bytes;
        }
    }

    public static OtterkitNativeMemory<byte> ALLOCATE(int bytes, BasedDataItem dataItem)
    {
        dataItem.Allocate(bytes, false);
        return dataItem.UnsafeMemory;
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

    public static void COMPUTE(ReadOnlySpan<byte> compute, Action OnSizeError, Action NotSizeError, params Numeric[] dataItems)
    {
        DecimalHolder result = DecimalMath.Arithmetic(compute);
        
        foreach (Numeric variable in dataItems)
        {
            if(result > Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                OnSizeError();

            if(result <= Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                NotSizeError();

            variable.Bytes = result.Bytes;
        }
    }
    
    public static void CONTINUE(double seconds)
    {
        if (seconds > 0)
            Thread.Sleep(Convert.ToInt32(seconds * 1000));

        return;
    }

    public static void DELETE()
    {
        // TODO: Implement DELETE
    }

    public static void DISPLAY(string upon, bool advancing, params string[] strings)
    {
        switch (upon)
        {
            case "STANDARD-OUTPUT":
                if (advancing)
                {
                    Console.WriteLine(String.Join(String.Empty, strings));
                    return;
                }
                Console.Write(String.Join(String.Empty, strings));
                return;

            case "STANDARD-ERROR":
                if (advancing)
                {
                    Console.Error.WriteLine(String.Join(String.Empty, strings));
                    return;
                }
                Console.Error.Write(String.Join(String.Empty, strings));
                return;
        }
        if (advancing)
        {
            Console.WriteLine(String.Join(String.Empty, strings));
            return;
        }
        Console.Write(String.Join(String.Empty, strings));
        return;
        
    }

    public static void DIVIDE(Numeric value, Action OnSizeError, Action NotSizeError, params Numeric[] dataItems)
    {
        foreach (Numeric variable in dataItems)
        {
            if(new DecimalHolder(variable.Bytes) / value.Bytes > Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                OnSizeError();

            if(new DecimalHolder(variable.Bytes) / value.Bytes <= Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                NotSizeError();

            variable.Bytes = (new DecimalHolder(variable.Bytes) / value.Bytes).Bytes;
        }
    }

    public static void DIVIDE(bool by, Numeric value, Numeric into, Action OnSizeError, Action NotSizeError, params Numeric[] giving)
    {   
        if (by)
        {
            foreach (Numeric variable in giving)
            {
                if(new DecimalHolder(value.Bytes) / into.Bytes > Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                    OnSizeError();

                if(new DecimalHolder(value.Bytes) / into.Bytes <= Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                    NotSizeError();

                variable.Bytes = (new DecimalHolder(value.Bytes) / into.Bytes).Bytes;
            }
            return;
        }

        foreach (Numeric variable in giving)
        {
            if(new DecimalHolder(into.Bytes) / value.Bytes > Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                OnSizeError();

            if(new DecimalHolder(into.Bytes) / value.Bytes <= Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                NotSizeError();

            variable.Bytes = (new DecimalHolder(into.Bytes) / value.Bytes).Bytes;
        }
    }

    public static void DIVIDE(bool by, Numeric value, Numeric into, Numeric giving, Numeric remainder, Action OnSizeError, Action NotSizeError)
    {   
        if (by)
        {
            if(new DecimalHolder(value.Bytes) / into.Bytes > Functions.HIGHEST_ALGEBRAIC(giving).Bytes)
                OnSizeError();

            if(new DecimalHolder(value.Bytes) / into.Bytes <= Functions.HIGHEST_ALGEBRAIC(giving).Bytes)
                NotSizeError();

            giving.Bytes = (new DecimalHolder(value.Bytes) / into.Bytes).Bytes;
            remainder.Bytes = Functions.REM(value, into).Bytes;
            return;
        }

        if(new DecimalHolder(into.Bytes) / value.Bytes > Functions.HIGHEST_ALGEBRAIC(giving).Bytes)
            OnSizeError();

        if(new DecimalHolder(into.Bytes) / value.Bytes <= Functions.HIGHEST_ALGEBRAIC(giving).Bytes)
            NotSizeError();

        giving.Bytes = (new DecimalHolder(into.Bytes) / value.Bytes).Bytes;
        remainder.Bytes = Functions.REM(value, into).Bytes;
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

    public static void FREE(params BasedDataItem[] dataItems)
    {
        foreach (BasedDataItem dataItem in dataItems)
        {
            dataItem.Free();
        }
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

    public static void IF()
    {
        // TODO: Implement IF
    }
    
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

    public static void MOVE()
    {
        // TODO: Implement MOVE
        // Move data between variables and assign values to variables
    }

    public static void MULTIPLY(Numeric value, Action OnSizeError, Action NotSizeError, params Numeric[] dataItems)
    {
        foreach (Numeric variable in dataItems)
        {
            if(new DecimalHolder(value.Bytes) * variable.Bytes > Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                OnSizeError();

            if(new DecimalHolder(value.Bytes) * variable.Bytes <= Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                NotSizeError();

            variable.Bytes = (new DecimalHolder(value.Bytes) * variable.Bytes).Bytes;
        }
    }

    public static void MULTIPLY(Numeric value, Numeric by, Action OnSizeError, Action NotSizeError, params Numeric[] giving)
    {   
        foreach (Numeric variable in giving)
        {
            if(new DecimalHolder(value.Bytes) * by.Bytes > Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                OnSizeError();

            if(new DecimalHolder(value.Bytes) * by.Bytes <= Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                NotSizeError();

            variable.Bytes = (new DecimalHolder(value.Bytes) * by.Bytes).Bytes;
        }
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

    public static void STOP(bool error, string status = "0")
    {
        if (error)
        {
            Console.ForegroundColor = ConsoleColor.Red;
            Console.Error.WriteLine("Error termination with status: {0}", status);
            Console.ResetColor();
            Environment.Exit(-1);
        }

        Console.ForegroundColor = ConsoleColor.Blue;
        Console.WriteLine("Normal termination with status: {0}", status);
        Console.ResetColor();
        Environment.Exit(1);
    }

    public static void STRING()
    {
        // TODO: Implement STRING
        // String method, extremely useful
        // Similar to String.Join()
    }

    public static void SUBTRACT(Numeric[] values, Action OnSizeError, Action NotSizeError, params Numeric[] dataItems)
    {
        DecimalHolder result = "0"u8;
        foreach (Numeric value in values)
        {
            result += value.Bytes;
        }
        
        foreach (Numeric variable in dataItems)
        {
            if(result - variable.Bytes > Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                OnSizeError();

            if(result - variable.Bytes <= Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                NotSizeError();

            variable.Bytes = (new DecimalHolder(variable.Bytes) - result).Bytes;
        }
    }

    public static void SUBTRACT(Numeric[] values, Numeric from, Action OnSizeError, Action NotSizeError, params Numeric[] giving)
    {
        DecimalHolder result = "0"u8;
        foreach (Numeric value in values)
        {
            result += value.Bytes;
        }
        
        foreach (Numeric variable in giving)
        {
            if(result - from.Bytes > Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                OnSizeError();

            if(result - from.Bytes <= Functions.HIGHEST_ALGEBRAIC(variable).Bytes)
                NotSizeError();

            variable.Bytes = (new DecimalHolder(from.Bytes) - result).Bytes;
        }
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