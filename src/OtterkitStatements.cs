using System.Threading;

namespace OtterkitLibrary;

public static class Statements
{
    public static string ACCEPT(string from, string format = "")
    {
        // ACCEPT Statement devices: STANDARD-INPUT, COMMAND-LINE.
        string? value;
        switch (from)
        {
            case "STANDARD-INPUT":
                value = Console.ReadLine();
                return value == null ? " " : value;

            case "COMMAND-LINE":
                return Environment.CommandLine;

            case "DATE":
                if (format == "YYYYMMDD")
                {
                    return DateTime.Now.ToString("yyyyMMdd");
                }
                // Default DATE value:
                return DateTime.Now.ToString("yyMMdd");;

            case "DAY":
                value = DateTime.Now.Year.ToString() + DateTime.Now.DayOfYear.ToString();
                if (format == "YYYYDDD")
                    return value;
                // Default DAY value:
                return value.Substring(2);

            case "DAY-OF-WEEK":
                return ((int)DateTime.Now.DayOfWeek).ToString();

            case "TIME":
                return DateTime.Now.ToString("HHmmssff");
        }

        value = Console.ReadLine();
        return value == null ? " " : value;
    }
    
    public static void ADD(Decimal128[] values, Action OnSizeError, Action NotSizeError, params Numeric[] dataItems)
    {
        Decimal128 result = 0;
        foreach (Decimal128 value in values)
        {
            result += value;
        }
        
        foreach (Numeric variable in dataItems)
        {
            if(result + variable.dataItem > Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(result + variable.dataItem <= Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.dataItem += result;
        }
    }

    public static void ADD(Decimal128[] values, Decimal128 to, Action OnSizeError, Action NotSizeError, params Numeric[] giving)
    {
        Decimal128 result = 0;
        foreach (Decimal128 value in values)
        {
            result += value;
        }
        
        foreach (Numeric variable in giving)
        {
            if(result + to > Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(result + to <= Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.dataItem = result + to;
        }
    }

    public static void ALLOCATE()
    {
        DISPLAY("STANDARD-ERROR", true, "Error, Unsupported: Otterkit COBOL does not support manual memory management");
    }

    public static void CALL()
    {
        // TODO: Implement CALL
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

    public static void COMPUTE(Func<Decimal128> compute, Action OnSizeError, Action NotSizeError, params Numeric[] dataItems)
    {
        // Usage example:
        // Will result in a "SIZE ERROR"
        // Statements.COMPUTE(
        //     () => 1000, 
        //     () => Statements.DISPLAY("", true, "SIZE ERROR"), 
        //     () => Statements.DISPLAY("", true, "NOT ERROR"), 
        //     new Numeric(999, 3, 0, false)
        // );

        Decimal128 result = compute();
        
        foreach (Numeric variable in dataItems)
        {
            if(result > Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(result <= Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.dataItem = result;
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

    public static void DIVIDE(Decimal128 value, Action OnSizeError, Action NotSizeError, params Numeric[] dataItems)
    {
        foreach (Numeric variable in dataItems)
        {
            if(variable.dataItem / value > Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(variable.dataItem / value <= Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.dataItem /= value;
        }
    }

    public static void DIVIDE(bool by, Decimal128 value, Decimal128 into, Action OnSizeError, Action NotSizeError, params Numeric[] giving)
    {   
        if (by)
        {
            foreach (Numeric variable in giving)
            {
                if(value / into > Functions.HIGHEST_ALGEBRAIC(variable))
                    OnSizeError();

                if(value / into <= Functions.HIGHEST_ALGEBRAIC(variable))
                    NotSizeError();

                variable.dataItem = value / into;
            }
            return;
        }

        foreach (Numeric variable in giving)
        {
            if(into / value > Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(into / value <= Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.dataItem = into / value;
        }
    }

    public static void DIVIDE(bool by, Decimal128 value, Decimal128 into, Numeric giving, Numeric remainder, Action OnSizeError, Action NotSizeError)
    {   
        if (by)
        {
            if(value / into > Functions.HIGHEST_ALGEBRAIC(giving))
                OnSizeError();

            if(value / into <= Functions.HIGHEST_ALGEBRAIC(giving))
                NotSizeError();

            giving.dataItem = value / into;
            remainder.dataItem = Functions.REM(value, into);
            return;
        }

        if(into / value > Functions.HIGHEST_ALGEBRAIC(giving))
            OnSizeError();

        if(into / value <= Functions.HIGHEST_ALGEBRAIC(giving))
            NotSizeError();

        giving.dataItem = into / value;
        remainder.dataItem = Functions.REM(value, into);
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
        DISPLAY("STANDARD-ERROR", true, "Error, Unsupported: Otterkit COBOL does not support manual memory management");
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

    public static void MULTIPLY(Decimal128 value, Action OnSizeError, Action NotSizeError, params Numeric[] dataItems)
    {
        foreach (Numeric variable in dataItems)
        {
            if(value * variable.dataItem > Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(value * variable.dataItem <= Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.dataItem *= value;
        }
    }

    public static void MULTIPLY(Decimal128 value, Decimal128 by, Action OnSizeError, Action NotSizeError, params Numeric[] giving)
    {   
        foreach (Numeric variable in giving)
        {
            if(value * by > Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(value * by <= Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.dataItem = value * by;
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

    public static void SUBTRACT(Decimal128[] values, Action OnSizeError, Action NotSizeError, params Numeric[] dataItems)
    {
        Decimal128 result = 0;
        foreach (Decimal128 value in values)
        {
            result += value;
        }
        
        foreach (Numeric variable in dataItems)
        {
            if(result - variable.dataItem > Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(result - variable.dataItem <= Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.dataItem -= result;
        }
    }

    public static void SUBTRACT(Decimal128[] values, Decimal128 from, Action OnSizeError, Action NotSizeError, params Numeric[] giving)
    {
        Decimal128 result = 0;
        foreach (Decimal128 value in values)
        {
            result += value;
        }
        
        foreach (Numeric variable in giving)
        {
            if(result - from > Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(result - from <= Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.dataItem = from - result;
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