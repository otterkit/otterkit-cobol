using System.Text;
using Otterkit.Numerics;

namespace Otterkit.Runtime;

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
        Decimal128 result = 0;
        foreach (Numeric value in values)
        {
            result += (Decimal128)value;
        }

        for (int i = 0; i < dataItems.Length; i++)
        {
            Numeric variable = dataItems[i];

            if (result + (Decimal128)variable > (Decimal128)Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(result + (Decimal128)variable <= (Decimal128)Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.Bytes = ((Numeric)((Decimal128)variable + result)).Bytes;
        }
    }

    public static void ADD(Numeric[] values, Numeric to, Action OnSizeError, Action NotSizeError, params Numeric[] giving)
    {
        Decimal128 result = 0;
        foreach (Numeric value in values)
        {
            result += (Decimal128)value;
        }

        for (int i = 0; i < giving.Length; i++)
        {
            Numeric variable = giving[i];

            if (result + (Decimal128)to > (Decimal128)Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(result + (Decimal128)to <= (Decimal128)Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.Bytes = ((Numeric)(result + (Decimal128)to)).Bytes;
        }
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

    public static void COMPUTE(Decimal128 compute, Action OnSizeError, Action NotSizeError, params Numeric[] dataItems)
    {
        Decimal128 result = compute;

        for (int i = 0; i < dataItems.Length; i++)
        {
            Numeric variable = dataItems[i];

            if (result > (Decimal128)Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(result <= (Decimal128)Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.Bytes = ((Numeric)result).Bytes;
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
        for (int i = 0; i < dataItems.Length; i++)
        {
            Numeric variable = dataItems[i];

            if (variable / value > Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(variable / value <= Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.Bytes = (variable / value).Bytes;
        }
    }

    public static void DIVIDE(bool by, Numeric value, Numeric into, Action OnSizeError, Action NotSizeError, params Numeric[] giving)
    {   
        if (by)
        {
            for (int i = 0; i < giving.Length; i++)
            {
                Numeric variable = giving[i];

                if (value / into > Functions.HIGHEST_ALGEBRAIC(variable))
                    OnSizeError();

                if(value / into <= Functions.HIGHEST_ALGEBRAIC(variable))
                    NotSizeError();

                variable.Bytes = (value / into).Bytes;
            }
            return;
        }

        for (int i = 0; i < giving.Length; i++)
        {
            Numeric variable = giving[i];

            if (into / value > Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(into / value <= Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.Bytes = (into / value).Bytes;
        }
    }

    public static void DIVIDE(bool by, Numeric value, Numeric into, Numeric giving, Numeric remainder, Action OnSizeError, Action NotSizeError)
    {   
        if (by)
        {
            if(value / into > Functions.HIGHEST_ALGEBRAIC(giving))
                OnSizeError();

            if(value / into <= Functions.HIGHEST_ALGEBRAIC(giving))
                NotSizeError();

            giving.Bytes = (value / into).Bytes;
            remainder.Bytes = Functions.REM(value, into).Bytes;
            return;
        }

        if(into / value > Functions.HIGHEST_ALGEBRAIC(giving))
            OnSizeError();

        if(into / value <= Functions.HIGHEST_ALGEBRAIC(giving))
            NotSizeError();

        giving.Bytes = (into / value).Bytes;
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
        for (int i = 0; i < dataItems.Length; i++)
        {
            Numeric variable = dataItems[i];

            if (value * variable > Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(value * variable <= Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.Bytes = (value * variable).Bytes;
        }
    }

    public static void MULTIPLY(Numeric value, Numeric by, Action OnSizeError, Action NotSizeError, params Numeric[] giving)
    {
        for (int i = 0; i < giving.Length; i++)
        {
            Numeric variable = giving[i];

            if (value * by > Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(value * by <= Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.Bytes = (value * by).Bytes;
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
        Decimal128 result = 0;
        foreach (Numeric value in values)
        {
            result += (Decimal128)value;
        }

        for (int i = 0; i < dataItems.Length; i++)
        {
            Numeric variable = dataItems[i];
            
            if (result - (Decimal128)variable > (Decimal128)Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(result - (Decimal128)variable <= (Decimal128)Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.Bytes = ((Numeric)((Decimal128)variable - result)).Bytes;
        }
    }

    public static void SUBTRACT(Numeric[] values, Numeric from, Action OnSizeError, Action NotSizeError, params Numeric[] giving)
    {
        Decimal128 result = 0;
        foreach (Numeric value in values)
        {
            result += (Decimal128)value;
        }
        
        foreach (Numeric variable in giving)
        {
            if(result - (Decimal128)from > (Decimal128)Functions.HIGHEST_ALGEBRAIC(variable))
                OnSizeError();

            if(result - (Decimal128)from <= (Decimal128)Functions.HIGHEST_ALGEBRAIC(variable))
                NotSizeError();

            variable.Bytes = ((Numeric)((Decimal128)from - result)).Bytes;
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