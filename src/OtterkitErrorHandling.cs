namespace Otterkit;

public static class ErrorHandler
{
    public static class Compiler
    {
        public static void Report(string error)
        {
            Console.ForegroundColor = ConsoleColor.Red;
            Console.WriteLine(error);
            Console.ResetColor();
        }
    }

    public static class Parser
    {
        public static void Report(Token token, string error, params string[] expected)
        {
            Console.ForegroundColor = ConsoleColor.Red;
            if (error == "choice")
            {
                Choice(token, expected);
                Console.ResetColor();
                Environment.Exit(1);
            }

            if (error == "expected")
            {
                Expected(token, expected);
                Console.ResetColor();
                Environment.Exit(1);
            }

            General(token, expected);
            Console.ResetColor();
            Environment.Exit(1);
        }

        static void General(Token token, string[] expected)
        {
            Console.Error.WriteLine("Otterkit parsing error: Line {0}, Column {1}", token.line, token.column);
            Console.Error.WriteLine("Unexpected token: {0}", expected[0]);
            Console.WriteLine();
        }
        
        static void Choice(Token token, string[] expected)
        {
            Console.Error.WriteLine("Otterkit parsing error: Line {0}, Column {1}", token.line, token.column);
            Console.Error.WriteLine("Unexpected token: Expected {0} or {1}, instead of {2}", expected[0], expected[1], token.value);
            Console.WriteLine();
        }

        static void Expected(Token token, string[] expected)
        {
            Console.Error.WriteLine("Otterkit parsing error: Line {0}, Column {1}", token.line, token.column);
            Console.Error.WriteLine("Unexpected token: Expected {0}, instead of {1}", expected[0], token.value);
            Console.WriteLine();
        }

    }
    
}