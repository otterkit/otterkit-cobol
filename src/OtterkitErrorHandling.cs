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
        public static void PrettyError(string fileName, Token token)
        {
            string line = File.ReadLines(fileName).Skip(token.line - 1).Take(token.line).First();
            string error = new String(' ', line.Length - token.value.Length);
            error = error.Insert(token.column, new String('^', token.value.Length));
            
            Console.WriteLine($"{" ",5}|");
            Console.WriteLine($"{token.line,4} | {line.TrimStart()}");
            Console.Write($"{" ",5}|");

            Console.ForegroundColor = ConsoleColor.Yellow;
            Console.WriteLine($"{error}\n");
            Console.ResetColor();

            Environment.Exit(1);
        }
        public static void Report(string fileName, Token token, string error, params string[] expected)
        {
            Console.ForegroundColor = ConsoleColor.Red;
            if (error == "choice")
            {
                Choice(token, expected, fileName);
                Console.ResetColor();
                return;
            }

            if (error == "expected")
            {
                Expected(token, expected, fileName);
                Console.ResetColor();
                return;
            }

            General(token, expected, fileName);
            Console.ResetColor();
        }

        private static void General(Token token, string[] expected, string fileName)
        {
            Console.Error.WriteLine("Otterkit parsing error: In {0} at Line {1}, Column {2}", fileName, token.line, token.column);
            Console.Error.WriteLine("Unexpected token: {0}", expected[0]);
            Console.WriteLine();
        }
        
        private static void Choice(Token token, string[] expected, string fileName)
        {
            Console.Error.WriteLine("Otterkit parsing error: In {0} at Line {1}, Column {2}", fileName, token.line, token.column);
            Console.Error.WriteLine("Unexpected token: Expected {0} or {1}, instead of {2}", expected[0], expected[1], token.value);
            Console.WriteLine();
        }

        private static void Expected(Token token, string[] expected, string fileName)
        {
            Console.Error.WriteLine("Otterkit parsing error: In {0} at Line {1}, Column {2}", fileName, token.line, token.column);
            Console.Error.WriteLine("Unexpected token: Expected {0}, instead of {1}", expected[0], token.value);
            Console.WriteLine();
        }

    }
    
}