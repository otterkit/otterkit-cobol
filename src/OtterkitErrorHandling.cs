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

            int errorPosition = token.column - (line.Length - line.TrimStart().Length);
            error = error.Insert(errorPosition, new String('~', token.value.Length));

            Console.WriteLine($"{" ",5}|");
            Console.WriteLine($"{token.line,4} | {line.TrimStart()}");
            Console.Write($"{" ",5}|");

            Console.ForegroundColor = ConsoleColor.Yellow;
            Console.WriteLine($" {error}\n");
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

            if (error == "general")
            {
                General(token, expected, fileName);
                Console.ResetColor();
                return;
            }

            General(token, expected, fileName);
            Console.ResetColor();
        }

        private static void General(Token token, string[] expected, string fileName)
        {
            Console.Error.Write("Otterkit parsing error: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.Error.WriteLine("In {0} at line {1}, column {2}", fileName, token.line, token.column);

            Console.ForegroundColor = ConsoleColor.Red;
            Console.Error.Write("Unexpected token: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.WriteLine("{0}\n", expected[0]);
        }

        private static void Choice(Token token, string[] expected, string fileName)
        {
            Console.Error.Write("Otterkit parsing error: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.Error.WriteLine("In {0} at line {1}, column {2}", fileName, token.line, token.column);

            Console.ForegroundColor = ConsoleColor.Red;
            Console.Error.Write("Unexpected token: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.WriteLine("Expected {0} or {1}, instead of {2}\n", expected[0], expected[1], token.value);
        }

        private static void Expected(Token token, string[] expected, string fileName)
        {

            Console.Error.Write("Otterkit parsing error: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.Error.WriteLine("In {0} at line {1}, column {2}", fileName, token.line, token.column);

            Console.ForegroundColor = ConsoleColor.Red;
            Console.Error.Write("Unexpected token: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.WriteLine("Expected {0}, instead of {1}\n", expected[0], token.value);
        }

    }

}