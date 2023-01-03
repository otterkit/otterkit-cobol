namespace Otterkit;

public enum ErrorType
{
    General,
    Expected,
    Choice,
    Recovery,
}

public static class ErrorHandler
{
    public static bool Error = false;

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
        public static void PrettyError(string fileName, Token token, ConsoleColor color = ConsoleColor.Red)
        {
            string line = File.ReadLines(fileName).Skip(token.line - 1).Take(token.line).First();
            string error = new(' ', line.Length - token.value.Length);

            int count = line.TakeWhile(char.IsWhiteSpace).Count();
            int insertOffset = line.IndexOf(token.value) == token.column ? 0 : 7;
            error = error.Insert(token.column + insertOffset - count, new string('~', token.value.Length));

            Console.WriteLine($"{" ",5}|");
            Console.WriteLine($"{token.line,4} | {line.TrimStart()}");
            Console.Write($"{" ",5}|");

            Console.ForegroundColor = color;
            Console.WriteLine($" {error}\n");
            Console.ResetColor();


        }

        public static void AttemptRecovery(string[] anchors)
        {
            Console.ForegroundColor = ConsoleColor.Yellow;
            Console.Write("Attempting recovery: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.WriteLine($"""
            Unexpected tokens will be ignored until a separator period or an anchor point is found 
            (Anchors: {string.Join(", ", anchors)})

            """);
        }

        public static void AttemptRecovery(TokenType anchor)
        {
            Console.ForegroundColor = ConsoleColor.Yellow;
            Console.Write("Attempting recovery: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.WriteLine($"""
            Unexpected tokens will be ignored until a separator period or an anchor point is found 
            (Anchor: {anchor})

            """);
        }

        public static void Report(string fileName, Token token, ErrorType error, params string[] expected)
        {
            Error = true;

            Console.ForegroundColor = ConsoleColor.Red;
            if (error == ErrorType.Choice)
            {
                Choice(token, expected, fileName);
                Console.ResetColor();
                return;
            }

            if (error == ErrorType.Expected)
            {
                Expected(token, expected[0], fileName);
                Console.ResetColor();
                return;
            }

            if (error == ErrorType.Recovery)
            {
                Recovery(token, expected[0], fileName);
                Console.ResetColor();
                return;
            }

            if (error == ErrorType.General)
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
            Console.Error.WriteLine("{0}:{1}:{2}", Path.GetFullPath(fileName), token.line, token.column);

            Console.ForegroundColor = ConsoleColor.Red;
            Console.Error.Write("Unexpected token: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.WriteLine("{0}\n", expected[0]);
        }

        private static void Choice(Token token, string[] expected, string fileName)
        {
            Console.Error.Write("Otterkit parsing error: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.Error.WriteLine("{0}:{1}:{2}", fileName, token.line, token.column);

            Console.ForegroundColor = ConsoleColor.Red;
            Console.Error.Write("Unexpected token: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.WriteLine("Expected {0} or {1}, instead of {2}\n", expected[0], expected[1], token.value);
        }

        private static void Expected(Token token, string expected, string fileName)
        {

            Console.Error.Write("Otterkit parsing error: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.Error.WriteLine("{0}:{1}:{2}", fileName, token.line, token.column);

            Console.ForegroundColor = ConsoleColor.Red;
            Console.Error.Write("Unexpected token: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.WriteLine("Expected {0}, instead of {1}\n", expected, token.value);
        }

        private static void Recovery(Token token, string recovery, string fileName)
        {
            Console.ForegroundColor = ConsoleColor.Blue;
            Console.Error.Write("Otterkit parsing recovery: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.Error.WriteLine("{0}:{1}:{2}", Path.GetFullPath(fileName), token.line, token.column);

            Console.ForegroundColor = ConsoleColor.Blue;
            Console.Error.Write(recovery);
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.WriteLine("(Anchor: \"{0}\")\n", token.value);
        }

    }

    public static void Terminate(string errorType)
    {
        Console.ForegroundColor = ConsoleColor.Red;
        Console.WriteLine($"Compilation process cancelled due to a {errorType} error");
        Console.ResetColor();
        Environment.Exit(1);
    }
}