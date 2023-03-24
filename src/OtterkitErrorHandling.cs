namespace Otterkit;

public static class ErrorHandler
{
    internal static bool HasError = false;
    internal static Options Options = OtterkitCompiler.Options;

    public static class Compiler
    {
        public static void Report(string error)
        {
            Console.ForegroundColor = ConsoleColor.Red;
            Console.WriteLine(error);
            Console.ResetColor();
        }
    }

    public static class Analyzer
    {
        public static void PrettyError(string fileName, Token token, ConsoleColor color = ConsoleColor.Red)
        {
            string line = File.ReadLines(fileName).Skip(token.line - 1).Take(token.line).First();
            string error = new(' ', line.Length - token.value.Length);

            int count = line.TakeWhile(char.IsWhiteSpace).Count();
            error = error.Insert(token.column - count, new string('~', token.value.Length));

            Console.WriteLine($"{" ",5}|");
            Console.WriteLine($"{token.line,4} | {line.TrimStart()}");
            Console.Write($"{" ",5}|");

            Console.ForegroundColor = color;
            Console.WriteLine($" {error}\n");
            Console.ResetColor();

            
        }

        public static void Report(string fileName, Token token, ErrorType error, params string[] expected)
        {
            HasError = true;

            Console.ForegroundColor = ConsoleColor.Red;

            if (error == ErrorType.Syntax)
            {
                Syntax(token, expected[0], fileName);
                Console.ResetColor();
                return;
            }

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

            General(token, expected, fileName);
            Console.ResetColor();
        }

        private static void Syntax(Token token, string errorMessage, string fileName)
        {
            Console.Error.Write("Otterkit Lexer error: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.Error.WriteLine("{0}:{1}:{2}", Path.GetFullPath(fileName), token.line, token.column);

            Console.ForegroundColor = ConsoleColor.Red;
            Console.Error.Write("Syntax error: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.WriteLine("{0}\n", errorMessage);
        }

        private static void General(Token token, string[] expected, string fileName)
        {
            Console.Error.Write("Otterkit Analyzer error: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.Error.WriteLine("{0}:{1}:{2}", Path.GetFullPath(fileName), token.line, token.column);

            Console.ForegroundColor = ConsoleColor.Red;
            Console.Error.Write("Unexpected token: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.WriteLine("{0}\n", expected[0]);
        }

        private static void Choice(Token token, string[] expected, string fileName)
        {
            Console.Error.Write("Otterkit Analyzer error: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.Error.WriteLine("{0}:{1}:{2}", fileName, token.line, token.column);

            Console.ForegroundColor = ConsoleColor.Red;
            Console.Error.Write("Unexpected token: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.WriteLine("Expected {0} or {1}, instead of {2}\n", expected[0], expected[1], token.value);
        }

        private static void Expected(Token token, string expected, string fileName)
        {

            Console.Error.Write("Otterkit Analyzer error: ");
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
            Console.Error.Write("Otterkit Analyzer recovery: ");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.Error.WriteLine("{0}:{1}:{2}", Path.GetFullPath(fileName), token.line, token.column);

            Console.ForegroundColor = ConsoleColor.Blue;
            Console.Error.Write(recovery);
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.WriteLine("(Anchor: \"{0}\")\n", token.value);
        }

    }

    public static void SuccessfulParsing()
    {
        var filesCount = Options.FileNames.Count + 1;
        var isPlural = filesCount > 1 ? "s" : "";

        Console.ForegroundColor = ConsoleColor.Blue;
        Console.WriteLine($"Analyzed {filesCount} file{isPlural}, no errors found! \n");
        Console.ResetColor();
    }

    public static void Terminate(string errorType)
    {
        Console.ForegroundColor = ConsoleColor.Red;
        Console.WriteLine($"Compilation process cancelled due to a {errorType} error");
        Console.ResetColor();
        Environment.Exit(1);
    }
}