using static Otterkit.Types.TokenHandling;
using Otterkit.Types;

namespace Otterkit.Analyzers;

public static partial class DataDivision
{
    private static readonly HashSet<char> SymbolsUsed = new();

    public static int ParsePictureString(ReadOnlySpan<char> picture)
    {
        const string invalidPictureMessage = "Invalid picture clause character string.";
        
        var isAfterDecimalPoint = false;

        var validPicture = true;

        var pictureLength = 0;

        for (var index = 0; index < picture.Length; index++)
        {
            var character = char.ToUpperInvariant(picture[index]);

            if (character is 'S' && !SymbolsUsed.IsEmpty())
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 20, invalidPictureMessage)
                .WithSourceLine(Peek(-1))
                .WithNote("""
                    Symbol 'S' must be the first symbol of the picture string.
                    """)
                .CloseError();

                validPicture = false;
            }

            if(character is 'N' && SymbolsUsed.ContainsAny("9AXSVP1E"))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 20, invalidPictureMessage)
                .WithSourceLine(Peek(-1))
                .WithNote("""
                    Symbol 'N' must not follow any symbols other than 'N'.
                    """)
                .CloseError();

                validPicture = false;
            }

            if(character is '1' && SymbolsUsed.ContainsAny("9AXSVPNE"))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 20, invalidPictureMessage)
                .WithSourceLine(Peek(-1))
                .WithNote("""
                    Symbol '1' must not follow any symbols other than '1'.
                    """)
                .CloseError();

                validPicture = false;
            }

            if(character is 'A' or 'X' && SymbolsUsed.ContainsAny("SVP1NE"))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 20, invalidPictureMessage)
                .WithSourceLine(Peek(-1))
                .WithNote("""
                    Symbols 'A' and 'X' may only follow the symbols '9A' or 'X'.
                    """)
                .CloseError();

                validPicture = false;
            }

            if (character is 'V' && SymbolsUsed.Contains('V'))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 20, invalidPictureMessage)
                .WithSourceLine(Peek(-1))
                .WithNote("""
                    Symbol 'V' may only appear once in the same picture string.
                    """)
                .CloseError();

                validPicture = false;
            }

            if (character is 'V' or 'P' && SymbolsUsed.ContainsAny("AX1NE"))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 20, invalidPictureMessage)
                .WithSourceLine(Peek(-1))
                .WithNote("""
                    Symbols 'V' and 'P' must not follow the symbols 'AX1N' or 'E'.
                    """)
                .CloseError();

                validPicture = false;
            }

            if (character is 'V' && !SymbolsUsed.Contains('V')) isAfterDecimalPoint = true;
            
            if (character is 'P' && SymbolsUsed.ContainsAny("9P") && isAfterDecimalPoint)
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 20, invalidPictureMessage)
                .WithSourceLine(Peek(-1))
                .WithNote("""
                    Symbol 'P' or a string of 'P' may only appear once in a picture clause.
                    """)
                .CloseError();

                validPicture = false;
            }

            if (character is 'P' && !SymbolsUsed.IsEmpty() && SymbolsUsed.Contains('9') && !isAfterDecimalPoint)
            {
                while (picture[index] is 'P' or 'p')
                {
                    if (index >= picture.Length - 1) break;

                    index++;
                }
            }

            if (character is 'P' && (SymbolsUsed.IsEmpty() || !SymbolsUsed.Contains('9') || isAfterDecimalPoint))
            {
                while (picture[index] is 'P' or 'p')
                {
                    if (index >= picture.Length - 1) break;

                    index++;
                }

                isAfterDecimalPoint = true;
            }

            if (character is '(')
            {
                index++;

                var start = index;

                while (picture[index] != ')')
                {
                    if (!picture[index].IsOneOf("0123456789"))
                    {
                        validPicture = false;
                    }

                    index++;
                }

                var end = index;

                var count = int.Parse(picture.Slice(start, end - start));

                pictureLength += count - 1;

                continue;
            }

            if (character.IsOneOf("9AXSVP1N"))
            {
                pictureLength++;
            }

            SymbolsUsed.Add(character);
        }
        
        if (validPicture) return pictureLength;

        return -1;
    }

    private static (Classes, Categories) PictureType(bool validPicture)
    {
        if (!validPicture)
        {
            return (Classes.Invalid, Categories.Invalid);
        }

        if (SymbolsUsed.Contains('N'))
        {
            return (Classes.National, Categories.National);
        }

        if (SymbolsUsed.Contains('A') && !SymbolsUsed.ContainsAny("X9"))
        {
            return (Classes.Alphabetic, Categories.Alphabetic);
        }

        if (SymbolsUsed.Contains('9') && !SymbolsUsed.ContainsAny("XA"))
        {
            return (Classes.Numeric, Categories.Numeric);
        }

        if (SymbolsUsed.Contains('A') && SymbolsUsed.ContainsAny("X9"))
        {
            return (Classes.Alphanumeric, Categories.Alphanumeric);
        }

        if (SymbolsUsed.Contains('X'))
        {
            return (Classes.Alphanumeric, Categories.Alphanumeric);
        }

        if (SymbolsUsed.Contains('1'))
        {
            return (Classes.Boolean, Categories.Boolean);
        }

        return (Classes.Invalid, Categories.Invalid);
    }

    private static void ResetSymbols()
    {
        SymbolsUsed.Clear();
    }
}
