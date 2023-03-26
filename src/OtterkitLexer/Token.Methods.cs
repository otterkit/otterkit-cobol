using System.Globalization;
using System.Text;
using System.Text.RegularExpressions;

namespace Otterkit; 

public sealed partial record Token
{   
    private static Options CompilerOptions = Otterkit.Options;

    private string FetchFile()
    {
        if (FileIndex is 0)
        {
            return CompilerOptions.EntryPoint;
        }

        return CompilerOptions.FileNames[FileIndex];
    }

    private static bool TryValidateIdentifier(Token token)
    {
        // The Unicode standard requires us to document how this works:

        // Otterkit COBOL Identifier Profile (profile of UAX31-R1-1):
        // Identifiers must not have more than 63 characters and are case-insensitive.
        // All identifiers that contain non-normalized characters are converted into the Normalization Form NFKC
        
        // Identifier    :=  XID_Start (Medial | XID_Continue)* XID_Continue
        // XID_Start     :=  <All characters in the unicode categories Lu, Ll, Lt, Lm, Lo, Nl, basic digits [0-9], and characters with the Other_ID_Start property>
        // XID_Continue  :=  <All characters in XID_Start, plus characters in the unicode categories Mn, Mc, Nd, Pc, and characters with the Other_ID_Continue property>
        // Medial        :=  <Only the special characters minus sign (HYPHEN-MINUS), underscore (LOW LINE), and the ・ (KATAKANA MIDDLE DOT)>
        
        // Characters with the Other_ID_Start and Other_ID_Continue properties can be found here: https://www.unicode.org/Public/15.0.0/ucd/PropList.txt

        var fileName = token.FetchFile();

        if (!token.Value.IsNormalized(NormalizationForm.FormKC))
        {
            try
            {
                // If the identifier contains non-normalized characters,
                // try to convert into the Normalization Form NFKC.
                token.Value = token.Value.Normalize(NormalizationForm.FormKC);
            }
            catch (ArgumentException)
            {
                // https://learn.microsoft.com/en-us/dotnet/api/system.string.normalize
                // If a string contains non-normalized characters followed by invalid Unicode characters, 
                // the Normalize method will throw an ArgumentException.
                // Return false and report a syntax error:
                ErrorHandler.Analyzer.Report(fileName, token, ErrorType.Syntax, """
                Conversion into Normalization Form NFKC failed due to an invalid character. Identifiers must not contain invalid Unicode characters.
                """);
                ErrorHandler.Analyzer.PrettyError(fileName, token);

                return false;
            }
        }

        if (token.Value.Length >= 64)
        {
            ErrorHandler.Analyzer.Report(fileName, token, ErrorType.Syntax, """
            Identifiers (user-defined words) must have a length less than or equal to 63 characters.
            """);
            ErrorHandler.Analyzer.PrettyError(fileName, token);
            
            return false;
        }

        var matchesStartCategory = char.GetUnicodeCategory(token.Value[0]) switch
        {
            UnicodeCategory.UppercaseLetter or // Lu
            UnicodeCategory.LowercaseLetter or // Ll
            UnicodeCategory.TitlecaseLetter or // Lt
            UnicodeCategory.ModifierLetter or // Lm
            UnicodeCategory.OtherLetter or // Lo
            UnicodeCategory.LetterNumber => true, // Nl
            _ => false
        };

        var matchesOtherStartCharacters = token.Value[0] switch
        {
            // This list includes all characters with the property Other_ID_Start
            '\u1885' or '\u1886' => true, // MONGOLIAN LETTER ALI GALI BALUDA .. MONGOLIAN LETTER ALI GALI THREE BALUDA
            '\u2118' => true, // SCRIPT CAPITAL P
            '\u212E' => true, // ESTIMATED SYMBOL
            '\u309B' or '\u309C' => true, // KATAKANA-HIRAGANA VOICED SOUND MARK..KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
            '0' or '1' or '2' or '3' or '4' or '5' or '6' or '7' or '8' or '9' => true, // [0-9] Basic digits required by the COBOL Standard

            // NFKC Modifications:
            '\u0E33' => false, // THAI CHARACTER SARA AM
            '\u0EB3' => false, // LAO VOWEL SIGN AM
            '\uFF9E' => false, // HALFWIDTH KATAKANA VOICED SOUND MARK
            '\uFF9F' => false, // HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
            '\u037A' => false, // U+037A GREEK YPOGEGRAMMENI
            _ => false
        };

        if (!matchesStartCategory && !matchesOtherStartCharacters)
        {
            ErrorHandler.Analyzer.Report(fileName, token, ErrorType.Syntax, """
            Invalid character at the start of this identifier.
            """);
            ErrorHandler.Analyzer.PrettyError(fileName, token);
            
            return false;
        }

        static bool matchesContinueCategory(char character)
        {
            return char.GetUnicodeCategory(character) switch
            {
                UnicodeCategory.UppercaseLetter or // Lu
                UnicodeCategory.LowercaseLetter or // Ll
                UnicodeCategory.TitlecaseLetter or // Lt
                UnicodeCategory.ModifierLetter or // Lm
                UnicodeCategory.OtherLetter or // Lo
                UnicodeCategory.LetterNumber or // Nl
                UnicodeCategory.NonSpacingMark or // Mn
                UnicodeCategory.SpacingCombiningMark or // Mc
                UnicodeCategory.DecimalDigitNumber or // Nd
                UnicodeCategory.ConnectorPunctuation => true, // Pc
                _ => false
            };
        }

        static bool matchesOtherContinueCharacters(char character) 
        {
            return character switch
            {
                // This list includes all characters with the properties Other_ID_Start and Other_ID_Continue
                '\u1885' or '\u1886' => true, // MONGOLIAN LETTER ALI GALI BALUDA .. MONGOLIAN LETTER ALI GALI THREE BALUDA
                '\u2118' => true, // SCRIPT CAPITAL P
                '\u212E' => true, // ESTIMATED SYMBOL
                '\u309B' or '\u309C' => true, // KATAKANA-HIRAGANA VOICED SOUND MARK .. KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
                '0' or '1' or '2' or '3' or '4' or '5' or '6' or '7' or '8' or '9' => true, // [0-9] Basic digits required by the COBOL Standard
                '\u00B7' => true, // MIDDLE DOT
                '\u0387' => true, // GREEK ANO TELEIA
                '\u1369' or '\u136A' or '\u136B' or '\u136C' or '\u136D' or '\u136E' or '\u136F' or '\u1370' or '\u1371' => true, // ETHIOPIC DIGIT ONE .. ETHIOPIC DIGIT NINE
                '\u19DA' => true, // NEW TAI LUE THAM DIGIT ONE
                // Extra characters required by the COBOL standard:
                '\u002D' or '\u005F' => true, // minus sign (HYPHEN-MINUS) and underscore (LOW LINE)
                '\u30FB' => true, // KATAKANA MIDDLE DOT

                // NFKC Modifications:
                '\u037A' => false, // U+037A GREEK YPOGEGRAMMENI
                _ => false
            };
        }

        foreach (var character in token.Value.AsSpan(1))
        {
            if (!matchesContinueCategory(character) && !matchesOtherContinueCharacters(character))
            {
                ErrorHandler.Analyzer.Report(fileName, token, ErrorType.Syntax, """
                Invalid character in the middle of this identifier.
                """);
                ErrorHandler.Analyzer.PrettyError(fileName, token);
                
                return false;
            }
        }

        if (token.Value[token.Value.Length - 1] is '\u002D' or '\u005F' or '\u30FB')
        {
            ErrorHandler.Analyzer.Report(fileName, token, ErrorType.Syntax, """
            Invalid character at the end of this identifier.
            """);
            ErrorHandler.Analyzer.PrettyError(fileName, token);
            
            return false;
        }

        return true;
    }

    private static TokenType FindType(Token token)
    {
        if (token.Type is not TokenType.None) return token.Type;

        var value = token.Value;

        // check if the value is a reserved keyword
        if (TokenLookup.IsReservedWord(value))
            return TokenType.ReservedKeyword;

        // check if the value is a figurative literal
        if (TokenLookup.IsReservedFigurativeLiteral(value))
            return TokenType.FigurativeLiteral;

        // check if the value is an intrinsic function
        if (TokenLookup.IsIntrinsicFunctionName(value))
            return TokenType.IntrinsicFunction;

        // check if the value is a symbol
        if (TokenLookup.IsReservedSymbol(value))
            return TokenType.Symbol;

        // check if the value is a numeric
        if (IsNumeric(value) && NumericRegex().IsMatch(value))
            return TokenType.Numeric;

        // check if the value is End Of File
        if (value.Equals("EOF"))
            return TokenType.EOF;
        
        // if none of the above, it's an identifier
        if (!TryValidateIdentifier(token))
        {
            ErrorHandler.HasError = true;
        }

        return TokenType.Identifier;
    }

    private static TokenContext? FindContext(Token token)
    {
        // check if the token belongs to a data division clause
        if (TokenLookup.IsReservedClause(token.Value))
            return TokenContext.IsClause;

        // check if the token is a statement
        if (TokenLookup.IsReservedStatement(token.Value))
            return TokenContext.IsStatement;

        // check if the token represents a file separator   
        if (token.Line is -5)
            return TokenContext.IsEOF;

        // if none of the above, return null
            return null;
    }

    private static TokenScope? FindScope(Token token, Token previousToken)
    {
        if (token.Value.Equals("PROGRAM-ID", StringComparison.OrdinalIgnoreCase))
            return TokenScope.ProgramId;

        if (token.Value.Equals("FUNCTION-ID", StringComparison.OrdinalIgnoreCase))
            return TokenScope.FunctionId;

        if (token.Value.Equals("INTERFACE-ID", StringComparison.OrdinalIgnoreCase))
            return TokenScope.InterfaceId;

        if (token.Value.Equals("CLASS-ID", StringComparison.OrdinalIgnoreCase))
            return TokenScope.ClassId;

        if (token.Value.Equals("METHOD-ID", StringComparison.OrdinalIgnoreCase))
            return TokenScope.MethodId;

        if (token.Value.Equals("ENVIRONMENT", StringComparison.OrdinalIgnoreCase))
            return TokenScope.EnvironmentDivision;

        if (token.Value.Equals("DATA", StringComparison.OrdinalIgnoreCase))
            return TokenScope.DataDivision;

        if (token.Value.Equals("PROCEDURE", StringComparison.OrdinalIgnoreCase))
            return TokenScope.ProcedureDivision;

        if (token.Value.Equals("FACTORY", StringComparison.OrdinalIgnoreCase))
            return TokenScope.Factory;

        if (token.Value.Equals("OBJECT", StringComparison.OrdinalIgnoreCase))
            return TokenScope.Object;

        return previousToken.Scope;
    }

    public static List<Token> FromValue(List<Token> tokens)
    {
        Token previousToken = tokens[0];
        foreach (Token token in tokens)
        {
            token.Type = FindType(token);
            token.Scope = FindScope(token, previousToken);
            token.Context = FindContext(token);

            previousToken = token;
        }

        // If a lexing error has occured, terminate the compilation process.
        // We do not want the compiler to continue when the source code
        // potentially contains invalid Unicode.
        if (ErrorHandler.HasError) ErrorHandler.Terminate("lexing");

        return tokens;
    }

    private static bool IsNumeric(ReadOnlySpan<char> value)
    {
        return value switch
        {
            ['+' or '-', '0' or '1' or '2' or '3' or '4' or '5' or '6' or '7' or '8' or '9', .. _] => true,
            ['.', '0' or '1' or '2' or '3' or '4' or '5' or '6' or '7' or '8' or '9', .. _] => true,
            ['0' or '1' or '2' or '3' or '4' or '5' or '6' or '7' or '8' or '9', .. _] => true,
            [..] => false
        };

    }

    public override sealed string ToString()
    {
        StringBuilder stringBuilder = new();
        stringBuilder.Append("Token");
        stringBuilder.Append(" { ");
        
        stringBuilder.Append($"Ln: {Line, -6},");
        stringBuilder.Append($"Col: {Column, -6},");
        stringBuilder.Append($"Type: {Type, -18},");
        stringBuilder.Append($"Scope: {Scope, -20},");
        stringBuilder.Append($"Context: {Context, -12},");
        stringBuilder.Append($"Value: {Value}");

        stringBuilder.Append(" }");
        return stringBuilder.ToString();
    }

    [GeneratedRegex("^(\\+|-)?\\.?[0-9]+(\\.[0-9]+)?$", RegexOptions.ExplicitCapture | RegexOptions.NonBacktracking)]
    private static partial Regex NumericRegex();
}
