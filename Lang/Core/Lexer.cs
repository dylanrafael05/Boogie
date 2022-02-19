using Boogie.Model;
namespace Boogie.Lang.Core;

public class Lexer
{
    public const char EOS = '\0';

    private string str;
    private string filename;
    private int lastCharIdx, lastLineIdx, lastIdx;
    private int charIdx, lineIdx, idx;

    private char Get(int offset)
    {
        var ridx = idx + offset;
        if (ridx >= str.Length) return EOS;
        return str[ridx];
    }
    private char Current => Get(0);
    private char Next => Get(1);

    private void Advance(int count = 1)
    {
        idx += count;
        charIdx += count;
    }

    private void StartToken()
    {
        lastCharIdx = charIdx;
        lastLineIdx = lineIdx;
        lastIdx = idx;
    }

    private SourceSpan Span
        => new(lastCharIdx, charIdx, lastLineIdx, lineIdx, lastIdx, idx, filename);
    private SourceSpan CharSpan
        => new(charIdx, charIdx + 1, lineIdx, lineIdx + 1, idx, idx + 1, filename);

    private Token MakeToken(TokenType type, object? value = null)
        => new(type, Span, value);
    private Token MakeTokenOfLen(int len, TokenType type, object? value = null)
    {
        Advance(len);
        return new(type, Span, value);
    }

    public Lexer(string str, string filename)
    {
        this.str = str;
        this.filename = filename;

        charIdx = 0;
        lineIdx = 0;
        idx = 0;
    }

    private bool IsWhiteSpace(char c)
        => c is ' ' or '\t' or '\n' or '\r';

    public string GetEscapeSequence()
    {
        if (Current is 'n')
        {
            Advance();
            return "\n";
        }
        else if (Current is 'r')
        {
            Advance();
            return "\r";
        }
        else if (Current is 't')
        {
            Advance();
            return "\t";
        }
        else if (Current is '0')
        {
            Advance();
            return "\0";
        }
        else if (Current is '$')
        {
            Advance();
            return "$";
        }
        else if (Current is '\\')
        {
            Advance();
            return "\\";
        }
        else if (IsWhiteSpace(Current))
        {
            while (Current is not '\\')
            {
                if (!IsWhiteSpace(Current))
                {
                    throw new LexerException($"Unexpected non-whitespace character inside of whitespace escape", CharSpan);
                }
                else if (Current is EOS)
                {
                    throw new LexerException($"Unterminated whitespace escape", Span);
                }

                if (Current is '\n' or '\r')
                {
                    SkipLine();
                }
                else
                {
                    Advance();
                }
            }
            Advance();

            return "";
        }

        throw new LexerException($"Unknown escape sequence '\\{Current}'", CharSpan);
    }

    public Token ReadString()
    {
        var startChar = Current;
        var startCount = 0;
        while(Current == startChar)
        {
            startCount++;
            Advance();
        }

        if(startCount % 2 == 0)
        {
            return MakeToken(TokenType.StringLiteral, "");
        }

        var contents = "";
        var endCount = 0;
        while(endCount != startCount)
        {
            if(Current == startChar)
            {
                endCount++;
            }
            else
            {
                endCount = 0;
            }

            if (Current is '\n' or '\r')
            {
                contents += SkipLine();
            }
            else if (Current is '\\')
            {
                Advance();
                contents += GetEscapeSequence();
            }
            else
            {
                contents += Current;
                Advance();
            }
        }

        return MakeToken(TokenType.StringLiteral, contents.Substring(0, contents.Length - startCount));
    }

    public string SkipLine()
    {
        var ret = "";

        if (Current is '\n')
        {
            ret += '\n';

            if (Next == '\r')
            {
                ret += '\r';
                Advance(2);
            }
            else Advance();

            lineIdx++;
            charIdx = 0;
        }
        else if(Current is '\r')
        {
            ret += '\r';

            if (Next == '\n')
            {
                ret += '\n';
                Advance(2);
            }
            else Advance();

            lineIdx++;
            charIdx = 0;
        }

        return ret;
    }

    public Token LexSingle()
    {
        while (true)
        {
            switch (Current)
            {
                case ' ' or '\t':
                {
                    Advance();
                }
                break;

                case '-':
                {
                    if (Next == '-')
                    {
                        Advance(2);
                        while (Current is not '\n' or '\r' or EOS)
                            Advance();
                    }
                    if (Next == '*')
                    {
                        Advance(2);
                        while (Current is not '*' && Next is not '-')
                        {
                            if(Current is EOS)
                            {
                                throw new LexerException("Unterminated multine comment", Span);
                            }
                            Advance();
                        }
                        Advance(2);
                    }
                }
                break;

                default:
                    goto ReadToken;
            }
        }

        ReadToken:
        StartToken();
        switch (Current)
        {
            case '\n' or '\r':
            {
                SkipLine();
                return MakeToken(TokenType.LineBreak);
            }

            case EOS:
            {
                return MakeToken(TokenType.EOF);
            }

            case '+':
                return MakeTokenOfLen(1, TokenType.Plus);
            case '-':
                return MakeTokenOfLen(1, TokenType.Minus);
            case '*':
            {
                if (Next == '*')
                {
                    return MakeTokenOfLen(2, TokenType.DoubleStar);
                }
                else if (Next == '.')
                {
                    return MakeTokenOfLen(2, TokenType.DotProduct);
                }
                else if (Next == '/')
                {
                    return MakeTokenOfLen(2, TokenType.CrossProduct);
                }
                else
                {
                    return MakeTokenOfLen(1, TokenType.Star);
                }
            }
            case '/':
            {
                if (Next == '/')
                {
                    return MakeTokenOfLen(2, TokenType.DoubleDiv);
                }
                else
                {
                    return MakeTokenOfLen(1, TokenType.Div);
                }
            }

            case '%':
                return MakeTokenOfLen(1, TokenType.Mod);


            case '=':
            {
                if (Next == '=')
                {
                    return MakeTokenOfLen(2, TokenType.DoubleEquals);
                }
                return MakeTokenOfLen(1, TokenType.Equals);
            }

            case ':':
            {
                if (Next == '=')
                {
                    return MakeTokenOfLen(2, TokenType.ColonEquals);
                }
                return MakeTokenOfLen(1, TokenType.Colon);
            }

            case '<':
            {
                if (Next == '=')
                {
                    return MakeTokenOfLen(2, TokenType.LessThanOrEqual);
                }
                return MakeTokenOfLen(1, TokenType.LessThan);
            }
            case '>':
            {
                if (Next == '=')
                {
                    return MakeTokenOfLen(2, TokenType.GreaterThanOrEqual);
                }
                return MakeTokenOfLen(1, TokenType.GreaterThan);
            }

            case '|':
            {
                if (Next == '>')
                {
                    return MakeTokenOfLen(2, TokenType.PipeOperator);
                }
                return MakeTokenOfLen(1, TokenType.VecBound);
            }

            case '~':
            {
                if (Next == '=')
                {
                    return MakeTokenOfLen(2, TokenType.NotEquals);
                }

                goto default;
            }

            case '!':
            {
                if (Next == '=')
                {
                    return MakeTokenOfLen(2, TokenType.NotEquals);
                }

                return MakeTokenOfLen(1, TokenType.Bang);
            }

            case '.':
                return MakeTokenOfLen(1, TokenType.Dot);

            case '(':
                return MakeTokenOfLen(1, TokenType.OpenParen);
            case ')':
                return MakeTokenOfLen(1, TokenType.CloseParen);
            case '{':
                return MakeTokenOfLen(1, TokenType.OpenTable);
            case '}':
                return MakeTokenOfLen(1, TokenType.CloseTable);
            case '[':
                return MakeTokenOfLen(1, TokenType.OpenIndex);
            case ']':
                return MakeTokenOfLen(1, TokenType.CloseIndex);

            case '\'' or '"':
                return ReadString();

            default:
            {
                // numbers
                if (char.IsDigit(Current))
                {
                    string value = "";
                    bool isFloat = false;

                    while (char.IsDigit(Current) || Current == '\'' || (Current == '.' && !isFloat))
                    {
                        var c = Current;
                        Advance();

                        if (Current == '\'')
                        {
                            if(!char.IsDigit(Get(1)))
                            {
                                break;
                            }

                            continue;
                        }
                        else if (Current == '.')
                        {
                            isFloat = true;
                        }

                        value += c;
                    }

                    return MakeToken(TokenType.NumberLiteral, isFloat ? float.Parse(value) : int.Parse(value));
                }

                // identifiers
                if (char.IsLetter(Current) || Current == '_')
                {
                    string value = "";

                    while (char.IsLetterOrDigit(Current) || Current == '_')
                    {
                        value += Current;
                        Advance();
                    }

                    return MakeToken(TokenTypeExtensions.FromIdentifier(value), TokenTypeExtensions.ValueFromIdentifier(value));
                }
                // TODO: strings

                throw new LexerException($"Invalid character '{Current}'.", Span);
            }
        }
    }

    public ForkableDataStream<Token> GetTokenStream()
    {
        IEnumerator<Token> InnerStreams()
        {
            while (true)
            {
                yield return LexSingle();
            }
        }

        return new(InnerStreams(), null!);
    }
}