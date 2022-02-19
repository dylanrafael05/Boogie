namespace Boogie.Lang.Core;

/// <summary>
/// The base exception type for all exceptions relating to syntax.
/// </summary>
public abstract class SyntaxException : Exception
{
    public SourceSpan Position { get; }

    /// <summary>
    /// Get a formatted message with a given position.
    /// </summary>
    private static string WithPosition(string message, SourceSpan pos)
        => $"{pos}: {message}";

    public SyntaxException(string message, SourceSpan pos) : base(WithPosition(message, pos))
    {
        Position = pos;
    }
    public SyntaxException(string message, Exception innerException, SourceSpan pos) : base(WithPosition(message, pos), innerException)
    {
        Position = pos;
    }
}

/// <summary>
/// The exception type for all lexing exceptions.
/// </summary>
public sealed class LexerException : SyntaxException
{
    public LexerException(string message, SourceSpan pos) : base(message, pos)
    {
    }

    public LexerException(string message, Exception innerException, SourceSpan pos) : base(message, innerException, pos)
    {
    }
}

/// <summary>
/// The exception type for all parsing exceptions.
/// </summary>
public sealed class ParserException : SyntaxException
{
    public ParserException(string message, SourceSpan pos) : base(message, pos)
    {
    }

    public ParserException(string message, Exception innerException, SourceSpan pos) : base(message, innerException, pos)
    {
    }
}
