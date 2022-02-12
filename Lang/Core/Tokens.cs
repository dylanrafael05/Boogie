using System;
using System.Collections.Generic;
using System.Text;

namespace Boogie.Lang.Core;

// TODO: Add support for 'trivia'
// See implementation of C#'s roslyn.
public record Token(TokenType Type, SourceSpan Position, object? Value = null) : Construct
{
    public override SourceSpan Span { get; } = Position;
}

/// <summary>
/// The types which a token can be.
/// </summary>
public enum TokenType
{
    EOF,

    // Endings
    LineBreak,
    Semicolon,

    // Simple values
    Identifier,
    StringLiteral,
    NumberLiteral,
    NilLiteral,
    FalseLiteral,
    TrueLiteral,
    InfinityLiteral,
    NanLiteral,

    // Control characters
    Comma,
    Dot,
    Colon,

    DoubleDot,

    // Operators
    Plus,
    Minus,
    Star,
    Div,
    Mod,

    DoubleStar,
    DoubleDiv,

    DotProduct,
    CrossProduct,

    DoubleEquals,
    NotEquals,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,

    Or,
    And,
    Not,

    Equals,
    ColonEq,

    Bang,

    PipeOperator,

    // Block indicators
    OpenParen,
    CloseParen,
    OpenTable,
    CloseTable,

    VecBound,

    OpenIndex,
    CloseIndex,

    // Keywords
    LetKeyword,

    FunctionKeyword,
    DoKeyword,
    EndKeyword,

    IfKeyword,
    ElseKeyword,
    ElseifKeyword,
    ThenKeyword,

    WhileKeyword,
    ForKeyword,
    InKeyword,

    //ThrowKeyword,
    //TryKeyword,
    //CatchKeyword,

    ObjectKeyword,

    ReturnKeyword,
    SelfKeyword,

    //TypeKeyword,
    //FromKeyword,

    FinalKeyword,

    ToKeyword,
    ByKeyword,

    OfKeyword,

    MetaKeyword,

    GlobalsKeyword,
    LocalsKeyword,
}

/// <summary>
/// All methods associated with <see cref="TokenType"/>
/// </summary>
public static class TokenTypeExtensions
{
    public static TokenType FromIdentifier(string name)
        => name switch
        {
            "let" => TokenType.LetKeyword,
            "function" => TokenType.FunctionKeyword,
            "do" => TokenType.DoKeyword,
            "end" => TokenType.EndKeyword,
            "if" => TokenType.IfKeyword,
            "else" => TokenType.ElseKeyword,
            "elseif" => TokenType.ElseifKeyword,
            "then" => TokenType.ThenKeyword,
            "while" => TokenType.WhileKeyword,
            "for" => TokenType.ForKeyword,
            "in" => TokenType.InKeyword,
            "object" => TokenType.ObjectKeyword,
            "return" => TokenType.ReturnKeyword,
            "self" => TokenType.SelfKeyword,
            "final" => TokenType.FinalKeyword,
            "to" => TokenType.ToKeyword,
            "by" => TokenType.ByKeyword,
            "of" => TokenType.OfKeyword,
            "meta" => TokenType.MetaKeyword,
            "globals" => TokenType.GlobalsKeyword,
            "locals" => TokenType.LocalsKeyword,

            "false" => TokenType.FalseLiteral,
            "true" => TokenType.TrueLiteral,
            "nil" => TokenType.NilLiteral,

            //NOTE: this list must be exhuastive ^^^^^^^^^

            _ => TokenType.Identifier
        };

    public static object? ValueFromIdentifier(string name)
        => name switch
        {
            "false" => false,
            "true" => true,
            "nil" => null,

            _ => name
        };

    public static string GetDescription(this TokenType tokenType)
#pragma warning disable CS8524 // The switch expression does not handle some values of its input type (it is not exhaustive) involving an unnamed enum value.
        => tokenType switch
#pragma warning restore CS8524 // The switch expression does not handle some values of its input type (it is not exhaustive) involving an unnamed enum value.
        {
            TokenType.EOF => "end of input",
            TokenType.LineBreak => "line break",
            TokenType.Semicolon => "';'",
            TokenType.Identifier => "identifier",
            TokenType.StringLiteral => "string literal",
            TokenType.NumberLiteral => "number literal",
            TokenType.NilLiteral => "'nil'",
            TokenType.FalseLiteral => "'false'",
            TokenType.TrueLiteral => "'true'",
            TokenType.InfinityLiteral => "'infinity'",
            TokenType.NanLiteral => "'nan'",
            TokenType.Comma => "','",
            TokenType.Dot => "'.'",
            TokenType.Colon => "':'",
            TokenType.DoubleDot => "'..'",
            TokenType.Plus => "'+'",
            TokenType.Minus => "'-'",
            TokenType.Star => "'*'",
            TokenType.Div => "'/'",
            TokenType.Mod => "'%'",
            TokenType.DoubleStar => "'**'",
            TokenType.DoubleDiv => "'//'",
            TokenType.DotProduct => "'*.'",
            TokenType.CrossProduct => "'*/'",
            TokenType.DoubleEquals => "'=='",
            TokenType.NotEquals => "not equals operator",
            TokenType.GreaterThan => "'>'",
            TokenType.GreaterThanOrEqual => "'>='",
            TokenType.LessThan => "'<'",
            TokenType.LessThanOrEqual => "'<='",
            TokenType.PipeOperator => "'|>'",
            TokenType.Or => "'or'",
            TokenType.And => "'and'",
            TokenType.Not => "'not'",
            TokenType.Equals => "'='",
            TokenType.ColonEq => "':='",
            TokenType.OpenParen => "'('",
            TokenType.CloseParen => "')'",
            TokenType.OpenTable => "'{'",
            TokenType.CloseTable => "'}'",
            TokenType.VecBound => "'|'",
            TokenType.OpenIndex => "'['",
            TokenType.CloseIndex => "']'",
            TokenType.LetKeyword => "let keyword",
            TokenType.FunctionKeyword => "function keyword",
            TokenType.DoKeyword => "do kewyword",
            TokenType.EndKeyword => "end keyword",
            TokenType.IfKeyword => "if keyword",
            TokenType.ElseKeyword => "else keyword",
            TokenType.ElseifKeyword => "elseif keyword",
            TokenType.ThenKeyword => "then keyword",
            TokenType.WhileKeyword => "while keyword",
            TokenType.ForKeyword => "for keyword",
            TokenType.InKeyword => "in keyword",
            TokenType.ObjectKeyword => "object keyword",
            TokenType.ReturnKeyword => "return keyword",
            TokenType.SelfKeyword => "self keyword",
            TokenType.FinalKeyword => "final keyword",
            TokenType.ToKeyword => "to keyword",
            TokenType.ByKeyword => "by keyword",
            TokenType.OfKeyword => "of keyword",
            TokenType.MetaKeyword => "meta keyword",
            TokenType.GlobalsKeyword => "globals keyword",
            TokenType.LocalsKeyword => "locals keyword",
            TokenType.Bang => "'!'",
        };

    public static bool IsComparisonOperator(this TokenType tokenType)
        => tokenType is TokenType.Equals
                     or TokenType.NotEquals
                     or TokenType.GreaterThan
                     or TokenType.GreaterThanOrEqual
                     or TokenType.LessThan
                     or TokenType.LessThanOrEqual;

    public static string GenericBinaryMetamethodFromTokenType(this TokenType tokenType)
        => tokenType switch
        {
            TokenType.Plus => "__add",
            TokenType.Minus => "__sub",
            TokenType.Star => "__mul",
            TokenType.Div => "__div",

            TokenType.DoubleStar => "__pow",
            TokenType.DoubleDiv => "__idiv",

            TokenType.DotProduct => "__dot_prod",
            TokenType.CrossProduct => "__cross_prod",

            //Comparison operators are handled seperately
        };
}
