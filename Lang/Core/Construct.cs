using Boogie.Lang.Utils;

namespace Boogie.Lang.Core;

// TODO: later on, improve the notion of a construct.
// Constructs should store the text range they are found in, including trivia like whitespace and comments
// This can be implemented with a source generator that generates a 'GetChildren()' which can be used to construct
// the full range. This will require that all items store *all* tokens which form them.
// This will mostly be useful once/if a language server is needed.

/// <summary>
/// The base class for all 'units' of a compilation.
/// Examples of a unit include 'a + b', 'print()', and '0'.
/// </summary>
public abstract record Construct
{
    /// <summary>
    /// Get the position of this item.
    /// </summary>
    public virtual SourceSpan Span
    {
        get
        {
            return SourceSpan.CombineAll(GetChildren().Select(c => c.Span));
        }
    }

    // TODO: reimplement with a compile-time solution
    /// <summary>
    /// Automatically produce a list of all children of this construct.
    /// Assumes record properties to be in order.
    /// </summary>
    public IEnumerable<Construct> GetChildren()
    {
        return GetType().GetProperties()
            .Select(c => (
                cons: c, 
                isCons: typeof(Construct).IsAssignableFrom(c.PropertyType),
                isEnum: typeof(IEnumerable<Construct>).IsAssignableFrom(c.PropertyType)
            ))
            .Where(c => c.isCons || c.isEnum)
            .SelectMany(
                c => c.isCons ? 
                    new Construct[] { (Construct)c.cons.GetValue(this) } : 
                    c.cons.GetValue(this) as IEnumerable<Construct>
            )
            .Where(c => c is not null);
    }

    private const string Indentation = "    ";
    /// <summary>
    /// Serialize to a human-readable form.
    /// </summary>
    public string GetDescription(int level = 1)
    {
        string Indent(string txt, int levelOffset = 0)
            => new StringBuilder(Indentation.Length * (level + levelOffset) + txt.Length).Insert(0, Indentation, level + levelOffset).Append(txt).ToString();

        var t = GetType();
        var str = new StringBuilder(t.Name);

        str.Append(' ').Append('{');

        foreach (var prop in t.GetProperties())
        {
            str.Append("\n")
               .Append(Indent(prop.Name + " = "));

            var pVal = prop.GetValue(this);
            Type pType = pVal?.GetType()!;

            if (pVal == null)
            {
                str.Append("null");
            }
            // Construct or DerivedConstruct
            else if (pType.IsSubclassOf(typeof(Construct)))
            {
                str.Append((pVal as Construct)!.GetDescription(level + 1));
            }
            // Any IEnumerable<Construct>
            else if (pType.GetInterfaces()
               .FirstOrDefault(
                    t => t.GetInterfaces().FirstOrDefault(
                        t1 => typeof(IEnumerable<Construct>).IsAssignableFrom(t1)
                    ) != null
                ) != null)
            {
                str.Append("[");

                foreach (var item in (IEnumerable<Construct>)pVal!)
                {
                    str.Append('\n')
                       .Append(Indent("", 1))
                       .Append(item.GetDescription(level + 2));
                }

                str.Append('\n')
                   .Append(Indent("]"));
            }
            // Strings
            else if (pType == typeof(string))
            {
                str.Append(((string)pVal!).ToRepr());
            }
            // Other
            else
            {
                str.Append(pVal);
            }
        }

        str.Append('\n')
           .Append(Indent("}", -1));

        return str.ToString();
    }
}

////////////////////////
/// BUILDING BLOCKS
////////////////////////
public record Block(List<Construct> Content) : Construct;
public record FullStatement(List<Token> StartTokens, Construct Construct, List<Token> EndTokens) : Construct;
public abstract record Expression : Construct;
public record CommaList(List<Construct> Args) : Construct
{
    public Expression this[int index] => (Args[2*index] as Expression)!;
}

////////////////////////
/// STATEMENTS
////////////////////////
public record IfStatement(Token If, Expression Condition, Token Do, Block Content, IfStatementTrailing Trailing) : Construct;
public record IfStatementTrailing : Construct;
public record IfEnd(Token End) : IfStatementTrailing;
public record ElseBlock(Token Else, Block Content, Token End) : IfStatementTrailing;
public record ElseifBlock(Token Elseif, Expression Condition, Token Do, Block Content, IfStatementTrailing Trailing) : IfStatementTrailing;

public record WhileStatement(Token While, Expression Condition, Token Do, Block Content, Token End) : Construct;
public record ForIterStatement(Token For, Identifier Name, Token In, Expression Iterable, Token Do, Block Content, Token End) : Construct;

public record TryStatement(Token Try, Block Content, Token Catch, Identifier Err, Token In, Block CatchContent, Token End) : Construct;
public record ThrowStatement(Token Throw, Expression Err) : Construct;

public record AssignBase(Expression Key, Expression Value): Construct;
public record Assign(Expression Key, Token Eq, Expression Value) : AssignBase(Key, Value);
public record AssignNew(Expression Key, Token ColEq, Expression Value) : AssignBase(Key, Value);

////////////////////////
/// EXPRESSIONS
////////////////////////
public record ParenthesizedExpression(Token Open, Expression Expression, Token Close) : Expression;

public record PrefixUnaryOperator(UnaryOperatorType Type, Token Operator, Expression Arg) : Expression;
public record BinaryOperator(BinaryOperatorType Type, Expression Lhs, Token Operator, Expression Rhs) : Expression;

public record DotOperator(Expression Base, Token Dot, Expression Value, string StrValue) : Expression;
public record IndexOperator(Expression Base, Token Open, Expression Index, Token Close) : Expression;

public record Literal(Token Token, object? Value) : Expression;
// For string interpolation
//public record StringLiteral(Token Token, object Value) : Expression;
public record Identifier(Token Token, string StrValue) : Expression;
//public record VectorLiteral(Expression[] Args) : Expression;
public record ListLiteral(Token Open, CommaList Args, Token Close) : Expression;
public record TableLiteral(Token Open, Block Args, Token Close) : Expression;
//public record ObjectLiteral(Expression[] Args) : Expression;

public record RangeLiteral(Expression Start, Token To, Expression Value, RangeLiteralTrailing Trailing) : Expression;
public record RangeLiteralTrailing : Construct;
public record RangeLiteralEnd : RangeLiteralTrailing;
public record ByClause(Token By, Expression Value) : RangeLiteralTrailing;

public record CallOperator(Expression Func) : Expression;
public record StandardCallOperator(Expression Func, Token OpenCall, CommaList Args, Token CloseCall) : CallOperator(Func);
public record BangCallOperator(Expression Func, Token Bang, List<Expression> Args) : CallOperator(Func);






/*
 * Worth consideration:
 * 
 * 
 * Rather than using only metamethods like __add, use operator notation:
 * 
 * x.meta.__add --would become
 * x.meta.'+' --internally stored as 'op_+'
 * 
 * x.meta.__iter --would remain as
 * x.meta.__iter
 * 
 * 
 * Custom operators:
 * x.meta.'<>' := x.meta.'!=' --precidence based on first character
 * x.meta.'unary+^' --putting 'unary' makes the opeartor unary
 * 
 * allowed characters are: +*-/%=!$#@<>.&|
 * note that some of these characters can take on special meaning in some contexts
 * 
 * 
 * Seperate '.' and '[]':
 * 
 * x.meta.'.' --index with constant identifiers only (or (+))
 * x.meta.'[]' --index with [] syntax only
 * x.meta.'set.' --special case: setter for '.'
 * x.meta.'set[]' --special case: setter for '[]'
 */