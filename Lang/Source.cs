using Boogie.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace Boogie.Lang.Core;

public static class Metamethods
{
    public const string

        Plus = "__add",
        Minus = "__sub",
        Star = "__mul",
        Div = "__div",
        Neg = "__neg",

        DoubleStar = "__pow",
        DoubleDiv = "__idiv",

        DotProduct = "__dot_prod",
        CrossProduct = "__cross_prod",

        Equal = "__eq",
        LessThan = "__lt",
        LessThanEqual = "__le",

        Read = "__get",
        Write = "__set",
        WriteNew = "__set_new",

        Iterator = "__iter",
        IterAdvance = "advance",
        IterGet = "get",
        IterReset = "reset",

        Bool = "__bool"

    ;
}


public record struct LabelInfo(int Id, int Position);

public static class StringComparisonExtensions
{
    public static bool EqualsWhitespiceInsensitive(this string a, string b)
        => Regex.Replace(a, @"\s+", "") == Regex.Replace(b, @"\s+", " ");

    // From https://stackoverflow.com/a/14087738
    public static string ToLiteral(this string input)
    {
        StringBuilder literal = new StringBuilder(input.Length + 2);
        literal.Append("\"");
        foreach (var c in input)
        {
            literal.Append(c switch
            {
                '\"' => "\\\"",
                '\\' => @"\\",
                '\0' => @"\0",
                '\a' => @"\a",
                '\b' => @"\b",
                '\f' => @"\f",
                '\n' => @"\n",
                '\r' => @"\r",
                '\t' => @"\t",
                '\v' => @"\v",
                _ when c >= 0x20 && c <= 0x7e => c,
                _ => $@"\u{(int)c:x4}"
            });
        }
        literal.Append("\"");
        return literal.ToString();
    }
}

/*public abstract record Construct
{
    public SourcePosition Position { get; }

    public Construct(SourcePosition position)
    {
        this.Position = position;
    }

    private const string Indentation = "    ";
    public string GetDescription(int level = 1)
    {
        string Indent(string txt, int levelOffset = 0)
            => new StringBuilder(Indentation.Length * (level + levelOffset) + txt.Length).Insert(0, Indentation, level + levelOffset).Append(txt).ToString();

        var t = GetType();
        var str = new StringBuilder(t.Name);

        str.Append(' ').Append('{');

        foreach(var prop in t.GetProperties())
        {
            if(prop.Name == nameof(Position))
            {
                continue;
            }

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
                        t1 => t1.IsGenericType 
                           && t1.GetGenericTypeDefinition() == typeof(IEnumerable<>) 
                           && typeof(Construct).IsAssignableFrom(t1.GenericTypeArguments[0])
                    ) != null
                ) != null)
            {
                str.Append("[");

                foreach (var item in (IEnumerable)pVal!)
                {
                    str.Append('\n')
                       .Append(Indent("", 1))
                       .Append(((Construct)item).GetDescription(level + 2))
                       .Append(',');
                }

                str.Remove(str.Length - 1, 1)
                   .Append('\n')
                   .Append(Indent("]"));
            }
            // Strings
            else if(pType == typeof(string))
            {
                str.Append(((string)pVal!).ToLiteral());
            }
            // Other
            else
            {
                str.Append(pVal);
            }

            str.Append(',');
        }

        str.Remove(str.Length - 1, 1)
           .Append('\n')
           .Append(Indent("}", -1));

        return str.ToString();
    }
}*/

public record Block(Construct[] Content) : Construct;
public record IfStatement(Expression Condition, Block Content, Block? Else, IfStatement? Elseif) : Construct;
public record WhileStatement(Expression Condition, Block Content) : Construct;
public record ForIterStatement(Token Name, Expression Iterable, Block Content) : Construct;
public record TryStatement(Construct[] Content, Block? CatchBlock, Block? FinallyBlock) : Construct;

public record AssignSimple(Identifier Base, Expression Value) : Construct;
public record AssignDot(DotOperator Base, Expression Value) : Construct;
public record AssignIndex(IndexOperator Base, Expression Value) : Construct;

public record AssignNewSimple(Identifier Base, Expression Value) : Construct;
public record AssignNewDot(DotOperator Base, Expression Value) : Construct;
public record AssignNewIndex(IndexOperator Base, Expression Value) : Construct;
public record AssignNewSimpleFn(Identifier Base, Construct Value) : Construct;
public record AssignNewDotFn(DotOperator Base, Construct Value) : Construct;
public record AssignNewIndexFn(IndexOperator Base, Construct Value) : Construct;

public record Let(Expression Table, Token Name) : Construct;

public abstract record Expression : Construct;
public record UnaryOperator(UnaryOperatorType Type, Expression Arg) : Expression;
public enum UnaryOperatorType
{
    Not,
    Neg,
}
public record BinaryOperator(BinaryOperatorType Type, Expression Lhs, Expression Rhs) : Expression;
public enum BinaryOperatorType
{
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    IDiv,
    Pow,

    DotProd,
    CrossProd,

    DotIndex,
    Index,

    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    In,

    PipeForward,

    And,
    Or,
}

public record DotOperator(Expression Value, string Index) : Expression;
public record IndexOperator(Expression Value, Expression Index) : Expression;

public record Literal(object Value) : Expression;
public record Identifier(string Value) : Expression;
public record VectorLiteral(Expression[] Args) : Expression;
public record ListLiteral(Expression[] Args) : Expression;
public record TableLiteral(Expression[] Args) : Expression;
public record ObjectLiteral(Expression[] Args) : Expression;
public record RangeLiteral(Expression Start, Expression To, Expression? By) : Expression;
public record CallOperator(Expression Func, Expression[] Args) : Expression;

public record CommaList(Expression[] Args);

public enum Associativity
{
    Left,
    Right,
}

public class Parser
{
    // INPUT
    private readonly ForkableDataStream<Token> tokenStream;

    public Parser(Lexer lexer)
    {
        tokenStream = lexer.GetTokenStream();
    }

    private Token Current => tokenStream.Get()!;
    private Token Get(int offset) => tokenStream.Get(offset)!;
    private void Advance(int count = 1)
        => tokenStream.Advance(count);
    private Token Read(int offset = 0)
        => tokenStream.Read(offset);

    private void SkipNewlines()
    {
        while (Current.Type == TokenType.LineBreak)
        {
            Advance();
        }
    }

    private Token Require(Predicate<TokenType> tokenPredicate, Func<Token, string> errorMessageBuilder)
    {
        var t = tokenStream.Read();
        if (tokenPredicate(t.Type))
        {
            return t;
        }
        else
        {
            throw new ParserException(errorMessageBuilder(t), t.Span);
        }
    }

    public Block Parse()
    {
        SkipNewlines();
        (var block, _) = ParseBlockUpto(t => t is TokenType.EOF);
        Require(t => t is TokenType.EOF, tok => "Expected end of file.");

        return block;
    }

    // Parse from basic file
    private (Block block, Token end) ParseBlockUpto(Predicate<TokenType>? endPred = null)
    {
        endPred ??= t => t is TokenType.EndKeyword;
        var stmts = new List<Construct>();

        while(!endPred(Current.Type))
        {
            stmts.Add(ParseStatement());
        }

        return (new(stmts.ToArray()), Read());
    }

    public IfStatement ParseIf(Token? baseIf = null)
    {
        var start = baseIf ?? Read();
        var condition = ParseExpression();
        var doKey = Require(t => t is TokenType.DoKeyword, tok => "Expected 'do' or 'then' after 'if [condition]'");

        var (body, ender) = ParseBlockUpto(t => t is TokenType.ElseifKeyword or TokenType.ElseKeyword or TokenType.EndKeyword);

        switch(ender.Type)
        {
            case TokenType.ElseKeyword:
                return new(condition, body, ParseBlockUpto(t => t is TokenType.EndKeyword).block, null);

            case TokenType.ElseifKeyword:
                return new(condition, body, null, ParseIf(ender));

            default:
            case TokenType.EndKeyword:
                return new(condition, body, null, null);
        }
    }
    public WhileStatement ParseWhile()
    {
        var start = Read();
        var condition = ParseExpression();
        var doKey = Require(t => t is TokenType.DoKeyword, tok => "Expected 'do' after 'when [condition]'");

        (var body, _) = ParseBlockUpto(t => t is TokenType.EndKeyword);

        return new(condition, body);
    }
    public ForIterStatement ParseForIter()
    {
        var start = Read();
        var iden = Require(t => t is TokenType.Identifier, tok => "Expected an identifier after 'for'");
        var inKey = Require(t => t is TokenType.InKeyword, tok => "Expected 'in' after identifier in 'for'");
        var iterable = ParseExpression();
        var doKey = Require(t => t is TokenType.DoKeyword, tok => "Expected 'do' after 'for [item] in [some]'");

        (var body, _) = ParseBlockUpto(t => t is TokenType.EndKeyword);

        return new(iden, iterable, body);
    }

    public Construct ParseStatement()
    {
        while (Current.Type is TokenType.LineBreak or TokenType.Semicolon)
        {
            Advance();
        }

        Construct item;

        if (Current.Type is TokenType.IfKeyword)
        {
            item = ParseIf();
        }
        else if(Current.Type is TokenType.WhileKeyword)
        {
            item = ParseWhile();
        }
        else if(Current.Type is TokenType.ForKeyword)
        {
            item = ParseForIter();
        }
        else
        {
            item = ParseSimpleStatement();
        }

        while (Current.Type is TokenType.LineBreak or TokenType.Semicolon)
        {
            Advance();
        }

        return item;
    }
    public Construct ParseSimpleStatement()
    {
        var assignee = ParseExpression();

        // a [:]= d
        if(assignee is Identifier i)
        {
            switch(Current.Type)
            {
                case TokenType.ColonEq:
                {
                    var tok = Read();
                    return new AssignNewSimple(i, ParseExpression());
                }
                case TokenType.Equals:
                {
                    var tok = Read();
                    return new AssignSimple(i, ParseExpression());
                }
                default:
                {
                    return i;
                }
            }
        }
        // a.b.c [:]= d
        else if(assignee is DotOperator d)
        {
            switch (Current.Type)
            {
                case TokenType.ColonEq:
                {
                    var tok = Read();
                    return new AssignNewDot(d, ParseExpression());
                }
                case TokenType.Equals:
                {
                    var tok = Read();
                    return new AssignDot(d, ParseExpression());
                }
                default:
                {
                    return d;
                }
            }
        }
        // a[0] [:]= dt`
        else if(assignee is IndexOperator ind)
        {
            switch (Current.Type)
            {
                case TokenType.ColonEq:
                {
                    var tok = Read();
                    return new AssignNewIndex(ind, ParseExpression());
                }
                case TokenType.Equals:
                {
                    var tok = Read();
                    return new AssignIndex(ind, ParseExpression());
                }
                default:
                {
                    return ind;
                }
            }
        }
        // call statement
        else if(assignee is CallOperator call)
        {
            return call;
        }

        // TODO: parse a[.b.c]() := [do/expr] and a[.b.c]! := [do/expr]

        throw new ParserException("Expected a valid statement", assignee.Span);
    }

    public Expression ParseBinary(Func<TokenType, BinaryOperatorType?> op, Func<Expression> next, Associativity assoc = Associativity.Left)
    {
        // LEFT ASSOCIATIVITY:
        // (a + b) + c

        // RIGHT ASSOCIATIVITY:
        // a ** (b ** c)

        var args = new List<Expression>();
        var ops = new List<(BinaryOperatorType type, SourceSpan pos)>();

        args.Add(next());
        
        while (op(Current.Type) is BinaryOperatorType opType)
        {
            var tok = Read();
            ops.Add((opType, tok.Span));

            SkipNewlines();

            args.Add(next());
        }

        // Single case
        if(args.Count == 1)
        {
            return args[0];
        }

        // Many
        if(assoc is Associativity.Left)
        {
            var binOp = args.First();
            for(int i = 1; i < args.Count; i++)
            {
                binOp = new BinaryOperator(ops[i-1].type, binOp, args[i]);
            }
            return binOp;
        }
        else
        {
            var binOp = args.Last();
            for(int i = args.Count - 2; i >= 0; i--)
            {
                binOp = new BinaryOperator(ops[i].type, args[i], binOp);
            }
            return binOp;
        }
    }
    public Expression ParseUnaryPre(Func<TokenType, UnaryOperatorType?> op, Func<Expression> next)
    {
        if (op(Current.Type) is UnaryOperatorType opType)
        {
            var tok = Read();
            var val = next();

            return new UnaryOperator(opType, val);
        }

        return next();
    }

    // Expression parsers
    public Expression ParseExpression()
    {
        return ParseCommaListExpr();
    }

    public bool CanParseInline()
        => Current.Type is not TokenType.LineBreak and not TokenType.Semicolon;
    public Expression ParseInline(Func<Expression> item)
    {
        while(Current.Type is TokenType.DoubleDot)
        {
            Advance();
            Require(t => t is TokenType.LineBreak, tok => $"Double dots must be followed by line breaks!");
            while (Current.Type is TokenType.LineBreak)
                Advance();
        }

        return item();
    }

    private CommaList ParseCommaList(Predicate<TokenType>? endPred = null)
    {
        ////////////////////////////////
        var next = ParseForwardPipeExpr;
        ////////////////////////////////

        var exprs = new List<Expression>();
        Token? first = null;

        if(endPred != null)
        {
            while (!endPred(Current.Type))
            {
                exprs.Add(next());
                if(Current.Type != TokenType.Comma)
                {
                    break;
                }

                first ??= Read();
            }
        }
        else
        {
            first = Current;
            exprs.Add(next());

            while (Current.Type == TokenType.Comma)
            {
                first ??= Current;
                exprs.Add(next());
            }
        }

        return new(exprs.ToArray());
    }

    public Expression ParseCommaListExpr()
        => ParseCommaList() is var cl && cl.Args.Length > 1 ? new ListLiteral(cl.Args) : cl.Args.First();

    public Expression ParseForwardPipeExpr()
        => ParseBinary(t => t is TokenType.PipeOperator ? BinaryOperatorType.PipeForward : null, ParseOrExpr);
    public Expression ParseOrExpr()
        => ParseBinary(t => t is TokenType.Or ? BinaryOperatorType.Or : null, ParseAndExpr);
    public Expression ParseAndExpr()
        => ParseBinary(t => t is TokenType.And ? BinaryOperatorType.And : null, ParseNotExpr);
    public Expression ParseNotExpr()
        => ParseUnaryPre(t => t is TokenType.Not ? UnaryOperatorType.Not : null, ParseComparisonExpr);
    public Expression ParseComparisonExpr()
        => ParseBinary(t => t switch
        {
            TokenType.DoubleEquals => BinaryOperatorType.Equal,
            TokenType.NotEquals => BinaryOperatorType.NotEqual,
            TokenType.GreaterThan => BinaryOperatorType.GreaterThan,
            TokenType.LessThan => BinaryOperatorType.LessThan,
            TokenType.GreaterThanOrEqual => BinaryOperatorType.GreaterThanOrEqual,
            TokenType.LessThanOrEqual => BinaryOperatorType.LessThanOrEqual,
            TokenType.InKeyword => BinaryOperatorType.In,
            _ => null
        }, ParseAdditiveExpr);

    public Expression ParseAdditiveExpr()
        => ParseBinary(t => t is TokenType.Plus ? BinaryOperatorType.Add : (t is TokenType.Minus ? BinaryOperatorType.Sub : null),
                       ParseMultiplicativeExpr);
    public Expression ParseMultiplicativeExpr()
        => ParseBinary(t => t switch
        {
            TokenType.Star          => BinaryOperatorType.Mul,
            TokenType.Div           => BinaryOperatorType.Div,
            TokenType.Mod           => BinaryOperatorType.Mod,
            TokenType.DoubleDiv     => BinaryOperatorType.IDiv,
            TokenType.DotProduct    => BinaryOperatorType.DotProd,
            TokenType.CrossProduct  => BinaryOperatorType.CrossProd,
            _ => null
        }, ParseExponentialExpr);

    public Expression ParseExponentialExpr()
        => ParseBinary(t => t is TokenType.DoubleStar ? BinaryOperatorType.Pow : null, ParseNegateExpr);

    public Expression ParseNegateExpr()
        => ParseUnaryPre(t => t is TokenType.Minus ? UnaryOperatorType.Not : null, ParseDotExpr);

    public Expression ParseDotExpr()
    {
        ////////////////////////////////
        var next = ParseCallExpr;
        ////////////////////////////////
        
        var val = next();

        while (Current.Type == TokenType.Dot)
        {
            if(Get(1).Type == TokenType.Identifier)
            {
                var dot = Read();
                var iden = Read();
                val = new DotOperator(val, (string)dot.Value!);
            }
        }

        return val;
    }

    public Expression ParseCallExpr()
    {
        ////////////////////////////////
        var next = ParseBasicValue;
        ////////////////////////////////
        
        var val = next();

        // f(a, b, c)
        if(Current.Type == TokenType.OpenParen)
        {
            var start = Read();
            var inner = ParseCommaList(t => t is TokenType.CloseParen);
            var close = Require(t => t is TokenType.CloseParen, tok => $"Expected matching ')' for '(' (at {start.Span})");
            return new CallOperator(val, inner.Args);
        }

        // f! a b c
        if(Current.Type == TokenType.Bang)
        {
            var bang = Read();
            var exprs = new List<Expression>();

            while(CanParseInline())
            {
                exprs.Add(ParseInline(ParseBasicValue));
            }

            return new CallOperator(val, exprs.ToArray());
        }

        // otherwise
        return val;
    }

    public Expression ParseBasicValue()
    {
        switch(Current.Type)
        {
            // a
            case TokenType.Identifier:
                return new Identifier((string)Read().Value!);

            // literal
            case TokenType.StringLiteral
              or TokenType.NumberLiteral
              or TokenType.FalseLiteral
              or TokenType.TrueLiteral
              or TokenType.InfinityLiteral
              or TokenType.NanLiteral
              or TokenType.NilLiteral:
                return new Literal(Read().Value!);

            // (...)
            case TokenType.OpenParen:
            {
                var start = Read();
                var inner = ParseExpression();
                var close = Require(t => t is TokenType.CloseParen, tok => $"Expected matching ')' for '(' (at {start.Span})");
                return inner;
            }

            //case TokenType.OpenTable:
            //    return ParseTableLiteral();

            // [...]
            case TokenType.OpenIndex:
            {
                var start = Read();
                var inner = ParseCommaList(t => t is TokenType.CloseIndex);
                var close = Require(t => t is TokenType.CloseIndex, tok => $"Expected matching ']' for '[' at {start.Span}");
                return new ListLiteral(inner.Args);
            }
            // |...|
            case TokenType.VecBound:
            {
                var start = Read();
                var inner = ParseCommaList();
                var close = Require(t => t is TokenType.VecBound, tok => $"Expected matching '|' for '|' at {start.Span}");
                return new VectorLiteral(inner.Args);
            }

            // ???
            default:
            {
                throw new ParserException("Unexpected item: " + Current.Type.GetDescription(), Current.Span);
            }
        }
    }
}

#if EXECUTION
/// <summary>
/// An enumeration representing all possible operations.
/// Any operation ending in 'NC' is a 'non consuming' version of a previous operation:
/// - Does not pop from the stack any values
/// - If it must, replaces them immediately
/// </summary>
public enum OperationType
{
    Push,
    Dup,
    Discard,

    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,

    MetaCallSingle,
    MetaCallDouble,

    Not,

    Read,
    Assign,
    Define,

    GetInScope,

    Pipe,

    GetGlobals,
    GetLocals,

    GetMeta,
    SetMeta,

    Bool,
    Goto,
    CondGoto,
    CondGotoNC,

    Throw,

    BeginTry,
    Catch,
    Finally,
    EndTry,

    // CONTROL OPERATIONS
    NoOp,
}

//////////////////////////////////////
// OPERATIONS
//////////////////////////////////////

public record struct Operation(OperationType Type, object? Argument = null)
{
    public void Execute(ExecutionContext context)
    {
        switch(Type)
        {
            case OperationType.NoOp:
            break;

            case OperationType.Push:
            {
                context.Push((DynValue)Argument!);
            }
            break;

            case OperationType.Dup:
            {
                context.Push(context.Peek());
            }
            break;

            case OperationType.Discard:
            {
                context.Pop();
            }
            break;

            case OperationType.MetaCallSingle:
            {
                var value = context.Pop();
                var result = DynValue.MetaCall(value, (string)Argument!, new DynValue[0]);
                context.Push(result);
            }
            break;

            case OperationType.GetMeta:
            {
                var current = context.Pop();
                context.Push(current.Metatable);
            }
            break;
        }
    }
}

public record OperationsList(List<Operation> Operations);
public record struct StackFrameContext(int? CatchOp);
public record StackFrame(Stack<DynValue> ExecutionStack, OperationsList Operations, int CurrentOp, StackFrameContext Context);



public sealed class RuntimeException : Exception
{
    public DynValue InternalException { get; }
    public RuntimeException(DynValue internalException) : base("An error has occured within the script: ")
    {
        this.InternalException = internalException;
    }
}
public record ExecutionContext(Stack<StackFrame> CallStack)
{
    public void Push(DynValue value)
    {
        CallStack.Peek().ExecutionStack.Push(value);
    }
    public DynValue Peek()
    {
        return CallStack.Peek().ExecutionStack.Peek();
    }
    public DynValue Pop()
    {
        return CallStack.Peek().ExecutionStack.Pop();
    }
    public void Throw(DynValue value)
    {
        
    }
    public void Assert(bool that, DynValue value)
    {
        if(!that)
        {
            Throw(value);
        }
    }
}

public static class Errors
{
    public const string

        InvalidInput = "INVALID_INPUT"

    ;

    public static DynObject NewError(string code, string message)
        => DynObject.FromTrusted(new() { { "code", new DynString(code) }, { "message", new DynString(message) } }, true);

    public static DynObject NewInvalidInput(string message)
        => NewError(InvalidInput, message);
}

public static class Metatables
{
    // possible but might throw off: //TODO: make all basic operators be builtin and separate checking logic to a function defined in operators?
    //TODO: make '.Throw' and '.Assert' throw with callback functions in order to prevent fallthrough!
    private static DynCallbackFunction UnaryOp(Func<DynValue, DynValue> op, Predicate<DynValue> pred, string predError)
        => new DynCallbackFunction(
            (ctx, args) =>
            {
                var val = args[0];
                ctx.Assert(pred(val), Errors.NewInvalidInput(predError));
                return op(val);
            },
            new[] { "val" }
        );
    private static DynCallbackFunction BinaryOp(Func<DynValue, DynValue, DynValue> op, Predicate<DynValue> lhsPred, Predicate<DynValue> rhsPred, string predError)
        => new DynCallbackFunction(
            (ctx, args) =>
            {
                var (lhs, rhs) = (args[0], args[1]);
                ctx.Assert(lhsPred(lhs), Errors.NewInvalidInput(predError));
                ctx.Assert(rhsPred(rhs), Errors.NewInvalidInput(predError));
                return op(lhs, rhs);
            },
            new[] { "lhs", "rhs" }
        );

    private const string IntegralMismatchErrorMessage = "Cannot call integral operator with these arguments!";
    private const string NumericMismatchErrorMessage = "Cannot call numeric operator with these arguments!";

    private static DynCallbackFunction IntBinaryOp(Func<int, int, int> opII, Func<int, double, double> opID)
        => BinaryOp(
            (lhs, rhs) =>
            {
                var lhsI = ((DynInt)lhs).Value;
                if (rhs is DynInt rhsI)
                {
                    var result = opII(lhsI, rhsI.Value);
                    return new DynInt(result);
                }
                else
                {
                    var rhsD = ((DynNum)rhs)!;

                    var result = opID(lhsI, rhsD.Value);
                    return new DynNum(result);
                }
            },
            lhs => lhs is DynInt,
            rhs => rhs is DynInt or DynNum,
            IntegralMismatchErrorMessage
        );
    private static DynCallbackFunction IntBinaryOp(Func<int, int, double> opII, Func<int, double, double> opID)
        => BinaryOp(
            (lhs, rhs) =>
            {
                var lhsI = ((DynInt)lhs).Value;
                if (rhs is DynInt rhsI)
                {
                    var result = opII(lhsI, rhsI.Value);
                    return new DynNum(result);
                }
                else
                {
                    var rhsD = ((DynNum)rhs)!;

                    var result = opID(lhsI, rhsD.Value);
                    return new DynNum(result);
                }
            },
            lhs => lhs is DynInt,
            rhs => rhs is DynInt or DynNum,
            IntegralMismatchErrorMessage
        );
    private static DynCallbackFunction IntBinaryOp(Func<int, int, int> opII, Func<int, double, int> opID)
        => BinaryOp(
            (lhs, rhs) =>
            {
                var lhsI = ((DynInt)lhs).Value;
                if (rhs is DynInt rhsI)
                {
                    var result = opII(lhsI, rhsI.Value);
                    return new DynInt(result);
                }
                else
                {
                    var rhsD = ((DynNum)rhs)!;

                    var result = opID(lhsI, rhsD.Value);
                    return new DynInt(result);
                }
            },
            lhs => lhs is DynInt,
            rhs => rhs is DynInt or DynNum,
            IntegralMismatchErrorMessage
        );
    private static DynCallbackFunction NumBinaryOp(Func<double, double, double> op)
        => BinaryOp(
            (lhs, rhs) =>
            {
                var lhsV = ((DynNum)lhs).Value;
                var rhsV = rhs is DynNum rhsD ? rhsD.Value : ((DynInt)rhs)!.Value;

                var result = op(lhsV, rhsV);
                return new DynNum(result);
            },
            lhs => lhs is DynNum,
            rhs => rhs is DynInt or DynNum,
            NumericMismatchErrorMessage
        );
    private static DynCallbackFunction NumBinaryOp(Func<double, double, int> op)
        => BinaryOp(
            (lhs, rhs) =>
            {
                var lhsV = ((DynNum)lhs).Value;
                var rhsV = rhs is DynNum rhsD ? rhsD.Value : ((DynInt)rhs)!.Value;

                var result = op(lhsV, rhsV);
                return new DynInt(result);
            },
            lhs => lhs is DynNum,
            rhs => rhs is DynInt or DynNum,
            NumericMismatchErrorMessage
        );

    public static DynCallbackFunction

        // Integral
        IntegralAdd = IntBinaryOp((a, b) => a + b, (a, b) => a + b),
        IntegralSub = IntBinaryOp((a, b) => a - b, (a, b) => a - b),
        IntegralMul = IntBinaryOp((a, b) => a * b, (a, b) => a * b),
        IntegralDiv = IntBinaryOp((a, b) => a / (double)b, (a, b) => a / b),

        IntegralPow = IntBinaryOp((a, b) => Math.Pow(a, b), (a, b) => Math.Pow(a, b)),
        IntegralIDiv = IntBinaryOp((a, b) => a / b, (a, b) => (int)(a / b)),

        // Numeric
        NumericAdd = NumBinaryOp((a, b) => a + b),
        NumericSub = NumBinaryOp((a, b) => a - b),
        NumericMul = NumBinaryOp((a, b) => a * b),
        NumericDiv = NumBinaryOp((a, b) => a / b),

        NumericPow = NumBinaryOp((a, b) => Math.Pow(a, b)),
        NumericIDiv = NumBinaryOp((a, b) => (int)(a / b)),

        Sus

    ;

    public static DynTableBase

        Int = DynTable.From(new()
        {
            { Metamethods.Plus, new DynCallbackFunction(
                (ctx, args) =>
                {
                    var (lhs, rhs) = (args[0], args[1]);
                    ctx.Assert(lhs is DynInt, DynObject.Error("Mismatched type", "Cannot call IntMeta.__plus with non-integers."));
                    ctx.Assert(rhs is DynInt or DynNum, DynObject.Error("Mismatched type", "Cannot call IntMeta.__plus with non-numbers."));
                }
            )}
        })

    ;
}

public class DynInt : DynValue
{
    public int Value;

    public DynInt(int value)
    {
        Value = value;
        Metatable = Metatables.Int;
    }
}
public class DynNum : DynValue
{
    public double Value;

    public DynNum(double value)
    {
        Value = value;
        Metatable = Metatables.Num;
    }
}
public class DynBool : DynValue
{
    public bool Value;

    public DynBool(bool value)
    {
        Value = value;
        Metatable = Metatables.Bool;
    }
}
public class DynString : DynValue
{
    public string Value;

    public DynString(string value)
    {
        Value = value;
        Metatable = Metatables.String;
    }
}

public abstract class DynCallable : DynValue
{
    public abstract DynValue Invoke(ExecutionContext context, DynValue[] args);
}
public class DynFunction : DynCallable
{
    private OperationsList operations;
    private string[] argumentNames;

    public DynFunction(OperationsList operations, string[] argumentNames)
    {
        this.operations = operations;
        this.argumentNames = argumentNames;

        Metatable = Metatables.Function;
    }

    public override DynValue Invoke(ExecutionContext context, DynValue[] args)
    {
        throw new NotImplementedException();
    }
}
public class DynCallbackFunction : DynCallable
{
    public delegate DynValue Delegate(ExecutionContext context, DynValue[] arguments);
    private Delegate innerDelegate;

    public DynCallbackFunction(Delegate innerDelegate, string[] argumentInfo)
    {
        this.innerDelegate = innerDelegate;

        Metatable = Metatables.CallbackFunction;
    }

    public override DynValue Invoke(ExecutionContext context, DynValue[] args)
        => innerDelegate(context, args);
}

public abstract class DynTableBase : DynValue
{
    public Dictionary<DynValue, (DynValue? value, bool final)> InnerTable { get; private set; }

    protected DynTableBase(Dictionary<DynValue, (DynValue?, bool)> innerTable)
    {
        InnerTable = innerTable;
    }

    protected DynTableBase() : this(new Dictionary<DynValue, (DynValue?, bool)>())
    {}
    protected DynTableBase(IDictionary<DynValue, DynValue?> dict) : this(dict.ToDictionary(kvp => kvp.Key, kvp => (kvp.Value, false)))
    {}

    public DynValue? Get(DynValue key)
    {
        return InnerTable[key].value;
    }

    public void Set(DynValue key, DynValue? value, ExecutionContext context)
    {
        if (!InnerTable.ContainsKey(key))
        {
            context.Throw();
        }

        (_, var final) = InnerTable[key];

        if (final)
        {
            context.Throw();
        }

        InnerTable[key] = (value, final);
    }

    public void MarkFinal(DynValue key, ExecutionContext context)
    {
        if (!InnerTable.ContainsKey(key))
        {
            context.Throw();
        }

        (var value, _) = InnerTable[key];
        InnerTable[key] = (value, true);
    }

    public virtual void SetNew(DynValue key, DynValue? value, ExecutionContext context)
    {
        if (InnerTable.ContainsKey(key))
        {
            context.Throw();
        }

        InnerTable.Add(key, (value, false));
    }

    public virtual void SetOrNew(DynValue key, DynValue? value, ExecutionContext context)
    {
        if (InnerTable.ContainsKey(key))
        {
            Set(key, value, context);
        }
        else
        {
            SetNew(key, value, context);
        }
    }

    public void Contains(DynValue key)
        => InnerTable.ContainsKey(key);
}
public class DynTable : DynTableBase
{
    protected DynTable() : base()
    {
        Metatable = Metatables.Table;
    }
    protected DynTable(Dictionary<DynValue, (DynValue?, bool)> table) : base(table)
    {
        Metatable = Metatables.Table;
    }
    protected DynTable(IDictionary<DynValue, DynValue?> dict) : base(dict)
    {
        Metatable = Metatables.Table;
    }

    public static DynTable From(Dictionary<DynValue, DynValue?> dict)
        => new(dict);
    public static DynTable New()
        => new();
    public static DynTable FromTable(DynTableBase table)
        => new(table.InnerTable.ToDictionary(kvp => kvp.Key, kvp => kvp.Value));
}
public class DynObject : DynTableBase
{
    private void Verify(ExecutionContext context)
    {
        foreach(var k in InnerTable.Keys)
        {
            if(k is not DynString)
            {
                context.Throw();
            }
        }
    }
    private void VerifyCompileTime()
    {
        foreach (var k in InnerTable.Keys)
        {
            if (k is not DynString)
            {
                throw new Exception("WTF!!!!");
            }
        }
    }

    protected DynObject() : base()
    {
        Metatable = Metatables.Object;
    }
    protected DynObject(Dictionary<DynValue, (DynValue?, bool)> trustedTable) : base(trustedTable)
    {
        Metatable = Metatables.Object;
        VerifyCompileTime();
    }
    protected DynObject(ExecutionContext context, Dictionary<DynValue, (DynValue?, bool)> table) : base(table)
    {
        Metatable = Metatables.Object;
        Verify(context);
    }
    protected DynObject(ExecutionContext context, IDictionary<DynValue, DynValue?> dict) : base(dict)
    {
        Metatable = Metatables.Object;
        Verify(context);
    }

    public static DynObject FromTable(ExecutionContext context, DynTableBase table)
        => new(context, table.InnerTable.ToDictionary(kvp => kvp.Key, kvp => kvp.Value));
    public static DynObject FromTrusted(Dictionary<string, DynValue?> table, bool final = false)
        => new(table.ToDictionary(kvp => new DynString(kvp.Key) as DynValue, kvp => (kvp.Value, final)));

    public override void SetNew(DynValue key, DynValue? value, ExecutionContext context)
    {
        context.Throw();
    }

    public override void SetOrNew(DynValue key, DynValue? value, ExecutionContext context)
    {
        Set(key, value, context);
    }
}


public abstract class DynValue
{
    public DynTableBase? Metatable { get; protected set; }

    internal DynValue() {}

    public static bool Equal(DynValue a, DynValue b)
        => (a, b) is (null, null) || a == b || a.HasMeta()

    public 

    public static DynValue MetaCall(DynValue obj, string name, DynValue[] arguments)
    {
        return (Metatable!.Get(name) as DynCallable).Invoke(arguments);
    }

    public void SetMetatable(DynTableBase table, ExecutionContext context)
    {

    }
}
#endif