using Boogie.Model;
namespace Boogie.Lang.Core;

public class Parser
{
    // INPUT
    private readonly ForkableDataStream<Token> tokenStream;

    public Parser(Lexer lexer)
    {
        tokenStream = lexer.GetTokenStream();
    }

    private enum Associativity
    {
        Left,
        Right,
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

    private Identifier IdenFromToken(Token token)
    {
        if(token.Type is not TokenType.Identifier)
        {
            throw new InvalidOperationException();
        }

        return new(token, (token.Value as string)!);
    }

    private Identifier RequireIdentifier(Func<Token, string> errorMessageBuilder)
    {
        var tok = Require(t => t is TokenType.Identifier, errorMessageBuilder);
        return IdenFromToken(tok);
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

        return (new(stmts), Read());
    }


    public ElseBlock ParseElse(Token elseKey)
    {
        var (content, end) = ParseBlockUpto();
        return new(elseKey, content, end);
    }
    public ElseifBlock ParseElseif(Token elseifKey)
    {
        var condition = ParseExpression();
        var doKey = Require(t => t is TokenType.DoKeyword, tok => "Expected 'do' or 'then' after 'elseif [condition]'");

        var (content, end) = ParseBlockUpto(t => t is TokenType.ElseifKeyword or TokenType.ElseKeyword or TokenType.EndKeyword);
        var trailing = ParseIfStatementTrailing(end);

        return new(elseifKey, condition, doKey, content, trailing);
    }

    private IfStatementTrailing ParseIfStatementTrailing(Token end)
    {
        return end.Type switch
        {
            TokenType.ElseifKeyword => ParseElseif(end),
            TokenType.ElseKeyword => ParseElse(end),
            TokenType.EndKeyword => new IfEnd(end) as IfStatementTrailing,
            _ => throw new InvalidOperationException()
        };
    }

    public IfStatement ParseIf()
    {
        var ifKey = Read();
        var condition = ParseExpression();
        var doKey = Require(t => t is TokenType.DoKeyword, tok => "Expected 'do' or 'then' after 'if [condition]'");

        var (content, end) = ParseBlockUpto(t => t is TokenType.ElseifKeyword or TokenType.ElseKeyword or TokenType.EndKeyword);

        var trailing = ParseIfStatementTrailing(end);

        return new(ifKey, condition, doKey, content, trailing);
    }

    public WhileStatement ParseWhile()
    {
        var whileKey = Read();
        var condition = ParseExpression();
        var doKey = Require(t => t is TokenType.DoKeyword, tok => "Expected 'do' after 'when [condition]'");

        var (content, end) = ParseBlockUpto();

        return new(whileKey, condition, doKey, content, end);
    }
    public ForIterStatement ParseForIter()
    {
        var forKey = Read();
        var name = RequireIdentifier(tok => "Expected an identifier after 'for'");
        var inKey = Require(t => t is TokenType.InKeyword, tok => "Expected 'in' after identifier in 'for'");
        var iterable = ParseExpression();
        var doKey = Require(t => t is TokenType.DoKeyword, tok => "Expected 'do' after 'for [item] in [some]'");

        var (content, end) = ParseBlockUpto(t => t is TokenType.EndKeyword);

        return new(forKey, name, inKey, iterable, doKey, content, end);
    }

    public FullStatement ParseStatement()
    {
        var start = new List<Token>();
        while (Current.Type is TokenType.LineBreak or TokenType.Semicolon)
        {
            start.Add(Read());
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

        var end = new List<Token>();
        while (Current.Type is TokenType.LineBreak or TokenType.Semicolon)
        {
            end.Add(Read());
        }

        return new(start, item, end);
    }
    public Construct ParseSimpleStatement()
    {
        var assignee = ParseExpression();

        // a [:]= d
        if(assignee is Identifier or DotOperator or IndexOperator)
        {
            switch(Current.Type)
            {
                case TokenType.Equals:
                {
                    var tok = Read();
                    return new Assign(assignee, tok, ParseExpression());
                }
                case TokenType.ColonEquals:
                {
                    var tok = Read();
                    return new AssignNew(assignee, tok, ParseExpression());
                }
            }
        }
        // call statement
        else if(assignee is CallOperator)
        {
            return assignee;
        }

        // TODO: parse a[.b.c]() := [do/expr] and a[.b.c]! := [do/expr]

        throw new ParserException($"Expected a valid statement but got.", assignee.Span);
    }

    private Expression ParseBinary(Func<TokenType, BinaryOperatorType?> op, Func<Expression> next, Associativity assoc = Associativity.Left)
    {
        // LEFT ASSOCIATIVITY:
        // (a + b) + c

        // RIGHT ASSOCIATIVITY:
        // a ** (b ** c)

        var args = new List<Expression>();
        var ops = new List<(BinaryOperatorType type, Token tok)>();

        args.Add(next());
        
        while (op(Current.Type) is BinaryOperatorType opType)
        {
            var tok = Read();
            ops.Add((opType, tok));

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
                var (type, tok) = ops[i - 1];
                binOp = new BinaryOperator(type, binOp, tok, args[i]);
            }
            return binOp;
        }
        else
        {
            var binOp = args.Last();
            for(int i = args.Count - 2; i >= 0; i--)
            {
                var (type, tok) = ops[i];
                binOp = new BinaryOperator(type, args[i], tok, binOp);
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

            return new PrefixUnaryOperator(opType, tok, val);
        }

        return next();
    }

    // Expression parsers
    public Expression ParseExpression()
    {
        return ParseOrExpr(); //ParseCommaListExpr();
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
        var next = ParseOrExpr; //ParseForwardPipeExpr;
        ////////////////////////////////

        var constructs = new List<Construct>();

        if(endPred != null)
        {
            while (!endPred(Current.Type))
            {
                constructs.Add(next());
                if(Current.Type != TokenType.Comma)
                {
                    break;
                }

                constructs.Add(Read());
            }
        }
        else
        {
            constructs.Add(next());

            while (Current.Type == TokenType.Comma)
            {
                constructs.Add(Read());
                constructs.Add(next());
            }
        }

        return new(constructs);
    }

    /*
    public Expression ParseCommaListExpr()
        => ParseCommaList() is var cl && cl.Args.Length > 1 ? new ListLiteral(cl.Args) : cl.Args.First();
    public Expression ParseForwardPipeExpr()
        => ParseBinary(t => t is TokenType.PipeOperator ? BinaryOperatorType.PipeForward : null, ParseOrExpr);
    */
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
                var iden = IdenFromToken(Read());
                val = new DotOperator(val, dot, iden, iden.StrValue);
            }
            else if(Get(1).Type == TokenType.StringLiteral)
            {
                var dot = Read();
                var strTok = Read();
                var str = new Literal(strTok, strTok.Value);
                val = new DotOperator(val, dot, str, (str.Value as string)!);
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
            return new StandardCallOperator(val, start, inner, close);
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

            return new BangCallOperator(val, bang, exprs);
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
                return IdenFromToken(Read());

            // literal
            case TokenType.StringLiteral
              or TokenType.NumberLiteral
              or TokenType.FalseLiteral
              or TokenType.TrueLiteral
              or TokenType.InfinityLiteral
              or TokenType.NanLiteral
              or TokenType.NilLiteral:
            {
                var tok = Read();
                return new Literal(tok, tok.Value);
            }

            // (...)
            case TokenType.OpenParen:
            {
                var open = Read();
                var inner = ParseExpression();
                var close = Require(t => t is TokenType.CloseParen, tok => $"Expected matching ')' for '(' (at {open.Span})");

                return new ParenthesizedExpression(open, inner, close);
            }

            case TokenType.OpenTable:
            {
                var table = new List<(Expression key, Expression value)>();

                var open = Read();
                var (content, close) = ParseBlockUpto(t => t is TokenType.CloseTable);

                //var close = Require(t => t is TokenType.CloseTable, tok => $"Expected matching '}}' for '{{' (at {start.Span})");
                return new TableLiteral(open, content, close);
            }

            // [...]
            case TokenType.OpenIndex:
            {
                var open = Read();
                var inner = ParseCommaList(t => t is TokenType.CloseIndex);
                var close = Require(t => t is TokenType.CloseIndex, tok => $"Expected matching ']' for '[' at {open.Span}");

                return new ListLiteral(open, inner, close);
            }
            // |...|
            /*
            case TokenType.VecBound:
            {
                var start = Read();
                var inner = ParseCommaList();
                var close = Require(t => t is TokenType.VecBound, tok => $"Expected matching '|' for '|' at {start.Span}");
                return new VectorLiteral(inner.Args);
            }*/

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