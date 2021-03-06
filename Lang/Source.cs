using System.Collections;
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