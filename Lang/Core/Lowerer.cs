using Boogie.Lang.Utils;
using System;
using System.Collections.Generic;
using System.Text;

namespace Boogie.Lang.Core;

public enum InstructionType
{
    BeginScope,
    EndScope,

    GetInScope,
    SetInScope,
    SetNewInScope,

    GetIndex,
    SetIndex,
    SetNewIndex,

    GetDot,
    SetDot,
    SetNewDot,

    PushLiteral,

    JumpIfNot,
    Jump,

    /// <summary>
    /// WARNING:
    /// Only for use in program builders.
    /// </summary>
    Label
}

public record Instruction(SourceSpan Span, InstructionType Type, object? Argument = null);

public readonly struct LabelID
{
    private readonly int id;
    private LabelID(int id) { this.id = id; }



    private static int uniqueCount = 0;
    public static LabelID NextFree => new(uniqueCount++);

    public override bool Equals(object? obj)
    {
        return obj is LabelID other &&
               id == other.id;
    }

    public override int GetHashCode()
    {
        return 1877310944 + id.GetHashCode();
    }
}
public record struct Label(LabelID ID);

public class Program
{
    public Program(List<Instruction> instructions)
    {
        Instructions = instructions;
    }

    public List<Instruction> Instructions { get; private set; }
    public string GetDescription()
    {
        return string.Join("\n", Instructions.Select((i, index) => $"{index,-10}: {i.Type,-15}{i.Argument.ToRepr()} @ {i.Span}"));
    }
}
public class ProgramBuilder
{
    public List<Instruction> Instructions { get; } = new();
    public Dictionary<LabelID, bool> LabelIDs { get; } = new();

    public static ProgramBuilder New()
        => new ProgramBuilder();

    public ProgramBuilder Add(Instruction instruction)
    {
        Instructions.Add(instruction);
        return this;
    }
    public ProgramBuilder Add(SourceSpan span, InstructionType type, object? argument = null)
        => Add(new(span, type, argument));
    public ProgramBuilder AddLabel(SourceSpan span, LabelID label)
    { 
        if(!LabelIDs.ContainsKey(label))
        {
            throw new InvalidOperationException($"Definition before declaration of label.");
        }
        else if(LabelIDs[label])
        {
            throw new InvalidOperationException($"Double definition of label.");
        }

        return Add(new(span, InstructionType.Label, label));
    }

    public ProgramBuilder DefineLabel(LabelID labelInfo)
    { 
        LabelIDs.Add(labelInfo, false);
        return this;
    }
    public LabelID DefineNewLabel()
    {
        var newId = LabelID.NextFree;
        DefineLabel(newId);
        return newId;
    }

    public Program Build()
    {
        var labelPositions = new Dictionary<LabelID, int>();
        var offset = 0;

        var prog = new List<Instruction>();

        // GET LABELS
        for(int i = 0; i < Instructions.Count; i++)
        {
            var cur = Instructions[i];

            if(cur.Type is InstructionType.Label)
            {
                labelPositions.Add((LabelID)cur.Argument!, i + offset);
                offset--;
            }
        }

        // OUTPUT
        foreach(var i in Instructions)
        {
            if (i.Type is InstructionType.Label)
                continue;
            else if (i.Type is InstructionType.Jump or InstructionType.JumpIfNot)
                prog.Add(new(i.Span, i.Type, labelPositions[(LabelID)i.Argument!]));
            else prog.Add(i);
        }

        return new(prog);
    }
    
    public ProgramBuilder AddCons(Construct construct)
    {
        var span = construct.Span;

        switch(construct)
        {
            case Identifier iden:
            {
                Add(
                    span,
                    InstructionType.GetInScope,
                    iden.StrValue
                );

                break;
            }

            case DotOperator dot:
            {
                this
                .AddCons(dot.Base)
                .Add(
                    span,
                    InstructionType.GetDot,
                    dot.StrValue
                );

                break;
            }

            case AssignBase assign:
            {
                if (assign.Key is DotOperator d)
                {
                    this
                    .AddCons(d.Base)
                    .AddCons(assign.Value)
                    .Add(
                        span,
                        assign is Assign ? InstructionType.SetDot : InstructionType.SetNewDot,
                        d.StrValue
                    );
                }
                else if(assign.Key is Identifier i)
                {
                    this
                    .AddCons(assign.Value)
                    .Add(
                        span,
                        assign is AssignNew ? InstructionType.SetInScope : InstructionType.SetNewInScope,
                        i.StrValue
                    );
                }
                else
                { 
                    throw new InvalidOperationException();
                }

                break;
            }

            case Literal literal:
            {
                Add(
                    span,
                    InstructionType.PushLiteral,
                    literal.Value
                );

                break;
            }

            case IfStatement or ElseifBlock:
            {
                var ifS = construct as IfStatement;
                var eiS = construct as ElseifBlock;

                var condition   = ifS?.Condition    ?? eiS!.Condition;
                var content     = ifS?.Content      ?? eiS!.Content;
                var trailing    = ifS?.Trailing     ?? eiS!.Trailing;

                var trailingLabel = DefineNewLabel();
                var endLabel = DefineNewLabel();

                this
                .AddCons(condition)
                .Add(
                    span,
                    InstructionType.JumpIfNot,
                    endLabel
                )
                .Add(
                    span,
                    InstructionType.BeginScope
                )
                .AddCons(content)
                .Add(
                    span,
                    InstructionType.EndScope
                )
                .Add(
                    span,
                    InstructionType.Jump,
                    trailingLabel
                )
                .AddLabel(span, endLabel)
                .AddCons(trailing)
                .AddLabel(span, trailingLabel);

                break;
            }

            case IfEnd: break;

            case ElseBlock elseBlock:
            {
                AddCons(elseBlock.Content);
                break;
            }

            case Block block:
            {
                foreach(var cons in block.Content)
                {
                    AddCons(cons);
                }

                break;
            }

            // CONTROL GROUPS
            case FullStatement stmt:
            {
                AddCons(stmt.Construct);
                break;
            }


            default:
                throw new InvalidOperationException($"Construct type '{construct.GetType().Name}' is not supported for lowering.");
        }

        return this;
    }
}
