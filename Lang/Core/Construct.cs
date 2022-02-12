using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

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
            );
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
            if (prop.Name == nameof(Span))
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
                str.Append(((string)pVal!).ToLiteral());
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
