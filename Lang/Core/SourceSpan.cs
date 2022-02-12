using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Boogie.Lang.Core;

/// <summary>
/// A record which describes a position in some source code.
/// Stores both human-readable information and internal information.
/// </summary>
/// <param name="Char">The position of the item in line.</param>
/// <param name="EndChar">The end of the item in line.</param>
/// <param name="Line">The line which the item starts on.</param>
/// <param name="EndLine">The line which the item ends on.</param>
/// <param name="Raw">The raw character index which the item starts on.</param>
/// <param name="EndRaw">The raw character index which the item ends on.</param>
/// <param name="Filename">The filename of the source.</param>
public record struct SourceSpan(int Char, int EndChar, int Line, int EndLine, int Raw, int EndRaw, string Filename)
{
    public override string ToString()
        => $"({Filename}:{Line+1}:{Char})";

    /// <summary>
    /// Combine two source positions into one.
    /// Assumes that this instance and the other provided instance are contiguous.
    /// </summary>
    public SourceSpan Combine(SourceSpan other)
    {
        var min = other.Raw > Raw ? other : this;
        var max = other.EndRaw > EndRaw ? other : this;

        return new(
            min.Char,
            max.EndChar, 
            min.Line, 
            max.EndLine,
            min.Raw,
            max.EndRaw, 
            Filename
        );
    }

    /// <summary>
    /// Combines all the source positions into one.
    /// Assumes that the positions are contiguous, but does not make any assumptions about their ordering.
    /// </summary>
    public static SourceSpan CombineAll(IEnumerable<SourceSpan> positions)
        => positions.Aggregate((a, b) => a.Combine(b));
}