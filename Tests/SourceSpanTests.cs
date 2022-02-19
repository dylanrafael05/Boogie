using NUnit.Framework;
using Boogie.Lang.Core;
using System.Collections.Generic;

namespace Tests;

public class SourceSpanTests
{
    private static IEnumerable<TestCaseData> CombinationCases()
    {
        yield return new(
            new SourceSpan(), 
            new SourceSpan(), 
            new SourceSpan()
        );
        yield return new(
            new SourceSpan(0, 0, 0, 0, 0, 0, ""), 
            new SourceSpan(1, 1, 1, 1, 1, 1, ""), 
            new SourceSpan(0, 1, 0, 1, 0, 1, "")
        );
    }

    [Test, TestCaseSource(nameof(CombinationCases))]
    public void CombinesProperly(SourceSpan a, SourceSpan b, SourceSpan c)
    {
        Assert.AreEqual(a.Combine(b), c);
    }
}
