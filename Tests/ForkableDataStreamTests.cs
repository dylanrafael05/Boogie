using NUnit.Framework;
using Boogie.Lang;
using Boogie.Model;
using System.Collections.Generic;
using System.Linq;

namespace Tests;
public class ForkableDataStreamTests
{
    public static IEnumerable<object?> Enumerate(ForkableDataStream<object?> dataStream)
    {
        while(dataStream.Get() != dataStream.OnDry)
        {
            yield return dataStream.Read();
        }
    }

    private static readonly object?[][] testCases = new object?[][]
    {
        new object?[] { 1, 2, 3, 4 },
        new object?[] { true, false, true },
        new object?[] { null, "test" },
        new object?[] { 'a', "wao!" }
    };
    private const string SafeOnDry = "####################";

    [Test, TestCaseSource("testCases")]
    public void FollowsLength(object?[] testCase)
    {
        var cnt = 0;

        var forkable = new ForkableDataStream<object?>(testCase, SafeOnDry);
        foreach (var _ in Enumerate(forkable))
        {
            cnt++;
            Assert.LessOrEqual(cnt, testCase.Length);
        }

        Assert.AreEqual(cnt, testCase.Length);
    }

    [Test, TestCaseSource("testCases")]
    public void KeepsSequence(object?[] testCase)
    {
        var forkable = new ForkableDataStream<object?>(testCase, SafeOnDry);
        Assert.That(Enumerate(forkable).SequenceEqual(testCase));
    }
}