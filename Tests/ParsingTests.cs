using Boogie.Lang;
using Boogie.Lang.Core;
using NUnit.Framework;
using System;

namespace Tests;

public class ParsingTests
{
    public static readonly (string input, string output)[] testCases = new[]
    {
        (
            input: @"
a = b + c
",
            output: @"
Block {
    Content = [
        AssignSimple {
            Base = Identifier { Value = a },
            Value = BinaryOperator {
                Type = Add,
                Lhs = Identifier { Value = b },
                Rhs = Identifier { Value = c }
            }
        }
    ]
}
"
        )
    };

    [Test, TestCaseSource(nameof(testCases))]
    public void ParsesProperly((string ipt, string o) testCase)
    {
        //var cons = new Parser(new Lexer(testCase.ipt, "test_input.bgy")).Parse();
        //var consDesc = cons.GetDescription();
        //Console.WriteLine(consDesc);
        //Assert.That(consDesc.EqualsWhitespiceInsensitive(testCase.o), "Construct was not properly parsed.");
    }
}
