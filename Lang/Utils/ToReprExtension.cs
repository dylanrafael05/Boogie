using System;
using System.Collections.Generic;
using System.Text;

namespace Boogie.Lang.Utils;

public static class ToReprExtension
{
    private static string ToLiteral(this string input)
    {
        StringBuilder literal = new StringBuilder(input.Length + 2);
        foreach (var c in input)
        {
            literal.Append(c.ToLiteral());
        }
        return literal.ToString();
    }

    private static string ToLiteral(this char c)
        => c switch
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
            _ when c >= 0x20 && c <= 0x7e => c.ToString(),
            _ => $@"\u{(int)c:X4}"
        };

    public static string ToRepr(this object? o)
        => o switch
        {
            string s => s.ToRepr(),
            char c => c.ToRepr(),
            null => "null",
            _ => o.ToString()
        };
    public static string ToRepr(this string s)
        => $"\"{s.ToLiteral()}\"";
    public static string ToRepr(this char c)
        => $"'{c}'";
}
