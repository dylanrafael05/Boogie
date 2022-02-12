using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Boogie.Lang.Generators;

public static class GeneratorUtils
{
    public static string CS(this Accessibility accessibility)
        => accessibility switch
        {
            Accessibility.Public => "public",
            Accessibility.Protected => "protected",
            Accessibility.Internal => "internal",
            Accessibility.Private => "private",
            _ => ""
        };

    public static bool SubtypeOf(this ITypeSymbol type, ITypeSymbol baseType)
    {
        return SymbolEqualityComparer.Default.Equals(type.BaseType)
            || (bool)type?.BaseType?.SubtypeOf(baseType)!;
    }

    public static bool SECEquals(this ISymbol a, ISymbol b)
        => SymbolEqualityComparer.Default.Equals(a, b);
    public static bool SECNullableEquals(this ISymbol a, ISymbol b)
        => SymbolEqualityComparer.IncludeNullability.Equals(a, b);

    public static IEnumerable<(T symbol, S syntax)> WithDeclaringSyntax<T, S>(this IEnumerable<T> symbols) where T : ISymbol where S : SyntaxNode
        => symbols.Select(t => (t, (t.DeclaringSyntaxReferences.First().GetSyntax() as S)!));
}

[Generator]
public class ConstructGenerator : ISourceGenerator
{
    public void Initialize(GeneratorInitializationContext context)
    {
        //throw new NotImplementedException();
    }

    public void Execute(GeneratorExecutionContext context)
    {
        context.AddSource("test.g.cs", "");
        throw new Exception();

        /*
        // Type symbols
        var construct = (ITypeSymbol)context.Compilation
            .GetSymbolsWithName("Construct", SymbolFilter.Type)
            .First();
        var sourcePosition = (ITypeSymbol)context.Compilation
            .GetSymbolsWithName("SourcePosition", SymbolFilter.Type)
            .First();

        // Member symbols
        var construct_GetChildren = construct.GetMembers()
            .Where(m => m.Kind is SymbolKind.Property)
            .Cast<IPropertySymbol>()
            .First(p => p.Name == "GetChildren");

        // All partial subtypes of 'Construct' that do not explicitly override 'Position' and have 'Construct' parameters
        var subtypes = context.Compilation.GlobalNamespace
            .GetTypeMembers()
            .Where(t => t.IsRecord)
            .WithDeclaringSyntax<INamedTypeSymbol, RecordDeclarationSyntax>()
            .Where(t => t.syntax.ParameterList is not null)
            .Where(t => t.syntax.Modifiers.Any(t => t.Kind() is SyntaxKind.PartialKeyword))
            .Where(t => t.symbol.SubtypeOf(construct!))
            .Where(t => !t.symbol.GetMembers().Any(
                m => m is IMethodSymbol ms 
                    && ms.IsOverride 
                    && ms.Name == construct_GetChildren.Name
            ))
            .Select(t => (
                t.symbol, 
                t.syntax, 
                constructMembers: t.symbol
                    .GetMembers()
                    .Where(m => m.Kind is SymbolKind.Property)
                    .Cast<IPropertySymbol>()
                    .Where(p => p.Type.SubtypeOf(construct))
            ))
            .Where(t => t.constructMembers.Any());

        foreach (var (subtype, syntax, constructMembers) in subtypes)
        {
            Console.WriteLine("AAAAAAAAAAAA");

            var newSource = new StringBuilder($@"
using System.Collections.Generic;

namespace {subtype.ContainingNamespace};

{subtype.DeclaredAccessibility.CS()} partial record {subtype.Name}
{{
    public override IEnumerable<Construct> GetChildren()
    {{
        return new Construct[] {{{string.Join(", ", constructMembers.Select(m => "this." + m.Name))}}};
    }}
}}
");

            // ADD SOURCE
            context.AddSource($"{subtype.Name}.generated.cs", newSource.ToString());
        }
        */
    }
}
