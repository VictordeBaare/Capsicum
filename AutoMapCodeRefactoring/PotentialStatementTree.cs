using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace AutoMapCodeRefactoring
{
    internal class PotentialStatementTree
    {
        internal PotentialStatementTree(StatementSyntax root)
            : this(root, new List<MethodDeclarationSyntax>())
        {
        }

        internal PotentialStatementTree(
            StatementSyntax root,
            IEnumerable<MethodDeclarationSyntax> extraStatements)
        {
            Root = root;
            ExtraStatements = extraStatements;
        }

        internal StatementSyntax Root { get;}

        internal IEnumerable<MethodDeclarationSyntax> ExtraStatements { get; }
    }
}
