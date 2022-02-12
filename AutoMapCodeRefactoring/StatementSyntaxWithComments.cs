using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace AutoMapCodeRefactoring
{
    internal class StatementSyntaxWithComments
    {
        internal StatementSyntaxWithComments(StatementSyntax statement, List<string> comments)
        {
            Statement = statement;
            Comments = comments;
        }

        internal StatementSyntax Statement { get; }

        internal List<string> Comments { get; }
    }
}
