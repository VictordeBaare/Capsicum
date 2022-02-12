namespace AutoMapCodeRefactoring
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using Microsoft.CodeAnalysis.Editing;
    using Microsoft.CodeAnalysis.Formatting;
    using Microsoft.CodeAnalysis;
    using Microsoft.CodeAnalysis.CodeActions;
    using Microsoft.CodeAnalysis.CodeRefactorings;
    using Microsoft.CodeAnalysis.CSharp;
    using Microsoft.CodeAnalysis.CSharp.Syntax;
    using System.Composition;
    using System.Threading;
    using System.Threading.Tasks;

    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(AutoMapRefactoring)), Shared]
    public class AutoMapRefactoring : CodeRefactoringProvider
    {
        public sealed override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var node = root.FindNode(context.Span);

            ImplementRefactoringCommand(context, node);
        }

        /// <summary>
        /// Register the code action.
        /// Current registrations is for:
        /// Method with 1 return type and 1 parameter.
        /// </summary>
        /// <param name="context"></param>
        /// <param name="node"></param>
        /// <returns></returns>
        private static void ImplementRefactoringCommand(CodeRefactoringContext context, SyntaxNode node)
        {
            if (node is MethodDeclarationSyntax methodDeclarationSyntax &&
                (!(methodDeclarationSyntax.ReturnType is PredefinedTypeSyntax returnType) || !returnType.Keyword.IsKind(SyntaxKind.VoidKeyword)) &&
                methodDeclarationSyntax.ParameterList.Parameters.Count == 1)
            {
                context.RegisterRefactoring(CodeAction.Create("Create Mapping", async token => await GetMappingBody(context.Document, methodDeclarationSyntax, token).ConfigureAwait(false)));
            }
        }

        /// <summary>
        /// Start method of the refactoring from a method with 1 parameter and 1 returntype. 
        /// </summary>
        /// <param name="contextDocument"></param>
        /// <param name="methodDeclaration"></param>
        /// <param name="token"></param>
        /// <returns></returns>
        private static async Task<Document> GetMappingBody(Document contextDocument, MethodDeclarationSyntax methodDeclaration, CancellationToken token)
        {
            var semanticModelAsync = await contextDocument.GetSemanticModelAsync(token).ConfigureAwait(false);
            var syntaxRoot = await contextDocument.GetSyntaxRootAsync(token).ConfigureAwait(false);
            var declaredMethodSymbol = semanticModelAsync.GetDeclaredSymbol(methodDeclaration, token);

            if (syntaxRoot == null || declaredMethodSymbol == null)
            {
                return contextDocument;
            }

            var syntaxGenerator = SyntaxGenerator.GetGenerator(contextDocument);

            var returnType = declaredMethodSymbol.ReturnType;
            var inputParameter = declaredMethodSymbol.Parameters[0];
            PotentialStatementTree statement;

            // If both types are enums we will do an enum mapping based on a switch statement.
            // The switch statement is used because it is explicit.
            if (returnType.IsEnum() && inputParameter.Type.IsEnum())
            {
                statement = CreateEnumSwitchStatementMapping(inputParameter, returnType);
            }
            //Type is not an enum but a object or list.
            else
            {
                statement = CreateNewObjectWithMapping(syntaxGenerator, inputParameter, returnType);
            }

            var newBlock = SyntaxFactory.Block(statement.Root).WithAdditionalAnnotations(Formatter.Annotation);
            var newMethod = methodDeclaration.WithBody(newBlock).WithExpressionBody(null).WithSemicolonToken(default);
            
            var list = new List<MethodDeclarationSyntax> { newMethod }.Concat(statement.ExtraStatements);
            
            var newRoot = syntaxRoot.ReplaceNode(methodDeclaration, list);

            return contextDocument.WithSyntaxRoot(newRoot);
        }

        private static StatementSyntax AddCommentsToStatement(StatementSyntax statement, List<string> comments)
        {
            //Add the comments before the mapping.
            return statement.WithLeadingTrivia(SyntaxFactory.TriviaList(CreateCommentList(comments)))
                            .WithAdditionalAnnotations(Formatter.Annotation);
        }

        /// <summary>
        /// Create a new object. This could be list or a complexobject.
        /// </summary>
        /// <param name="syntaxGenerator"></param>
        /// <param name="inputParameter"></param>
        /// <param name="returnType"></param>
        /// <param name="comments"></param>
        /// <returns></returns>
        private static PotentialStatementTree CreateNewObjectWithMapping(
            SyntaxGenerator syntaxGenerator,
            IParameterSymbol inputParameter,
            ITypeSymbol returnType)
        {
            var otherMethods = new List<MethodDeclarationSyntax>();
            var comments = new List<string>();
            var expression = GetExpressionSyntax(syntaxGenerator,
                inputParameter.Type,
                returnType,
                (ExpressionSyntax)syntaxGenerator.IdentifierName(inputParameter.Name),
                ref otherMethods,
                ref comments);

            var statement = (StatementSyntax)syntaxGenerator.ReturnStatement(expression);
            statement = AddCommentsToStatement(statement, comments);
            return new PotentialStatementTree(statement, otherMethods);
        }

        /// <summary>
        /// Base for the switch statement for the enum mapping.
        /// </summary>
        /// <param name="inputParameter"></param>
        /// <param name="returnType"></param>
        /// <param name="identifier"></param>
        /// <returns></returns>
        private static PotentialStatementTree CreateEnumSwitchStatementMapping(
            ITypeSymbol inputParameter,
            ITypeSymbol returnType,
            string identifier)
        {
            var comments = new List<string>();

            var identifierInputParameterName = SyntaxFactory.IdentifierName(identifier);

            var enumMembersInputParameter = GetEnumMembers(inputParameter.GetUnderlyingType());
            var enumMembersReturnType = GetEnumMembers(returnType.GetUnderlyingType());
            var statement = (StatementSyntax)SyntaxFactory
                .SwitchStatement(identifierInputParameterName)
                .WithSections(SyntaxFactory.List(GetSectionsForSwitchStatement(
                    enumMembersInputParameter,
                    enumMembersReturnType,
                    identifierInputParameterName,
                    inputParameter,
                    returnType,
                    ref comments)));
            statement = AddCommentsToStatement(statement, comments);

            return new PotentialStatementTree(statement);
        }

        /// <summary>
        /// Base for the switch statement for the enum mapping.
        /// </summary>
        /// <param name="inputParameter"></param>
        /// <param name="returnType"></param>
        /// <returns></returns>
        private static PotentialStatementTree CreateEnumSwitchStatementMapping(
            IParameterSymbol inputParameter,
            ITypeSymbol returnType)
        {
            return CreateEnumSwitchStatementMapping(inputParameter.Type, returnType, inputParameter.Name);
        }

        /// <summary>
        /// Get the enummembers. This is filtered on the field symbol so no methods are added.
        /// </summary>
        /// <param name="typeSymbol"></param>
        /// <returns></returns>
        private static List<IFieldSymbol> GetEnumMembers(ITypeSymbol? typeSymbol)
        {
            if(typeSymbol == null)
            {
                return Enumerable.Empty<IFieldSymbol>().ToList();
            }

            var underlying = typeSymbol.GetUnderlyingType();

            if(underlying == null)
            {
                return Enumerable.Empty<IFieldSymbol>().ToList();
            }

            return underlying.GetMembers()
                .Where(x => x is IFieldSymbol)
                .Cast<IFieldSymbol>()
                .ToList();
        }

        /// <summary>
        /// Get all the switch cases.
        /// This includes a possible NULL mapping
        /// Default is an exception statement.
        /// </summary>
        /// <param name="inputEnumMembers"></param>
        /// <param name="returnTypeEnumMembers"></param>
        /// <param name="inputParamaterSyntax"></param>
        /// <param name="inputParameter"></param>
        /// <param name="returnType"></param>
        /// <param name="comments"></param>
        /// <returns></returns>
        private static IEnumerable<SwitchSectionSyntax> GetSectionsForSwitchStatement(
            IList<IFieldSymbol> inputEnumMembers,
            IList<IFieldSymbol> returnTypeEnumMembers,
            ExpressionSyntax inputParamaterSyntax,
            ITypeSymbol inputParameter,
            ITypeSymbol returnType,
            ref List<string> comments)
        {
            var syntaxes = new List<SwitchSectionSyntax>();
            var identifierNameInputParameterType = SyntaxFactory.IdentifierName(inputParameter.GetUnderlyingType().Name);
            var identifierNameReturnType = SyntaxFactory.IdentifierName(returnType.GetUnderlyingType().Name);

            foreach (var inputParameterEnumMember in inputEnumMembers)
            {
                var matchingTarget = returnTypeEnumMembers.SingleOrDefault(x =>
                    x.Name.Equals(inputParameterEnumMember.Name, StringComparison.CurrentCultureIgnoreCase) ||
                    (x.ConstantValue != null && x.ConstantValue.Equals(inputParameterEnumMember.ConstantValue)));

                if (matchingTarget != null)
                {
                    syntaxes.Add(SyntaxFactory.SwitchSection()
                        .WithLabels(SyntaxFactory.SingletonList<SwitchLabelSyntax>(
                                SyntaxFactory.CaseSwitchLabel(
                                    SyntaxFactory.MemberAccessExpression(
                                        SyntaxKind.SimpleMemberAccessExpression,
                                        identifierNameInputParameterType,
                                        SyntaxFactory.IdentifierName(inputParameterEnumMember.Name))))
                        ).WithStatements(SyntaxFactory.SingletonList<StatementSyntax>(
                            SyntaxFactory.ReturnStatement(
                                SyntaxFactory.MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    identifierNameReturnType,
                                    SyntaxFactory.IdentifierName(matchingTarget.Name))))));

                    if (!matchingTarget.Name.Equals(inputParameterEnumMember.Name, StringComparison.CurrentCultureIgnoreCase))
                    {
                        comments.Add($"EnumMember: {inputParameterEnumMember.Name} differs in name but has the same value.");
                    }
                }
                else
                {
                    comments.Add($"EnumMember: {inputParameterEnumMember.Name} could not be matched");
                }
            }

            if (returnType.IsNullable() && inputParameter.IsNullable())
            {
                syntaxes.Add(SyntaxFactory.SwitchSection()
                    .WithLabels(SyntaxFactory.SingletonList<SwitchLabelSyntax>(
                        SyntaxFactory.CaseSwitchLabel(SyntaxFactory.LiteralExpression(
                            SyntaxKind.NullLiteralExpression)))
                    ).WithStatements(SyntaxFactory.SingletonList<StatementSyntax>(
                        SyntaxFactory.ReturnStatement(SyntaxFactory.LiteralExpression(
                            SyntaxKind.NullLiteralExpression)))));
            }

            syntaxes.Add(SyntaxFactory.SwitchSection()
                .WithLabels(SyntaxFactory.SingletonList<SwitchLabelSyntax>(SyntaxFactory.DefaultSwitchLabel()))
                .WithStatements(SyntaxFactory.SingletonList<StatementSyntax>(
                    SyntaxFactory.ThrowStatement(ThrowAgrumentOutOfRangeExpression(inputParamaterSyntax)))));

            return syntaxes;
        }

        private static IEnumerable<SyntaxTrivia> CreateCommentList(List<string> potentialCommentsToAdd)
        {
            foreach (var comment in potentialCommentsToAdd)
            {
                yield return SyntaxFactory.Comment(comment.StartsWith("//") ? comment : $"//{comment}");
                yield return SyntaxFactory.EndOfLine(Environment.NewLine);
            }
        }

        /// <summary>
        /// Start gathering the syntax that needs to be used to replace the old node.
        /// This method starts with the source (Parameter) and target (returntype)
        /// This method can be used as a recursive method to keep on mapping while you keep finding complex/list objects
        /// The moment the source and target are simple types you can stop the recursion.
        /// </summary>
        /// <param name="syntaxGenerator">The generator needed to create new syntax expressions.</param>
        /// <param name="source">The source from which you are mapping</param>
        /// <param name="target">Your target to which you are mapping</param>
        /// <param name="expression">Expression, in the first call this is the source identifier name. In other words the base from which we will start our mapping.</param>
        /// <param name="comments"></param>
        /// <returns></returns>
        private static ExpressionSyntax GetExpressionSyntax(
            SyntaxGenerator syntaxGenerator,
            ITypeSymbol source,
            ITypeSymbol target,
            ExpressionSyntax expression,
            ref List<MethodDeclarationSyntax> otherMethods,
            ref List<string> comments)
        {
            //If both target and source are collections then start mapping the collection.
            if (target.IsCollection() && source.IsCollection())
            {
                return CreateCollectionMappingExpression(
                    syntaxGenerator,
                    source,
                    target,
                    expression,
                    ref otherMethods,
                    ref comments);
            }

            var sourceProperties = source.GetMembers().Where(x => !x.IsStatic && !x.IsImplicitlyDeclared && x is IPropertySymbol && x.DeclaredAccessibility == Accessibility.Public).Cast<IPropertySymbol>().ToList();

            //If both target and source are not simple types then lets start to create a new object!
            if (!target.IsSimpleType() && !source.IsSimpleType())
            {
                if (target is INamedTypeSymbol namedType)
                {
                    var constructorsWithParameters = namedType.Constructors.Select(x => x.Parameters);
                    var foundConstructorParams = constructorsWithParameters.Where(x => x.Length == sourceProperties.Count).FirstOrDefault(x =>
                        x.All(t => sourceProperties.Any(y => y.Name.Equals(t.Name, StringComparison.OrdinalIgnoreCase))));

                    if (foundConstructorParams != null)
                    {
                        return TryToMapWithConstructor(
                            syntaxGenerator,
                            namedType,
                            foundConstructorParams.ToList(),
                            sourceProperties,
                            expression,
                            ref otherMethods,
                            ref comments);
                    }
                }

                var newObject = GetCreateNewObjectSyntax(syntaxGenerator, target);

                //Get the properties of the target and source. Ignore properties which are implicitly declared and non public properties.
                //GetMembers also returns methods, the IPropertySymbol is used to filter these away.
                var targetProperties = target.GetMembers().Where(x => !x.IsStatic && !x.IsImplicitlyDeclared && x is IPropertySymbol && x.DeclaredAccessibility == Accessibility.Public).Cast<IPropertySymbol>().ToList();

                var results = new List<AssignmentExpressionSyntax>();

                //Try to match the properties from the target and the source
                foreach (var targetProperty in targetProperties)
                {
                    //Match the properties on name. For now we do this only with ignoring case. In the future this can be updated to a more complex variant.
                    var matchedProp = PropertyMatcher.GetPropertiesMatch(sourceProperties, targetProperty);
                    //It should not be readonly, readonly objects cannot be set.
                    if (matchedProp != null && targetProperty.SetMethod != null)
                    {
                        //Create the MemberAccess expression for example: source.Property
                        var memberAccessExpression = (ExpressionSyntax)syntaxGenerator.MemberAccessExpression(expression, matchedProp.Name);

                        //If the target is not nullable and the source is nullable then add the safety check with an argumentnull exception
                        if (matchedProp.Type.IsNullable() && !targetProperty.Type.IsNullable())
                        {
                            //Create an ?? operator. The SyntaxKind is used for that.
                            memberAccessExpression = SyntaxFactory.BinaryExpression(SyntaxKind.CoalesceExpression,
                                memberAccessExpression,
                                SyntaxFactory.ThrowExpression(ThrowAgrumentNullExpression(memberAccessExpression)));
                        }

                        //Check if you are mapping a complex or simple type. Complex objects need an initializer so call the method in recursion
                        if (matchedProp.Type.IsSimpleType() && targetProperty.Type.IsSimpleType())
                        {
                            //If both target and source are enums then create a mapping for the enum.
                            //Store this as an extra method.
                            if (targetProperty.Type.IsEnum() && matchedProp.Type.IsEnum())
                            {
                                var mappingMethodName = $"Map{matchedProp.Type.Name}";
                                var statement = CreateEnumSwitchStatementMapping(matchedProp.Type, targetProperty.Type, matchedProp.Name.ToLower()).Root;
                                var method = SyntaxFactory.MethodDeclaration(
                                    SyntaxFactory.IdentifierName(targetProperty.Type.Name),
                                    SyntaxFactory.Identifier(mappingMethodName))
                                    .WithModifiers(SyntaxFactory.TokenList(new[]
                                    {
                                        SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                                        SyntaxFactory.Token(SyntaxKind.StaticKeyword)
                                    }))
                                    .WithParameterList(SyntaxFactory.ParameterList(
                                        SyntaxFactory.SingletonSeparatedList(SyntaxFactory.Parameter(
                                            SyntaxFactory.Identifier(matchedProp.Name.ToLower()))
                                        .WithType(SyntaxFactory.IdentifierName(matchedProp.Type.Name)))))
                                    .WithBody(SyntaxFactory.Block(statement));

                                var t = method.ToFullString();

                                otherMethods.Add(method); 
                                var invocation = ((InvocationExpressionSyntax)syntaxGenerator
                                    .InvocationExpression(syntaxGenerator.IdentifierName(mappingMethodName)))
                                    .WithArgumentList(SyntaxFactory.ArgumentList(
                                        SyntaxFactory.SingletonSeparatedList(
                                            SyntaxFactory.Argument(memberAccessExpression))));

                                results.Add(
                                    SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                                        SyntaxFactory.IdentifierName(targetProperty.Name),
                                        invocation
                                    ));
                            }
                            else
                            {
                                //Create the assignment So (Target) Property = source.Property
                                results.Add(
                                    SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                                        SyntaxFactory.IdentifierName(targetProperty.Name),
                                        memberAccessExpression
                                    ));
                            }
                        }
                        else
                        {
                            //Get the complex object for example: new TargetChild { Property = source.PropertyChild.Property }
                            var syntax = GetExpressionSyntax(
                                syntaxGenerator,
                                matchedProp.Type,
                                targetProperty.Type,
                                memberAccessExpression,
                                ref otherMethods,
                                ref comments);
                            //Create the assignment so (Target) PropertyChild = new TargetChild { Property = source.PropertyChild.Property }
                            results.Add(
                                SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                                    SyntaxFactory.IdentifierName(targetProperty.Name),
                                    syntax
                                ));
                        }
                    }
                    else
                    {
                        comments.Add($"{expression.SyntaxTree}: {targetProperty.Name} could not be matched");
                    }
                }

                var initializerExpression = SyntaxFactory.InitializerExpression(SyntaxKind.ObjectInitializerExpression,
                    new SeparatedSyntaxList<ExpressionSyntax>().AddRange(results)).WithLeadingTrivia(SyntaxTriviaList.Create(SyntaxFactory.EndOfLine(Environment.NewLine)));
                return newObject.WithInitializer(initializerExpression);
            }

            return expression;
        }

        private static ExpressionSyntax TryToMapWithConstructor(
            SyntaxGenerator syntaxGenerator,
            INamedTypeSymbol target,
            List<IParameterSymbol> foundConstructorParams,
            List<IPropertySymbol> sourceProperties,
            ExpressionSyntax expressionSyntax,
            ref List<MethodDeclarationSyntax> otherMethods,
            ref List<string> comments)
        {
            var createExpression = GetCreateNewObjectSyntax(syntaxGenerator, target);
            var constructorArguments = SyntaxFactory.ArgumentList();
            foreach (var constructorParam in foundConstructorParams)
            {
                var matchedProp = sourceProperties.First(x => constructorParam.Name.Equals(x.Name, StringComparison.OrdinalIgnoreCase));

                var node = GetExpressionSyntax(
                    syntaxGenerator,
                    matchedProp.Type,
                    constructorParam.Type,
                    (ExpressionSyntax)syntaxGenerator.MemberAccessExpression(expressionSyntax, matchedProp.Name),
                    ref otherMethods,
                    ref comments);
                constructorArguments = constructorArguments.AddArguments(SyntaxFactory.Argument(node));
            }

            return createExpression.WithArgumentList(constructorArguments);
        }

        private static ObjectCreationExpressionSyntax ThrowAgrumentNullExpression(ExpressionSyntax argument)
        {
            return ExceptionOfType(argument, "ArgumentNullException");
        }

        private static ObjectCreationExpressionSyntax ThrowAgrumentOutOfRangeExpression(ExpressionSyntax argument)
        {
            return ExceptionOfType(argument, "ArgumentOutOfRangeException");
        }

        private static ObjectCreationExpressionSyntax ExceptionOfType(ExpressionSyntax argument, string exceptionName)
        {
            var nameOfArgument =
                SyntaxFactory.InvocationExpression(SyntaxFactory.IdentifierName("nameof")).WithArgumentList(
                    SyntaxFactory.ArgumentList(SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Argument(argument))));

            return SyntaxFactory
                .ObjectCreationExpression(SyntaxFactory.IdentifierName(exceptionName))
                .WithArgumentList(SyntaxFactory.ArgumentList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Argument(nameOfArgument))));
        }

        private static ExpressionSyntax CreateCollectionMappingExpression(
            SyntaxGenerator syntaxGenerator,
            ITypeSymbol source,
            ITypeSymbol target,
            ExpressionSyntax expression,
            ref List<MethodDeclarationSyntax> otherMethods,
            ref List<string> comments)
        {
            //Get the generic type
            var targetGenericType = target.GetCollectionGenericType();
            var sourceGenericType = source.GetCollectionGenericType();

            if (targetGenericType != null && targetGenericType.IsSimpleType() && sourceGenericType != null && sourceGenericType.IsSimpleType())
            {
                return expression;
            }

            //Decide a lambda name for the select statement
            var lambdaName = $"{sourceGenericType.Name.ToLower()}Item";
            var expressionSyntaxForLambdaName = SyntaxFactory.IdentifierName(lambdaName);
            //Get the mapping from the generic type
            var mappingExpression = GetExpressionSyntax(
                syntaxGenerator,
                sourceGenericType,
                targetGenericType,
                expressionSyntaxForLambdaName,
                ref otherMethods,
                ref comments);

            var valueReturningLambdaExpression =
                (ExpressionSyntax)syntaxGenerator.ValueReturningLambdaExpression(lambdaName, mappingExpression);

            //Create a select statement invocation
            var invocation = SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, expression,
                    SyntaxFactory.IdentifierName("Select")));
            //Add the arguments to the select statement invocation
            invocation = invocation.WithArgumentList(SyntaxFactory.ArgumentList(
                SyntaxFactory.SeparatedList(new List<ArgumentSyntax>
                    {SyntaxFactory.Argument(valueReturningLambdaExpression)})));
            //Perform a ToList. This can be expanded with a possible ToArray or other To Types.
            return (ExpressionSyntax)syntaxGenerator.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, invocation,
                    SyntaxFactory.IdentifierName("ToList")));
        }

        /// <summary>
        /// Get the new object expression for the given typesymbol.
        /// </summary>
        /// <param name="syntaxGenerator"></param>
        /// <param name="typeSymbol"></param>
        /// <returns></returns>
        private static ObjectCreationExpressionSyntax GetCreateNewObjectSyntax(SyntaxGenerator syntaxGenerator, ITypeSymbol typeSymbol)
        {
            //Create Object
            var newTypeExpression = (TypeSyntax)syntaxGenerator.TypeExpression(typeSymbol);
            return SyntaxFactory.ObjectCreationExpression(newTypeExpression);
        }
    }
}
