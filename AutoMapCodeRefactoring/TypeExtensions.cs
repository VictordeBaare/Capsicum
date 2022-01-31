namespace AutoMapCodeRefactoring
{
    using Microsoft.CodeAnalysis;

    internal static class TypeExtensions
    {
        /// <summary>
        /// Check to see if the typesymbol is a simpletype
        /// </summary>
        /// <param name="type"></param>
        /// <returns></returns>
        public static bool IsSimpleType(this ITypeSymbol type)
        {
            if (type.IsNullable())
            {
                type = type.GetCollectionGenericType();
            }

            switch (type.SpecialType)
            {
                case SpecialType.System_Boolean:
                case SpecialType.System_SByte:
                case SpecialType.System_Int16:
                case SpecialType.System_Int32:
                case SpecialType.System_Int64:
                case SpecialType.System_Byte:
                case SpecialType.System_UInt16:
                case SpecialType.System_UInt32:
                case SpecialType.System_UInt64:
                case SpecialType.System_Single:
                case SpecialType.System_Double:
                case SpecialType.System_Char:
                case SpecialType.System_String:
                case SpecialType.System_Object:
                case SpecialType.System_Decimal:
                case SpecialType.System_DateTime:
                case SpecialType.System_Enum:
                    return true;
            }


            switch (type.TypeKind)
            {
                case TypeKind.Enum:
                    return true;
            }

            if (type.Name == "Guid" && type.ContainingNamespace.Name == "System")
            {
                return true;
            }


            return false;
        }

        /// <summary>
        /// Check to see if a type is nullable.
        /// </summary>
        /// <param name="type"></param>
        /// <returns></returns>
        public static bool IsNullable(this ITypeSymbol type)
        {
            return type.TypeKind == TypeKind.Struct && type.Name == "Nullable";
        }

        /// <summary>
        /// In case that a type is has an underlying type, this method will return the first underlying type.
        /// Usefull with for example nullable types Nullabe or with Lists.
        /// In case that there is no underlying type the type itself is returned
        /// </summary>
        /// <param name="type"></param>
        /// <returns>ITypeSymbol</returns>
        public static ITypeSymbol? GetUnderlyingType(this ITypeSymbol type)
        {
            if (type is INamedTypeSymbol namedTypeSymbol && namedTypeSymbol.TypeArguments.Any())
            {
                return namedTypeSymbol.TypeArguments.FirstOrDefault();
            }

            return type;
        }

        /// <summary>
        /// Check is a type is an enum.
        /// Nullable is accounted for and will be checked.
        /// </summary>
        /// <param name="type"></param>
        /// <returns></returns>
        public static bool IsEnum(this ITypeSymbol type)
        {
            return type.GetUnderlyingType()?.TypeKind == TypeKind.Enum;
        }

        /// <summary>
        /// Check to see if a type is a collection. A string is an array of chars, so this one is explicitly ignored here.
        /// </summary>
        /// <param name="typeSymbol"></param>
        /// <returns></returns>
        public static bool IsCollection(this ITypeSymbol typeSymbol)
        {
            return typeSymbol.Name != "String" && (typeSymbol.Kind == SymbolKind.ArrayType || typeSymbol.OriginalDefinition.AllInterfaces.Any(x => x.Name == "IEnumerable" && x.ToDisplayString() == "System.Collections.IEnumerable"));
        }

        public static ITypeSymbol? GetCollectionGenericType(this ITypeSymbol typeSymbol)
        {
            if (typeSymbol is IArrayTypeSymbol arrayTypeSymbol)
            {
                return arrayTypeSymbol.ElementType;
            }

            if (typeSymbol is INamedTypeSymbol namedTypeSymbol)
            {
                return namedTypeSymbol.TypeArguments[0];
            }

            return null;
        }
    }
}
