using Microsoft.CodeAnalysis;
using System;
using System.Collections.Generic;
using System.Linq;

namespace AutoMapCodeRefactoring
{
    internal static class PropertyMatcher
    {
        /// <summary>
        /// Check if the properties match, in the future it might be replaced with an interface
        /// </summary>
        /// <returns>IPropertySymbol</returns>
        internal static IPropertySymbol GetPropertiesMatch(List<IPropertySymbol> source, IPropertySymbol target) =>
            source.FirstOrDefault(x => target.Name.Equals(x.Name, StringComparison.OrdinalIgnoreCase));
    }
}
