namespace AutoMapCodeRefactoringTests
{
    using AutoMapCodeRefactoring;
    using Microsoft.VisualStudio.TestTools.UnitTesting;
    using System.Threading.Tasks;

    [TestClass]
    public class AutoMappingIntegrationTests
    {
        [TestMethod]
        public async Task HappyFlowTestWithOnlyBasicProperties()
        {
            await CSharpCodeRefactoringVerifier<AutoMapRefactoring>.VerifyRefactoringAsync(TestCases.PropertyClass_BeforeMapping, TestCases.PropertyClass_AfterMapping);
        }

        [TestMethod]
        public async Task HappyFlowTestWithImmutableObjects()
        {
            await CSharpCodeRefactoringVerifier<AutoMapRefactoring>.VerifyRefactoringAsync(TestCases.ConstructorPropertyClass_BeforeMapping, TestCases.ConstructorPropertyClass_AfterMapping);
        }

        [TestMethod]
        public async Task EnumTest_Nullable()
        {
            await CSharpCodeRefactoringVerifier<AutoMapRefactoring>.VerifyRefactoringAsync(TestCases.Enum_Nullable_BeforeMapping, TestCases.Enum_Nullable_AfterMapping);
        }

        [TestMethod]
        public async Task EnumTest_NonNullable()
        {
            await CSharpCodeRefactoringVerifier<AutoMapRefactoring>.VerifyRefactoringAsync(TestCases.Enum_NonNullable_BeforeMapping, TestCases.Enum_NonNullable_AfterMapping);
        }
    }
}
