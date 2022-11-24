// XSRoles.prg
// Created by    : fabri
// Creation Date : 3/31/2018 12:09:37 AM
// Created for   :
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.ComponentModel.Composition
USING ICSharpCode.ILSpy
USING ICSharpCode.Decompiler
USING ICSharpCode.Decompiler.CSharp
USING ICSharpCode.Decompiler.CSharp.Syntax
USING ICSharpCode.Decompiler.CSharp.Syntax.PatternMatching
USING ICSharpCode.Decompiler.CSharp.Transforms
USING ICSharpCode.Decompiler.CSharp.OutputVisitor
USING ICSharpCode.Decompiler.TypeSystem
USING Mono.Cecil


BEGIN NAMESPACE ILSpy.XSharpLanguage

	/// <summary>
    /// The XSRoles class.
    /// </summary>

    STATIC CLASS XSRoles
        // Fields
        STATIC INITONLY PUBLIC AliasKeyword := TokenRole{"alias"} AS TokenRole
        STATIC INITONLY PUBLIC Argument := Role<Expression>{"Argument", Expression.Null} AS Role<Expression>
        STATIC INITONLY PUBLIC Arrow := TokenRole{"=>"} AS TokenRole
        STATIC INITONLY PUBLIC @@Assign := TokenRole{":="} AS TokenRole
        STATIC INITONLY PUBLIC Attribute := Role<Attribute>{"Attribute", NULL} AS Role<Attribute>
        STATIC INITONLY PUBLIC AttributeTargetRole := Role<CSharpTokenNode>{"AttributeTarget", CSharpTokenNode.Null} AS Role<CSharpTokenNode>
        STATIC INITONLY PUBLIC BaseType := Role<AstType>{"BaseType", AstType.Null} AS Role<AstType>
        STATIC INITONLY PUBLIC Body := Role<BlockStatement>{"Body", BlockStatement.Null} AS Role<BlockStatement>
        STATIC INITONLY PUBLIC ClassKeyword := TokenRole{"class"} AS TokenRole
        STATIC INITONLY PUBLIC Colon := TokenRole{":"} AS TokenRole
        STATIC INITONLY PUBLIC Comma := TokenRole{","} AS TokenRole
        STATIC INITONLY PUBLIC Comment := Role<Comment>{"Comment", NULL} AS Role<Comment>
        STATIC INITONLY PUBLIC Condition := Role<Expression>{"Condition", Expression.Null} AS Role<Expression>
        STATIC INITONLY PUBLIC Constraint := Role<Constraint>{"Constraint", NULL} AS Role<Constraint>
        STATIC INITONLY PUBLIC ConstraintTypeParameter := Role<SimpleType>{"TypeParameter", SimpleType.Null} AS Role<SimpleType>
        STATIC INITONLY PUBLIC DelegateKeyword := TokenRole{"delegate"} AS TokenRole
        STATIC INITONLY PUBLIC Dot := TokenRole{"."} AS TokenRole
        STATIC INITONLY PUBLIC DoubleColon := TokenRole{"::"} AS TokenRole
        STATIC INITONLY PUBLIC EmbeddedStatement := Role<Statement>{"EmbeddedStatement", Statement.Null} AS Role<Statement>
        STATIC INITONLY PUBLIC EnumKeyword := TokenRole{"enum"} AS TokenRole
        //STATIC INITONLY PUBLIC Error := Role<ErrorNode>{"Error"} AS Role<ErrorNode>
        STATIC INITONLY PUBLIC Expression := Role<Expression>{"Expression", Expression.Null} AS Role<Expression>
        STATIC INITONLY PUBLIC ExternKeyword := TokenRole{"extern"} AS TokenRole
        STATIC INITONLY PUBLIC Identifier := Role<Identifier>{"Identifier", Identifier.Null} AS Role<Identifier>
        STATIC INITONLY PUBLIC InterfaceKeyword := TokenRole{"interface"} AS TokenRole
        STATIC INITONLY PUBLIC LBrace := TokenRole{"{"} AS TokenRole
        STATIC INITONLY PUBLIC LBracket := TokenRole{"["} AS TokenRole
        STATIC INITONLY PUBLIC LChevron := TokenRole{"<"} AS TokenRole
        STATIC INITONLY PUBLIC LPar := TokenRole{"("} AS TokenRole
        STATIC INITONLY PUBLIC NamespaceKeyword := TokenRole{"namespace"} AS TokenRole
        //STATIC INITONLY PUBLIC NewLine := Role<NewLineNode>{"NewLine"} AS Role<NewLineNode>
        STATIC INITONLY PUBLIC Parameter := Role<ParameterDeclaration>{"Parameter", NULL} AS Role<ParameterDeclaration>
        STATIC INITONLY PUBLIC PreProcessorDirective := Role<PreProcessorDirective>{"PreProcessorDirective", NULL} AS Role<PreProcessorDirective>
        STATIC INITONLY PUBLIC RBrace := TokenRole{"}"} AS TokenRole
        STATIC INITONLY PUBLIC RBracket := TokenRole{"]"} AS TokenRole
        STATIC INITONLY PUBLIC RChevron := TokenRole{">"} AS TokenRole
        //STATIC INITONLY PUBLIC Root := AstNode.RootRole AS Role<AstNode>
        STATIC INITONLY PUBLIC RPar := TokenRole{")"} AS TokenRole
        STATIC INITONLY PUBLIC Semicolon := TokenRole{";"} AS TokenRole
        STATIC INITONLY PUBLIC StructKeyword := TokenRole{"struct"} AS TokenRole
        STATIC INITONLY PUBLIC TargetExpression := Role<Expression>{"Target", Expression.Null} AS Role<Expression>
        //STATIC INITONLY PUBLIC Text := Role<TextNode>{"Text"} AS Role<TextNode>
        STATIC INITONLY PUBLIC @@Type := Role<AstType>{"Type", AstType.Null} AS Role<AstType>
        STATIC INITONLY PUBLIC TypeArgument := Role<AstType>{"TypeArgument", AstType.Null} AS Role<AstType>
        STATIC INITONLY PUBLIC TypeMemberRole := Role<EntityDeclaration>{"TypeMember", NULL} AS Role<EntityDeclaration>
        STATIC INITONLY PUBLIC TypeParameter := Role<TypeParameterDeclaration>{"TypeParameter", null} AS Role<TypeParameterDeclaration>
        STATIC INITONLY PUBLIC Variable := Role<VariableInitializer>{"Variable", VariableInitializer.Null} AS Role<VariableInitializer>
        STATIC INITONLY PUBLIC WhereKeyword := TokenRole{"where"} AS TokenRole
        //STATIC INITONLY PUBLIC Whitespace := Role<WhitespaceNode>{"Whitespace"} AS Role<WhitespaceNode>

    END CLASS

END NAMESPACE // ILSpy.XSharpLanguage
