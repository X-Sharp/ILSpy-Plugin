// XSharpHighlightingTokenWriter.prg
// Created by    : fabri
// Creation Date : 4/1/2018 8:56:43 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Collections
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
USING ICSharpCode.AvalonEdit.Highlighting


BEGIN NAMESPACE ILSpy.XSharpLanguage

	/// <summary>
		/// The XSharpHighlightingTokenWriter class.
	/// </summary>
	
	
	
	
	INTERNAL CLASS XSharpHighlightingTokenWriter INHERIT DecoratingTokenWriter
		// Fields
		PRIVATE accessorKeywordsColor AS HighlightingColor
		PRIVATE attributeKeywordsColor AS HighlightingColor
		PRIVATE checkedKeywordColor AS HighlightingColor
		PRIVATE delegateTypeColor AS HighlightingColor
		PRIVATE enumerationTypeColor AS HighlightingColor
		PRIVATE exceptionKeywordsColor AS HighlightingColor
		PRIVATE fieldAccessColor AS HighlightingColor
		PRIVATE fldDeclarationColor AS HighlightingColor
		PRIVATE gotoKeywordsColor AS HighlightingColor
		PRIVATE interfaceTypeColor AS HighlightingColor
		PRIVATE methodCallColor AS HighlightingColor
		PRIVATE methodDeclarationColor AS HighlightingColor
		PRIVATE modifiersColor AS HighlightingColor
		PRIVATE namespaceKeywordsColor AS HighlightingColor
		PRIVATE nodeStack AS System.Collections.Generic.Stack<AstNode>
		PRIVATE operatorKeywordsColor AS HighlightingColor
		PRIVATE parameterModifierColor AS HighlightingColor
		PRIVATE queryKeywordsColor AS HighlightingColor
		PRIVATE referenceTypeColor AS HighlightingColor
		PRIVATE referenceTypeKeywordsColor AS HighlightingColor
		PRIVATE structureKeywordsColor AS HighlightingColor
		PRIVATE textOutput AS ISmartTextOutput
		PRIVATE thisKeywordColor AS HighlightingColor
		PRIVATE trueKeywordColor AS HighlightingColor
		PRIVATE typeKeywordsColor AS HighlightingColor
		PRIVATE typeParameterTypeColor AS HighlightingColor
		PRIVATE unsafeKeywordsColor AS HighlightingColor
		PRIVATE valueKeywordColor AS HighlightingColor
		PRIVATE valueTypeColor AS HighlightingColor
		PRIVATE valueTypeKeywordsColor AS HighlightingColor
		PRIVATE visibilityKeywordsColor AS HighlightingColor
		PRIVATE commentColor AS HighlightingColor
		//
		PRIVATE specialTypeNames AS Hashtable
		PRIVATE beginend AS LOGIC
		PRIVATE xsSettings AS XSharpOptions
		
		PUBLIC PROPERTY Settings AS XSharpOptions GET SELF:xsSettings
		
		// Methods
		CONSTRUCTOR(decoratedWriter AS TokenWriter, textOutput AS ISmartTextOutput, settings AS XSharpOptions)//Inline call to base() in C#
			SUPER(decoratedWriter)
			LOCAL definition AS IHighlightingDefinition
			//
			SELF:xsSettings := settings
			SELF:nodeStack := System.Collections.Generic.Stack<AstNode>{}
			SELF:textOutput := textOutput
			definition := HighlightingManager.@@Instance:GetDefinition("C#")
			SELF:visibilityKeywordsColor := definition:GetNamedColor("Visibility")
			SELF:namespaceKeywordsColor := definition:GetNamedColor("NamespaceKeywords")
			SELF:structureKeywordsColor := definition:GetNamedColor("Keywords")
			SELF:gotoKeywordsColor := definition:GetNamedColor("GotoKeywords")
			SELF:queryKeywordsColor := definition:GetNamedColor("QueryKeywords")
			SELF:exceptionKeywordsColor := definition:GetNamedColor("ExceptionKeywords")
			SELF:checkedKeywordColor := definition:GetNamedColor("CheckedKeyword")
			SELF:unsafeKeywordsColor := definition:GetNamedColor("UnsafeKeywords")
			SELF:valueTypeKeywordsColor := definition:GetNamedColor("ValueTypeKeywords")
			SELF:referenceTypeKeywordsColor := definition:GetNamedColor("ReferenceTypeKeywords")
			SELF:operatorKeywordsColor := definition:GetNamedColor("OperatorKeywords")
			SELF:parameterModifierColor := definition:GetNamedColor("ParameterModifiers")
			SELF:modifiersColor := definition:GetNamedColor("Modifiers")
			SELF:accessorKeywordsColor := definition:GetNamedColor("GetSetAddRemove")
			SELF:referenceTypeColor := definition:GetNamedColor("ReferenceTypes")
			SELF:valueTypeColor := definition:GetNamedColor("ValueTypes")
			SELF:interfaceTypeColor := definition:GetNamedColor("InterfaceTypes")
			SELF:enumerationTypeColor := definition:GetNamedColor("EnumTypes")
			SELF:typeParameterTypeColor := definition:GetNamedColor("TypeParameters")
			SELF:delegateTypeColor := definition:GetNamedColor("DelegateTypes")
			SELF:methodDeclarationColor := SELF:methodCallColor := definition:GetNamedColor("MethodCall")
			SELF:fldDeclarationColor := SELF:fieldAccessColor := definition:GetNamedColor("FieldAccess")
			SELF:valueKeywordColor := definition:GetNamedColor("NullOrValueKeywords")
			SELF:thisKeywordColor := definition:GetNamedColor("ThisOrBaseReference")
			SELF:trueKeywordColor := definition:GetNamedColor("TrueFalse")
			SELF:typeKeywordsColor := definition:GetNamedColor("TypeKeywords")
			SELF:attributeKeywordsColor := definition:GetNamedColor("AttributeKeywords")
			SELF:commentColor := definition:GetNamedColor("Comment")
			//
			SELF:xsInitialize()
			
		PRIVATE METHOD xsInitialize() AS VOID
			// Special XSharp Init
			IF (specialTypeNames == NULL)
				// AS we are using the CSharpDecompiler, we deal with CSharp Types
				specialTypeNames := Hashtable{}
				specialTypeNames["void"] := "void"
				specialTypeNames["object"] := "Object"
				specialTypeNames["string"] := "string"
				specialTypeNames["sbyte"] := "SByte"
				specialTypeNames["byte"] := "Byte"
				specialTypeNames["short"] := "Short"
				specialTypeNames["ushort"] := "Word"
				specialTypeNames["int"] := "Long"
				specialTypeNames["uint"] := "DWord"
				specialTypeNames["int64"] := "Int64"
				specialTypeNames["uint64"] := "UInt64"
				specialTypeNames["char"] := "Char"
				specialTypeNames["bool"] := "Logic"
				specialTypeNames["float"] := "real4"
				specialTypeNames["double"] := "real8"
				specialTypeNames["decimal"] := "Decimal"
			ENDIF
			SELF:beginend := FALSE
		
		VIRTUAL METHOD EndNode(node AS AstNode) AS VOID
			//
			SUPER:EndNode(node)
			SELF:nodeStack:Pop()
		
		PRIVATE METHOD GetCurrentDefinition() AS ISymbol
			LOCAL parent AS AstNode
			//
			IF ((SELF:nodeStack != NULL) .AND. (SELF:nodeStack:Count != 0))
				//
				parent := SELF:nodeStack:Peek() ASTYPE Identifier
				IF (parent != NULL)
					//
					parent := parent:Parent
				ENDIF
				IF (XSharpHighlightingTokenWriter.IsDefinition(parent))
					//
					RETURN AnnotationExtensions.GetSymbol(parent)
				ENDIF
			ENDIF
			RETURN NULL
		
		PRIVATE METHOD GetCurrentMemberReference() AS ISymbol
			LOCAL mbr AS IMember
			LOCAL node AS AstNode
			LOCAL sym AS ISymbol
			LOCAL declaringType AS IType
			//
			node := SELF:nodeStack:Peek()
			sym := AnnotationExtensions.GetSymbol(node)
			IF (((sym == NULL) .AND. (node:Role == Roles.TargetExpression)) .AND. (node:Parent IS InvocationExpression))
				//
				sym := AnnotationExtensions.GetSymbol(node:Parent)
			ENDIF
			IF ((sym != NULL) .AND. (node:Parent IS ObjectCreateExpression))
				//
				sym := AnnotationExtensions.GetSymbol(node:Parent)
			ENDIF
			VAR nodeIE := node ASTYPE IdentifierExpression
			IF ((nodeIE != NULL) .AND. (node:Role == Roles.TargetExpression)) 
				VAR nodeParent := node:Parent ASTYPE InvocationExpression
				mbr := sym ASTYPE IMember
				IF ((nodeParent != NULL ) .AND. (mbr != NULL))
					//
					declaringType := mbr:DeclaringType
					IF ((declaringType != NULL) .AND. (declaringType:Kind == TypeKind.Delegate))
						//
						RETURN NULL
					ENDIF
				ENDIF
			ENDIF
			RETURN sym
		
		PRIVATE STATIC METHOD IsDefinition(node REF AstNode) AS LOGIC
			//
			IF ((node IS EntityDeclaration))
				//
				RETURN TRUE
			ENDIF
			IF ((node IS VariableInitializer) .AND. (node:Parent IS FieldDeclaration))
				//
				node := node:Parent
				RETURN TRUE
			ENDIF
			RETURN (node IS FixedVariableInitializer)
		
		VIRTUAL METHOD StartNode(node AS AstNode) AS VOID
			//
			SELF:nodeStack:Push(node)
			SUPER:StartNode(node)
			
		
		VIRTUAL METHOD WriteComment( cmtType AS CommentType, content AS STRING ) AS VOID
			//
			SELF:textOutput:BeginSpan( SELF:commentColor)
			SUPER:WriteComment( cmtType, content )
			SELF:textOutput:EndSpan()
			
			
		
		VIRTUAL METHOD WriteIdentifier(identifier AS Identifier) AS VOID
			LOCAL color AS HighlightingColor
			LOCAL accessor AS Accessor
			LOCAL currentDefinition AS ISymbol
			LOCAL definition AS ITypeDefinition
			LOCAL mthd AS IMethod
			LOCAL method2 AS IMethod
			LOCAL fld AS IField
			LOCAL field2 AS IField
			LOCAL definition2 AS ITypeDefinition
			LOCAL currentMemberReference AS ISymbol
			LOCAL typeDef AS IType
			LOCAL method3 AS IMethod
			LOCAL method4 AS IMethod
			LOCAL field3 AS IField
			LOCAL field4 AS IField
			LOCAL type2 AS IType
			//
			color := NULL
			IF (identifier:Name == "value")
				accessor := identifier:Ancestors.OfType<Accessor>().FirstOrDefault()
				IF (( accessor != NULL) .AND. (accessor:Role != PropertyDeclaration.GetterRole))
					//
					color := SELF:valueKeywordColor
				ENDIF
			ENDIF
			currentDefinition := SELF:GetCurrentDefinition()
			IF (currentDefinition != NULL)
				//
				definition := currentDefinition ASTYPE ITypeDefinition
				IF (definition == NULL)
					//
					mthd := currentDefinition ASTYPE IMethod
					IF (mthd != NULL)
						//
						method2 := mthd
						color := SELF:methodDeclarationColor
					ELSE
						//
						fld := currentDefinition ASTYPE IField
						IF (fld != NULL)
							//
							field2 := fld
							color := SELF:fldDeclarationColor
						ENDIF
					ENDIF
				ELSE
					//
					definition2 := definition
					SWITCH definition2:Kind
						CASE TypeKind.Class
							//
							color := SELF:referenceTypeColor
						CASE TypeKind.Interface
							//
							color := SELF:interfaceTypeColor
						CASE TypeKind.Struct
							//
							color := SELF:valueTypeColor
						CASE TypeKind.Delegate
							//
							color := SELF:delegateTypeColor
						CASE TypeKind.Enum
							//
							color := SELF:enumerationTypeColor
					END SWITCH
				ENDIF
			ENDIF
			currentMemberReference := SELF:GetCurrentMemberReference()
			IF (currentMemberReference != NULL)
				//
				typeDef := currentMemberReference ASTYPE IType
				IF (typeDef == NULL)
					//
					method3 := currentMemberReference ASTYPE IMethod
					IF (method3 != NULL)
						//
						method4 := method3
						color := SELF:methodCallColor
					ELSE
						//
						field3 := currentMemberReference ASTYPE IField
						IF (field3 != NULL)
							//
							field4 := field3
							color := SELF:fieldAccessColor
						ENDIF
					ENDIF
				ELSE
					//
					type2 := typeDef
					SWITCH type2:Kind
						CASE TypeKind.Class
							//
							color := SELF:referenceTypeColor
						CASE TypeKind.Interface
							//
							color := SELF:interfaceTypeColor
						CASE TypeKind.Struct
							//
							color := SELF:valueTypeColor
						CASE TypeKind.Delegate
							//
							color := SELF:delegateTypeColor
						CASE TypeKind.Enum
							//
							color := SELF:enumerationTypeColor
					END SWITCH
				ENDIF
			ENDIF
			IF (color != NULL)
				//
				SELF:textOutput:BeginSpan(color)
			ENDIF
			//
			IF ( XSharpOutputVisitor.IsKeyword( identifier:Name, identifier ) )
				SELF:textOutput:Write("@@")
			ENDIF
			//
			SUPER:WriteIdentifier(identifier)
			IF (color != NULL)
				//
				SELF:textOutput:EndSpan()
			ENDIF
		
		VIRTUAL METHOD WriteKeyword(role AS Role, keyword AS STRING) AS VOID
			LOCAL color AS HighlightingColor
			//
			color := NULL
			keyword := keyword:ToLower()
			SWITCH (keyword) 
				CASE "using"
					IF beginend
						color := structureKeywordsColor
						beginend := FALSE
					ELSE
						color := namespaceKeywordsColor
					ENDIF
				CASE "namespace"
					color := namespaceKeywordsColor
				CASE "self"
				CASE "super"
					color := thisKeywordColor
				CASE "true"
				CASE "false"
					color := trueKeywordColor
				CASE "public"
				CASE "export"
				CASE "internal"
				CASE "protected"
				CASE "private"
				CASE "hidden"
					color := visibilityKeywordsColor
				CASE "begin"
				CASE "end"
					color := structureKeywordsColor
					beginend := TRUE
				CASE "if"
				CASE "else"
				CASE "next"
				CASE "endif"
				CASE "enddo"
				CASE "switch"
				CASE "case"
				CASE "otherwise"
				CASE "while"
				CASE "do"
				CASE "for"
				CASE "to"
				CASE "downto"
				CASE "upto"
				CASE "step"
				CASE "foreach"
				CASE "lock"
				CASE "global"
				CASE "dynamic"
				CASE "await"
				CASE "method"
				CASE "property"
				CASE "event"
				CASE "member"
				CASE "constructor"
				CASE "destructor"
					color := structureKeywordsColor
				CASE "where"
					color := structureKeywordsColor
				CASE "in"
					color := structureKeywordsColor
				CASE "as"
				CASE "astype"
				CASE "local"
				CASE "is"
				CASE "new"
				CASE "sizeof"
				CASE "typeof"
				CASE "nameof"
				CASE "stackalloc"
					color := typeKeywordsColor
				CASE "try"
				CASE "throw"
				CASE "catch"
				CASE "finally"
					color := exceptionKeywordsColor
				CASE "when"
					color := structureKeywordsColor
				CASE "get"
				CASE "set"
				CASE "add"
				CASE "remove"
					color := accessorKeywordsColor
				CASE "abstract"
				CASE "initonly"				
				CASE "extern"
				CASE "override"
				CASE "readonly"
				CASE "sealed"
				CASE "static"
				CASE "virtual"
				CASE "volatile"
				CASE "async"
				CASE "partial"
					color := modifiersColor
				CASE "checked"
				CASE "unchecked"
					color := checkedKeywordColor
				CASE "fixed"
				CASE "unsafe"
					color := unsafeKeywordsColor
				CASE "enum"
				CASE "structure"
					color := valueTypeKeywordsColor
				CASE "class"
				CASE "interface"
				CASE "delegate"
				CASE "inherit"
				CASE "implements"
					color := referenceTypeKeywordsColor
				CASE "select"
				CASE "group"
				CASE "by"
				CASE "into"
				CASE "from"
				CASE "ascending"
				CASE "descending"
				CASE "orderby"
				CASE "let"
				CASE "join"
				CASE "on"
				CASE "equals"
					color := structureKeywordsColor
				CASE "explicit"
				CASE "implicit"
				CASE "operator"
					color := operatorKeywordsColor
				CASE "params"
				CASE "ref"
				CASE "out"
					color := parameterModifierColor
				CASE "break"
				CASE "continue"
				CASE "goto"
				CASE "yield"
				CASE "return"
				CASE "exit"
					color := gotoKeywordsColor
			END SWITCH
			//
			LOCAL isAttr AS AttributeSection
			isAttr := SELF:nodeStack.PeekOrDefault<AstNode>() ASTYPE AttributeSection
			IF ( isAttr != NULL )
				//
				color := SELF:attributeKeywordsColor
			ENDIF
			IF (color != NULL)
				//
				SELF:textOutput:BeginSpan(color)
			ENDIF
			IF ( SELF:xsSettings:UpperKeyword )
				keyword := keyword:ToUpper()
			ENDIF
			SUPER:WriteKeyword(role, keyword)
			IF (color != NULL)
				//
				SELF:textOutput:EndSpan()
			ENDIF
		
		VIRTUAL METHOD WritePrimitiveType(typeDef AS STRING) AS VOID
			LOCAL color AS HighlightingColor
			//
			color := NULL
			typeDef := typeDef:ToLower()
			SWITCH (typeDef) 
				CASE "new"
					color := typeKeywordsColor
				CASE "logic"
				CASE "byte"
				CASE "char"
				CASE "decimal"
				CASE "double"
				CASE "real4"
				CASE "real8"
				CASE "enum"
				CASE "float"
				CASE "int"
				CASE "long"
				CASE "sbyte"
				CASE "short"
				CASE "struct"
				CASE "uint"
				CASE "ushort"
				CASE "ulong"
					color := valueTypeKeywordsColor
				CASE "object"
				CASE "string"
				CASE "void"
					color := referenceTypeKeywordsColor
			END SWITCH
			IF (color != NULL)
				//
				SELF:textOutput:BeginSpan(color)
			ENDIF
			//
			// Convert System Types
			IF (specialTypeNames.Contains(typeDef))
				typeDef := (STRING)specialTypeNames[typeDef]
			END IF
			//
			SUPER:WritePrimitiveType(typeDef)
			IF (color != NULL)
				//
				SELF:textOutput:EndSpan()
			ENDIF
		
		VIRTUAL METHOD WritePrimitiveValue(VALUE AS OBJECT, format := LiteralFormat.None AS LiteralFormat ) AS VOID
			LOCAL color AS HighlightingColor
			//
			color := NULL
			IF (VALUE == NULL)
				//
				color := SELF:valueKeywordColor
			ENDIF
			IF (Object.Equals(TRUE, VALUE) .OR. Object.Equals(FALSE, VALUE))
				//
				color := SELF:trueKeywordColor
			ENDIF
			IF (color != NULL)
				//
				SELF:textOutput:BeginSpan(color)
			ENDIF
			// Indicate to XSharp to keep the String as it is
			IF ( VALUE IS STRING )
				SUPER:WriteInterpolatedText("e")
				//SUPER:WritePrimitiveValue( "e", LiteralFormat.)
			ENDIF
			SUPER:WritePrimitiveValue(VALUE )
			IF (color != NULL)
				//
				SELF:textOutput:EndSpan()
			ENDIF
			
		
	END CLASS
END NAMESPACE // ILSpy.XSharpLanguage