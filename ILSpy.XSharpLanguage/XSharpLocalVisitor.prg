// XSharpLocalVisitor.prg
// Created by    : fabri
// Creation Date : 4/2/2018 9:42:27 PM
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
    /// The XSharpLocalVisitor class.
    /// Visit/Walk Statements (only) in order to declare Variables
    /// This Visitor is called in XSharpOutputVisitor.WriteMethodBody 
    /// </summary>
    CLASS XSharpLocalVisitor IMPLEMENTS IAstVisitor
        INITONLY PROTECTED writer AS TokenWriter
        INITONLY PROTECTED policy AS CSharpFormattingOptions
        INITONLY PRIVATE stdout AS XSharpOutputVisitor
        //
        PRIVATE varList AS List<STRING>
        
        
        CONSTRUCTOR(writer AS TokenWriter, formattingPolicy AS CSharpFormattingOptions, outVisitor AS XSharpOutputVisitor);SUPER()
            //
            SELF:writer := writer 
            SELF:policy := formattingPolicy
            SELF:stdout := outVisitor
            //
            varList := List<STRING>{}
            
        VIRTUAL METHOD VisitVariableDeclarationStatement(variableDeclarationStatement AS VariableDeclarationStatement) AS VOID
            LOCAL toDeclare AS List<AstNode>
            LOCAL needLocal := FALSE AS LOGIC
            toDeclare := List<AstNode>{}
            // Now Create a List of non-declared Vars
            FOREACH localvar AS VariableInitializer IN variableDeclarationStatement:Variables
                IF ( !SELF:varList:Contains( localvar:Name ) )
                    toDeclare:Add( localvar )
                    needLocal := TRUE
                ENDIF
            NEXT
            //
            IF (!needLocal )
                RETURN
            ENDIF
            //
            SELF:writer:WriteKeyword( NULL, "LOCAL")
            SELF:writer:Space()
            //SELF:WriteCommaSeparatedList((System.Collections.Generic.IEnumerable<AstNode>)variableDeclarationStatement:Variables )
            SELF:WriteCommaSeparatedList( toDeclare )
            SELF:writer:Space()
            SELF:writer:WriteKeyword( NULL, "AS" )
            SELF:writer:Space()
            SELF:WriteModifiers(variableDeclarationStatement:GetChildrenByRole<CSharpModifierToken>(VariableDeclarationStatement.ModifierRole))
            //SELF:writer:Space()
            variableDeclarationStatement:@@Type:AcceptVisitor(stdout)
            SELF:writer:NewLine()
            //
            FOREACH localvar AS VariableInitializer IN toDeclare
                SELF:varList:Add( localvar:Name )
            NEXT
            //
            
        PROTECTED VIRTUAL METHOD WriteCommaSeparatedList(list AS System.Collections.Generic.IEnumerable<AstNode>) AS VOID
            LOCAL flag AS LOGIC
            //
            flag := TRUE
            FOREACH node AS AstNode IN list
                //
                IF (flag)
                        //
                    flag := FALSE
                ELSE
                    //
                SELF:writer:WriteToken( XSRoles.Comma, ",")
                ENDIF
            node:AcceptVisitor(SELF)
            NEXT
            
        PROTECTED VIRTUAL METHOD WriteModifiers(modifierTokens AS System.Collections.Generic.IEnumerable<CSharpModifierToken>) AS VOID
            //
            FOREACH token AS CSharpModifierToken IN modifierTokens
                //
                SELF:writer:WriteKeyword( NULL, XSharpTokenHelper.GetModifierName( token:Modifier ) )
            SELF:writer:Space()
            NEXT
            
            
        VIRTUAL METHOD ICSharpCode.Decompiler.CSharp.Syntax.IAstVisitor.VisitErrorNode(errorNode AS AstNode) AS VOID
            
        VIRTUAL METHOD ICSharpCode.Decompiler.CSharp.Syntax.IAstVisitor.VisitNullNode(nullNode AS AstNode) AS VOID
            
        VIRTUAL METHOD VisitAccessor(accessor AS Accessor) AS VOID
            
        VIRTUAL METHOD VisitAnonymousMethodExpression(anonymousMethodExpression AS AnonymousMethodExpression) AS VOID
            
        VIRTUAL METHOD VisitAnonymousTypeCreateExpression(anonymousTypeCreateExpression AS AnonymousTypeCreateExpression) AS VOID
            
        VIRTUAL METHOD VisitArrayCreateExpression(arrayCreateExpression AS ArrayCreateExpression) AS VOID
            
        VIRTUAL METHOD VisitArrayInitializerExpression(arrayInitializerExpression AS ArrayInitializerExpression) AS VOID
            
        VIRTUAL METHOD VisitArraySpecifier(arraySpecifier AS ArraySpecifier) AS VOID
            
        VIRTUAL METHOD VisitAsExpression(asExpression AS AsExpression) AS VOID
            
        VIRTUAL METHOD VisitAssignmentExpression(assignmentExpression AS AssignmentExpression) AS VOID
            
        VIRTUAL METHOD VisitAttribute(attribute AS ICSharpCode.Decompiler.CSharp.Syntax.Attribute) AS VOID
            
        VIRTUAL METHOD VisitAttributeSection(attributeSection AS AttributeSection) AS VOID
            
        VIRTUAL METHOD VisitBaseReferenceExpression(baseReferenceExpression AS BaseReferenceExpression) AS VOID
            
        VIRTUAL METHOD VisitBinaryOperatorExpression(binaryOperatorExpression AS BinaryOperatorExpression) AS VOID
            
        VIRTUAL METHOD VisitBlockStatement(blockStatement AS BlockStatement) AS VOID
            
        VIRTUAL METHOD VisitBreakStatement(breakStatement AS BreakStatement) AS VOID
            
        VIRTUAL METHOD VisitCaseLabel(caseLabel AS CaseLabel) AS VOID
            
        VIRTUAL METHOD VisitCastExpression(castExpression AS CastExpression) AS VOID
            
        VIRTUAL METHOD VisitCatchClause(catchClause AS CatchClause) AS VOID
            
        VIRTUAL METHOD VisitCheckedExpression(checkedExpression AS CheckedExpression) AS VOID
            
        VIRTUAL METHOD VisitCheckedStatement(checkedStatement AS CheckedStatement) AS VOID
            
        VIRTUAL METHOD VisitComment(comment AS Comment) AS VOID
            
        VIRTUAL METHOD VisitComposedType(composedType AS ComposedType) AS VOID
            
        VIRTUAL METHOD VisitConditionalExpression(conditionalExpression AS ConditionalExpression) AS VOID
            
        VIRTUAL METHOD VisitConstraint(constraint AS Constraint) AS VOID
            
        VIRTUAL METHOD VisitConstructorDeclaration(constructorDeclaration AS ConstructorDeclaration) AS VOID
            
        VIRTUAL METHOD VisitConstructorInitializer(constructorInitializer AS ConstructorInitializer) AS VOID
            
        VIRTUAL METHOD VisitContinueStatement(continueStatement AS ContinueStatement) AS VOID
            
        VIRTUAL METHOD VisitCSharpTokenNode(cSharpTokenNode AS CSharpTokenNode) AS VOID
            
        VIRTUAL METHOD VisitCustomEventDeclaration(customEventDeclaration AS CustomEventDeclaration) AS VOID
            
        VIRTUAL METHOD VisitDefaultValueExpression(defaultValueExpression AS DefaultValueExpression) AS VOID
            
        VIRTUAL METHOD VisitDelegateDeclaration(delegateDeclaration AS DelegateDeclaration) AS VOID
            
        VIRTUAL METHOD VisitDestructorDeclaration(destructorDeclaration AS DestructorDeclaration) AS VOID
            
        VIRTUAL METHOD VisitDirectionExpression(directionExpression AS DirectionExpression) AS VOID
            
        VIRTUAL METHOD VisitDocumentationReference(documentationReference AS DocumentationReference) AS VOID
            
        VIRTUAL METHOD VisitDoWhileStatement(doWhileStatement AS DoWhileStatement) AS VOID
            
        VIRTUAL METHOD VisitEmptyStatement(emptyStatement AS EmptyStatement) AS VOID
            
        VIRTUAL METHOD VisitEnumMemberDeclaration(enumMemberDeclaration AS EnumMemberDeclaration) AS VOID
            
        VIRTUAL METHOD VisitEventDeclaration(eventDeclaration AS EventDeclaration) AS VOID
            
        VIRTUAL METHOD VisitExpressionStatement(expressionStatement AS ExpressionStatement) AS VOID
            
        VIRTUAL METHOD VisitExternAliasDeclaration(externAliasDeclaration AS ExternAliasDeclaration) AS VOID
            
        VIRTUAL METHOD VisitFieldDeclaration(fieldDeclaration AS FieldDeclaration) AS VOID
            
        VIRTUAL METHOD VisitFixedFieldDeclaration(fixedFieldDeclaration AS FixedFieldDeclaration) AS VOID
            
        VIRTUAL METHOD VisitFixedStatement(fixedStatement AS FixedStatement) AS VOID
            
        VIRTUAL METHOD VisitFixedVariableInitializer(fixedVariableInitializer AS FixedVariableInitializer) AS VOID
            
        VIRTUAL METHOD VisitForeachStatement(foreachStatement AS ForeachStatement) AS VOID
            SELF:WriteEmbeddedStatement(foreachStatement:EmbeddedStatement)
            
        VIRTUAL METHOD VisitForStatement(forStatement AS ForStatement) AS VOID
            SELF:WriteEmbeddedStatement(forStatement:EmbeddedStatement)
            
        VIRTUAL METHOD VisitGotoCaseStatement(gotoCaseStatement AS GotoCaseStatement) AS VOID
            
        VIRTUAL METHOD VisitGotoDefaultStatement(gotoDefaultStatement AS GotoDefaultStatement) AS VOID
            
        VIRTUAL METHOD VisitGotoStatement(gotoStatement AS GotoStatement) AS VOID
            
        VIRTUAL METHOD VisitIdentifier(identifier AS Identifier) AS VOID
            
        VIRTUAL METHOD VisitIdentifierExpression(identifierExpression AS IdentifierExpression) AS VOID
            
        VIRTUAL METHOD VisitIfElseStatement(ifElseStatement AS IfElseStatement) AS VOID
            IF (ifElseStatement:FalseStatement:IsNull)
                WriteEmbeddedStatement(ifElseStatement:TrueStatement)
            ELSE 
                WriteEmbeddedStatement(ifElseStatement:TrueStatement)
                WriteEmbeddedStatement(ifElseStatement:FalseStatement)
            ENDIF
            
        VIRTUAL METHOD VisitIndexerDeclaration(indexerDeclaration AS IndexerDeclaration) AS VOID
            
        VIRTUAL METHOD VisitIndexerExpression(indexerExpression AS IndexerExpression) AS VOID
            
        VIRTUAL METHOD VisitInterpolatedStringExpression(interpolatedStringExpression AS InterpolatedStringExpression) AS VOID
            
        VIRTUAL METHOD VisitInterpolatedStringText(interpolatedStringText AS InterpolatedStringText) AS VOID
            
        VIRTUAL METHOD VisitInterpolation(interpolation AS Interpolation) AS VOID
            
        VIRTUAL METHOD VisitInvocationExpression(invocationExpression AS InvocationExpression) AS VOID
            
        VIRTUAL METHOD VisitIsExpression(isExpression AS IsExpression) AS VOID
            
        VIRTUAL METHOD VisitLabelStatement(labelStatement AS LabelStatement) AS VOID
            
        VIRTUAL METHOD VisitLambdaExpression(lambdaExpression AS LambdaExpression) AS VOID
            
        VIRTUAL METHOD VisitLockStatement(lockStatement AS LockStatement) AS VOID
            
        VIRTUAL METHOD VisitMemberReferenceExpression(memberReferenceExpression AS MemberReferenceExpression) AS VOID
            
        VIRTUAL METHOD VisitMemberType(memberType AS MemberType) AS VOID
            
        VIRTUAL METHOD VisitMethodDeclaration(methodDeclaration AS MethodDeclaration) AS VOID
            
        VIRTUAL METHOD VisitNamedArgumentExpression(namedArgumentExpression AS NamedArgumentExpression) AS VOID
            
        VIRTUAL METHOD VisitNamedExpression(namedExpression AS NamedExpression) AS VOID
            
        VIRTUAL METHOD VisitNamespaceDeclaration(namespaceDeclaration AS NamespaceDeclaration) AS VOID
            
        VIRTUAL METHOD VisitNewLine(newLineNode AS NewLineNode) AS VOID
            
        VIRTUAL METHOD VisitNullReferenceExpression(nullReferenceExpression AS NullReferenceExpression) AS VOID
            
        VIRTUAL METHOD VisitObjectCreateExpression(objectCreateExpression AS ObjectCreateExpression) AS VOID
            
        VIRTUAL METHOD VisitOperatorDeclaration(operatorDeclaration AS OperatorDeclaration) AS VOID
            
        VIRTUAL METHOD VisitOutVarDeclarationExpression(outVarDeclarationExpression AS OutVarDeclarationExpression) AS VOID
            
        VIRTUAL METHOD VisitParameterDeclaration(parameterDeclaration AS ParameterDeclaration) AS VOID
            
        VIRTUAL METHOD VisitParenthesizedExpression(parenthesizedExpression AS ParenthesizedExpression) AS VOID
            
        VIRTUAL METHOD VisitPatternPlaceholder(placeholder AS AstNode, pattern AS Pattern) AS VOID
            
        VIRTUAL METHOD VisitPointerReferenceExpression(pointerReferenceExpression AS PointerReferenceExpression) AS VOID
            
        VIRTUAL METHOD VisitPreProcessorDirective(preProcessorDirective AS PreProcessorDirective) AS VOID
            
        VIRTUAL METHOD VisitPrimitiveExpression(primitiveExpression AS PrimitiveExpression) AS VOID
            
        VIRTUAL METHOD VisitPrimitiveType(primitiveType AS PrimitiveType) AS VOID
            
        VIRTUAL METHOD VisitPropertyDeclaration(propertyDeclaration AS PropertyDeclaration) AS VOID
            
        VIRTUAL METHOD VisitQueryContinuationClause(queryContinuationClause AS QueryContinuationClause) AS VOID
            
        VIRTUAL METHOD VisitQueryExpression(queryExpression AS QueryExpression) AS VOID
            
        VIRTUAL METHOD VisitQueryFromClause(queryFromClause AS QueryFromClause) AS VOID
            
        VIRTUAL METHOD VisitQueryGroupClause(queryGroupClause AS QueryGroupClause) AS VOID
            
        VIRTUAL METHOD VisitQueryJoinClause(queryJoinClause AS QueryJoinClause) AS VOID
            
        VIRTUAL METHOD VisitQueryLetClause(queryLetClause AS QueryLetClause) AS VOID
            
        VIRTUAL METHOD VisitQueryOrderClause(queryOrderClause AS QueryOrderClause) AS VOID
            
        VIRTUAL METHOD VisitQueryOrdering(queryOrdering AS QueryOrdering) AS VOID
            
        VIRTUAL METHOD VisitQuerySelectClause(querySelectClause AS QuerySelectClause) AS VOID
            
        VIRTUAL METHOD VisitQueryWhereClause(queryWhereClause AS QueryWhereClause) AS VOID
            
        VIRTUAL METHOD VisitReturnStatement(returnStatement AS ReturnStatement) AS VOID
            
        VIRTUAL METHOD VisitSimpleType(simpleType AS SimpleType) AS VOID
            
        VIRTUAL METHOD VisitSizeOfExpression(sizeOfExpression AS SizeOfExpression) AS VOID
            
        VIRTUAL METHOD VisitStackAllocExpression(stackAllocExpression AS StackAllocExpression) AS VOID
            
        VIRTUAL METHOD VisitSwitchSection(switchSection AS SwitchSection) AS VOID
            FOREACH statement AS Statement IN switchSection:Statements
                //
            statement:AcceptVisitor(SELF)
            NEXT
            
        VIRTUAL METHOD VisitSwitchStatement(switchStatement AS SwitchStatement) AS VOID
            FOREACH section AS SwitchSection IN switchStatement:SwitchSections
                //
            section:AcceptVisitor(SELF)
            NEXT
            
            
        VIRTUAL METHOD VisitSyntaxTree(syntaxTree AS SyntaxTree) AS VOID
            
        VIRTUAL METHOD VisitText(textNode AS TextNode) AS VOID
            
        VIRTUAL METHOD VisitThisReferenceExpression(thisReferenceExpression AS ThisReferenceExpression) AS VOID
            
        VIRTUAL METHOD VisitThrowExpression(throwExpression AS ThrowExpression) AS VOID
            
        VIRTUAL METHOD VisitThrowStatement(throwStatement AS ThrowStatement) AS VOID
            
        VIRTUAL METHOD VisitTryCatchStatement(tryCatchStatement AS TryCatchStatement) AS VOID
            
        VIRTUAL METHOD VisitTypeDeclaration(typeDeclaration AS TypeDeclaration) AS VOID
            
        VIRTUAL METHOD VisitTypeOfExpression(typeOfExpression AS TypeOfExpression) AS VOID
            
        VIRTUAL METHOD VisitTypeParameterDeclaration(typeParameterDeclaration AS TypeParameterDeclaration) AS VOID
            
        VIRTUAL METHOD VisitTypeReferenceExpression(typeReferenceExpression AS TypeReferenceExpression) AS VOID
            
        VIRTUAL METHOD VisitUnaryOperatorExpression(unaryOperatorExpression AS UnaryOperatorExpression) AS VOID
            
        VIRTUAL METHOD VisitUncheckedExpression(uncheckedExpression AS UncheckedExpression) AS VOID
            
        VIRTUAL METHOD VisitUncheckedStatement(uncheckedStatement AS UncheckedStatement) AS VOID
            
        VIRTUAL METHOD VisitUndocumentedExpression(undocumentedExpression AS UndocumentedExpression) AS VOID
            
        VIRTUAL METHOD VisitUnsafeStatement(unsafeStatement AS UnsafeStatement) AS VOID
            
        VIRTUAL METHOD VisitUsingAliasDeclaration(usingAliasDeclaration AS UsingAliasDeclaration) AS VOID
            
        VIRTUAL METHOD VisitUsingDeclaration(usingDeclaration AS UsingDeclaration) AS VOID
            
        VIRTUAL METHOD VisitUsingStatement(usingStatement AS UsingStatement) AS VOID
            SELF:WriteEmbeddedStatement(usingStatement:EmbeddedStatement)
            
        VIRTUAL METHOD VisitVariableInitializer(variableInitializer AS VariableInitializer) AS VOID
            //
            SELF:writer:WriteIdentifier(variableInitializer:NameToken)
            
        VIRTUAL METHOD VisitWhileStatement(whileStatement AS WhileStatement) AS VOID
        SELF:WriteEmbeddedStatement(whileStatement:EmbeddedStatement)
            
        VIRTUAL METHOD VisitWhitespace(whitespaceNode AS WhitespaceNode) AS VOID
            
        VIRTUAL METHOD VisitYieldBreakStatement(yieldBreakStatement AS YieldBreakStatement) AS VOID
            
        VIRTUAL METHOD VisitYieldReturnStatement(yieldReturnStatement AS YieldReturnStatement) AS VOID
            
        PROTECTED VIRTUAL METHOD WriteBlock(blockStatement AS BlockStatement) AS VOID
            //
            FOREACH statement AS Statement IN blockStatement:Statements
                //
            statement:AcceptVisitor( SELF )
            NEXT
            //
            
        PROTECTED VIRTUAL METHOD WriteEmbeddedStatement(embeddedStatement AS Statement) AS VOID
            LOCAL blockStatement AS BlockStatement
            //
            IF (embeddedStatement:IsNull)
                    //
                NOP
            ELSE
                //
                blockStatement := embeddedStatement ASTYPE BlockStatement
                IF (blockStatement != NULL)
                        //
                    SELF:WriteBlock(blockStatement)
                ELSE
                    //
                embeddedStatement:AcceptVisitor(SELF)
            ENDIF
            ENDIF

        PUBLIC PROPERTY Variables AS List<STRING>
            GET
                RETURN SELF:varList
            END GET
        END PROPERTY
            
    END CLASS
END NAMESPACE // ILSpy.XSharpLanguage