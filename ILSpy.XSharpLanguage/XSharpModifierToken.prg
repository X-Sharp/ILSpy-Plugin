// XSharpModifierToken.prg
// Created by    : fabri
// Creation Date : 3/31/2018 4:25:00 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING ICSharpCode.Decompiler.CSharp.Syntax


BEGIN NAMESPACE ILSpy.XSharpLanguage
    
    /// <summary>
    /// The XSharpModifierToken class.
    /// </summary>
    CLASS XSharpTokenHelper
        
        CONSTRUCTOR()
            RETURN
            
        PUBLIC STATIC METHOD GetModifierName( modifier AS Modifiers) AS STRING
            //
            LOCAL result AS STRING
            SWITCH (modifier) 
                CASE Modifiers.Private
                    Result := "private"
                CASE Modifiers.Internal
                    Result := "internal"
                CASE Modifiers.Protected
                    Result := "protected"
                CASE Modifiers.Public
                    Result := "public"
                CASE Modifiers.Abstract
                    Result := "abstract"
                CASE Modifiers.Virtual
                    Result := "virtual"
                CASE Modifiers.Sealed
                    Result := "sealed"
                CASE Modifiers.Static
                    Result := "static"
                CASE Modifiers.Override
                    Result := "override"
                CASE Modifiers.Readonly
                    Result := "initonly"
                CASE Modifiers.Const
                    Result := "const"
                CASE Modifiers.New
                    Result := "new"
                CASE Modifiers.Partial
                    Result := "partial"
                CASE Modifiers.Extern
                    Result := "extern"
                CASE Modifiers.Volatile
                    Result := "volatile"
                CASE Modifiers.Unsafe
                    Result := "unsafe"
                CASE Modifiers.Async
                    Result := "async"
                CASE Modifiers.Ref
                    Result := "ref"
                CASE Modifiers.Any
                    // even though it's used for pattern matching only, 'any' needs to be in this list to be usable in the AST
                    Result := "any"
                OTHERWISE
                THROW NotSupportedException{"Invalid value for Modifiers"}
            END SWITCH
            //
            RETURN result:ToUpper()
            
            
        STATIC PUBLIC METHOD GetOperatorRole( op AS AssignmentOperatorType) AS STRING
            SWITCH (op) 
                CASE AssignmentOperatorType.Assign
                    RETURN ":="
                CASE AssignmentOperatorType.Add
                    RETURN "+="
                CASE AssignmentOperatorType.Subtract
                    RETURN "-="
                CASE AssignmentOperatorType.Multiply
                    RETURN "*="
                CASE AssignmentOperatorType.Divide
                    RETURN "/="
                CASE AssignmentOperatorType.Modulus
                    RETURN "%="
                CASE AssignmentOperatorType.ShiftLeft
                    RETURN "<<="
                CASE AssignmentOperatorType.ShiftRight
                    RETURN ">>="
                CASE AssignmentOperatorType.BitwiseAnd
                    RETURN "&="
                CASE AssignmentOperatorType.BitwiseOr
                    RETURN "|="
                CASE AssignmentOperatorType.ExclusiveOr
                RETURN "^="
                OTHERWISE
                    THROW NotSupportedException{"Invalid value for AssignmentOperatorType"}
            END SWITCH
            
            
            END CLASS
END NAMESPACE // ILSpy.XSharpLanguage