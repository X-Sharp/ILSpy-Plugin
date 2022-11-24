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
                    result := "private"
                CASE Modifiers.Internal
                    result := "internal"
                CASE Modifiers.Protected
                    result := "protected"
                CASE Modifiers.Public
                    result := "public"
                CASE Modifiers.Abstract
                    result := "abstract"
                CASE Modifiers.Virtual
                    result := "virtual"
                CASE Modifiers.Sealed
                    result := "sealed"
                CASE Modifiers.Static
                    result := "static"
                CASE Modifiers.Override
                    result := "override"
                CASE Modifiers.Readonly
                    result := "initonly"
                CASE Modifiers.Const
                    result := "const"
                CASE Modifiers.New
                    result := "new"
                CASE Modifiers.Partial
                    result := "partial"
                CASE Modifiers.Extern
                    result := "extern"
                CASE Modifiers.Volatile
                    result := "volatile"
                CASE Modifiers.Unsafe
                    result := "unsafe"
                CASE Modifiers.Async
                    result := "async"
                CASE Modifiers.Ref
                    result := "ref"
                CASE Modifiers.Any
                    // even though it's used for pattern matching only, 'any' needs to be in this list to be usable in the AST
                    result := "any"
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