// XSharpModifierToken.prg
// Created by    : fabri
// Creation Date : 3/31/2018 12:04:57 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING ICSharpCode.Decompiler.CSharp.Syntax
USING ICSharpCode.Decompiler.CSharp.Syntax.PatternMatching
USING ICSharpCode.Decompiler.CSharp.OutputVisitor


BEGIN NAMESPACE ILSpy.XSharpLanguage
    
    /// <summary>
    /// The XSharpModifierToken class.
    /// </summary>
    
    CLASS XSharpModifierToken INHERIT XSharpTokenNode
        
        // Fields
        PRIVATE _modifier AS Modifiers
        
        // Methods
        CONSTRUCTOR(location AS TextLocation, modifier AS Modifiers)//Inline call to base() in C#
            SUPER(location, NULL)
            //
            SELF:_Modifier := modifier
        
        PROTECTED VIRTUAL METHOD DoMatch(other AS AstNode, match AS Match) AS LOGIC
            LOCAL token AS CSharpModifierToken
            //
            token := (CSharpModifierToken)(other)
            RETURN ((token != NULL) .AND. (SELF:modifier == token:modifier))
        
        STATIC METHOD GetModifierLength(modifier AS Modifiers) AS LONG
            //
            IF (modifier <= Modifiers.Override)
                //
                IF (modifier <= Modifiers.Abstract)
                    //
                    IF (modifier <= Modifiers.Protected)
                        //
                        SWITCH modifier
                            CASE Modifiers.Private
                                //
                                RETURN "private":Length
                            CASE Modifiers.Internal
                                //
                                RETURN "internal":Length
                            CASE Modifiers.Protected
                                //
                                RETURN "protected":Length
                            CASE (Modifiers.None | Modifiers.Any)
                                //
                                RETURN "any":Length
                        END SWITCH
                    ELSE
                        //
                        IF (modifier == Modifiers.Public)
                            //
                            RETURN "public":Length
                        ENDIF
                        IF (modifier == Modifiers.Abstract)
                            //
                            RETURN "abstract":Length
                        ENDIF
                    ENDIF
                ELSE
                    //
                    IF (modifier <= Modifiers.Sealed)
                        //
                        IF (modifier == Modifiers.Virtual)
                            //
                            RETURN "virtual":Length
                        ENDIF
                        IF (modifier == Modifiers.Sealed)
                            //
                            RETURN "sealed":Length
                        ENDIF
                    ELSE
                        //
                        IF (modifier == Modifiers.Static)
                            //
                            RETURN "static":Length
                        ENDIF
                        IF (modifier == Modifiers.Override)
                            //
                            RETURN "override":Length
                        ENDIF
                    ENDIF
                ENDIF
            ELSE
                //
                IF (modifier <= Modifiers.Partial)
                    //
                    IF (modifier <= Modifiers.Const)
                        //
                        IF (modifier == Modifiers.Readonly)
                            //
                            RETURN "readonly":Length
                        ENDIF
                        IF (modifier == Modifiers.Const)
                            //
                            RETURN "const":Length
                        ENDIF
                    ELSE
                        //
                        IF (modifier == Modifiers.New)
                            //
                            RETURN "new":Length
                        ENDIF
                        IF (modifier == Modifiers.Partial)
                            //
                            RETURN "partial":Length
                        ENDIF
                    ENDIF
                ELSE
                    //
                    IF (modifier <= Modifiers.Volatile)
                        //
                        IF (modifier == Modifiers.Extern)
                            //
                            RETURN "extern":Length
                        ENDIF
                        IF (modifier == Modifiers.Volatile)
                            //
                            RETURN "volatile":Length
                        ENDIF
                    ELSE
                        //
                        IF (modifier == Modifiers.Unsafe)
                            //
                            RETURN "unsafe":Length
                        ENDIF
                        IF (modifier == Modifiers.Async)
                            //
                            RETURN "async":Length
                        ENDIF
                        IF (modifier == Modifiers.Ref)
                            //
                            RETURN "ref":Length
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
            THROW System.NotSupportedException{"Invalid value for Modifiers"}
        
        STATIC METHOD GetModifierName(modifier AS Modifiers) AS STRING
            //
            IF (modifier <= Modifiers.Override)
                //
                IF (modifier <= Modifiers.Abstract)
                    //
                    IF (modifier <= Modifiers.Protected)
                        //
                        SWITCH modifier
                            CASE Modifiers.Private
                                //
                                RETURN "private"
                            CASE Modifiers.Internal
                                //
                                RETURN "internal"
                            CASE Modifiers.Protected
                                //
                                RETURN "protected"
                            CASE (Modifiers.None | Modifiers.Any)
                                //
                                RETURN "any"
                        END SWITCH
                    ELSE
                        //
                        IF (modifier == Modifiers.Public)
                            //
                            RETURN "public"
                        ENDIF
                        IF (modifier == Modifiers.Abstract)
                            //
                            RETURN "abstract"
                        ENDIF
                    ENDIF
                ELSE
                    //
                    IF (modifier <= Modifiers.Sealed)
                        //
                        IF (modifier == Modifiers.Virtual)
                            //
                            RETURN "virtual"
                        ENDIF
                        IF (modifier == Modifiers.Sealed)
                            //
                            RETURN "sealed"
                        ENDIF
                    ELSE
                        //
                        IF (modifier == Modifiers.Static)
                            //
                            RETURN "static"
                        ENDIF
                        IF (modifier == Modifiers.Override)
                            //
                            RETURN "override"
                        ENDIF
                    ENDIF
                ENDIF
            ELSE
                //
                IF (modifier <= Modifiers.Partial)
                    //
                    IF (modifier <= Modifiers.Const)
                        //
                        IF (modifier == Modifiers.Readonly)
                            //
                            RETURN "readonly"
                        ENDIF
                        IF (modifier == Modifiers.Const)
                            //
                            RETURN "const"
                        ENDIF
                    ELSE
                        //
                        IF (modifier == Modifiers.New)
                            //
                            RETURN "new"
                        ENDIF
                        IF (modifier == Modifiers.Partial)
                            //
                            RETURN "partial"
                        ENDIF
                    ENDIF
                ELSE
                    //
                    IF (modifier <= Modifiers.Volatile)
                        //
                        IF (modifier == Modifiers.Extern)
                            //
                            RETURN "extern"
                        ENDIF
                        IF (modifier == Modifiers.Volatile)
                            //
                            RETURN "volatile"
                        ENDIF
                    ELSE
                        //
                        IF (modifier == Modifiers.Unsafe)
                            //
                            RETURN "unsafe"
                        ENDIF
                        IF (modifier == Modifiers.Async)
                            //
                            RETURN "async"
                        ENDIF
                        IF (modifier == Modifiers.Ref)
                            //
                            RETURN "ref"
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
            THROW System.NotSupportedException{"Invalid value for Modifiers"}
        
        STATIC METHOD GetModifierValue(modifier AS STRING) AS Modifiers
            //
            SWITCH (modifier)
                CASE "private"
                    RETURN Modifiers.Private
                CASE "internal"
                    RETURN Modifiers.Internal
                CASE "protected"
                    RETURN Modifiers.Protected
                CASE "public"
                    RETURN Modifiers.Public
                CASE "abstract"
                    RETURN Modifiers.Abstract
                CASE "virtual"
                    RETURN Modifiers.Virtual
                CASE "sealed"
                    RETURN Modifiers.Sealed
                CASE "static"
                    RETURN Modifiers.Static
                CASE "override"
                    RETURN Modifiers.Override
                CASE "readonly"
                    RETURN Modifiers.Readonly
                CASE "const"
                    RETURN Modifiers.Const
                CASE "new"
                    RETURN Modifiers.New
                CASE "partial"
                    RETURN Modifiers.Partial
                CASE "extern"
                    RETURN Modifiers.Extern
                CASE "volatile"
                    RETURN Modifiers.Volatile
                CASE "unsafe"
                    RETURN Modifiers.Unsafe
                CASE "async"
                    RETURN Modifiers.Async
                CASE "ref"
                    RETURN Modifiers.Ref
                CASE "any"
                    // even though it's used for pattern matching only, 'any' needs to be in this list to be usable in the AST
                    RETURN Modifiers.Any
                    default
                    THROW System.NotSupportedException{"Invalid value for Modifiers"}
            END SWITCH
        
        VIRTUAL METHOD ToString(formattingOptions AS CSharpFormattingOptions) AS STRING
            //
            RETURN CSharpModifierToken.GetModifierName(SELF:Modifier)
        
        
        // Properties
        STATIC PROPERTY AllModifiers AS System.Collections.Immutable.ImmutableArray<Modifiers>
            GET
                //
                RETURN ImmutableArray.Create(;
                Modifiers.Public, Modifiers.Private, Modifiers.Protected, Modifiers.Internal,;
                Modifiers.New,;
                Modifiers.Unsafe,;
                Modifiers.Abstract, Modifiers.Virtual, Modifiers.Sealed, Modifiers.Static, Modifiers.Override,;
                Modifiers.Readonly, Modifiers.Volatile,;
                Modifiers.Ref,;
                Modifiers.Extern, Modifiers.Partial, Modifiers.Const,;
                Modifiers.Async,;
                Modifiers.Any )
            END GET
        END PROPERTY
        
        OVERRIDE PROPERTY EndLocation AS TextLocation
            GET
                //
                RETURN TextLocation{SELF:StartLocation:Line, (SELF:StartLocation:Column + XSharpModifierToken.GetModifierLength(SELF:Modifier))}
            END GET
        END PROPERTY
        
        PROPERTY Modifier AS Modifiers
            GET
                //
                RETURN SELF:_modifier
            END GET
            SET
                //
                SUPER:ThrowIfFrozen()
                SELF:modifier := value
            END SET
        END PROPERTY
        
        
    END CLASS
    
    
END NAMESPACE // ILSpy.XSharpLanguage