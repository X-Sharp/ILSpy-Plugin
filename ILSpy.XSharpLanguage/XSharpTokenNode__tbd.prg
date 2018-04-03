// XSharpTokenNode.prg
// Created by    : fabri
// Creation Date : 3/31/2018 12:02:17 PM
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
    /// The XSharpTokenNode class.
    /// </summary>
    CLASS XSharpTokenNode INHERIT AstNode
        // Fields
        STATIC INITONLY PUBLIC NULLTokenNode := NullXSharpTokenNode{} AS XSharpTokenNode
        
        PRIVATE _startLocation AS TextLocation
        
        // Methods
        CONSTRUCTOR(location AS TextLocation, role AS TokenRole);SUPER()
            //
            SELF:_startLocation := location
            IF (role != NULL)
                //
            SUPER:flags := (SUPER:flags | (role:TokenIndex << 10))
            ENDIF
            
        VIRTUAL METHOD AcceptVisitor(visitor AS IAstVisitor) AS VOID
            //
            visitor:VisitCSharpTokenNode(SELF)
            
        VIRTUAL METHOD AcceptVisitor<T>(visitor AS IAstVisitor<T>) AS T
            //
            RETURN visitor:VisitXSharpTokenNode(SELF)
            
        VIRTUAL METHOD AcceptVisitor<T, S>(visitor AS IAstVisitor<T, S>, data AS T) AS S
            //
            RETURN visitor:VisitXSharpTokenNode(SELF, data)
            
        PROTECTED OVERRIDE METHOD DoMatch(other AS AstNode, match AS Match) AS LOGIC
            LOCAL node AS XSharpTokenNode
            //
            node := (XSharpTokenNode)(other)
            RETURN (((node != NULL) .AND. ! node:IsNull) .AND. ! (node IS CSharpModifierToken))
        
        VIRTUAL METHOD ToString(formattingOptions AS CSharpFormattingOptions) AS STRING
            //
            RETURN TokenRole.Tokens:Item[(LONG)(SUPER:flags >> 10) ]
            
            
            // Properties
        VIRTUAL PROPERTY EndLocation AS TextLocation
            GET
                //
            RETURN TextLocation{SELF:StartLocation:Line, (SELF:StartLocation:Column + SELF:TokenLength)}
            END GET
        END PROPERTY
        
        VIRTUAL PROPERTY NodeType AS NodeType
            GET
                //
            RETURN NodeType.Token
            END GET
        END PROPERTY
        
        VIRTUAL PROPERTY StartLocation AS TextLocation
            GET
                //
            RETURN SELF:_startLocation
            END GET
        END PROPERTY
        
        PRIVATE PROPERTY TokenLength AS LONG
            GET
                //
            RETURN TokenRole.TokenLengths:Item[(LONG)(SUPER:flags >> 10) ]
            END GET
        END PROPERTY
        
        
        
        
    END CLASS
    
    // Nested Types ??
    INTERNAL CLASS NullXSharpTokenNode INHERIT XSharpTokenNode
        // Methods
        CONSTRUCTOR()//Inline call to base() in C#
            SUPER(TextLocation.Empty, NULL)
            
            
        VIRTUAL METHOD AcceptVisitor(visitor AS IAstVisitor) AS VOID
            //
            visitor:VisitNullNode(SELF)
            
        VIRTUAL METHOD AcceptVisitor<T>(visitor AS IAstVisitor<T>) AS T
            //
            RETURN visitor:VisitNullNode(SELF)
            
        VIRTUAL METHOD AcceptVisitor<T, S>(visitor AS IAstVisitor<T, S>, data AS T) AS S
            //
            RETURN visitor:VisitNullNode(SELF, data)
            
        PROTECTED VIRTUAL METHOD DoMatch(other AS AstNode, match AS Match) AS LOGIC
            //
            IF (other != NULL)
                //
            RETURN other:IsNull
            ENDIF
            RETURN TRUE
            
            
            // Properties
        VIRTUAL PROPERTY IsNull AS LOGIC
            GET
                //
            RETURN TRUE
            END GET
        END PROPERTY
        
        
    END CLASS
    
    
END NAMESPACE // ILSpy.XSharpLanguage