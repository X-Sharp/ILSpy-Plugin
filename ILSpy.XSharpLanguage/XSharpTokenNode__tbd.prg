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
    CLASS XSharpTokenNode INHERIT CSharpTokenNode
        // Fields
        STATIC INITONLY PUBLIC NULLTokenNode := NullXSharpTokenNode{} AS XSharpTokenNode

        // Methods
        CONSTRUCTOR(location AS TextLocation, role AS TokenRole)
            SUPER(location, role)

        PROTECTED OVERRIDE METHOD DoMatch(other AS AstNode, match AS Match) AS LOGIC
            LOCAL node AS XSharpTokenNode
            node := other ASTYPE XSharpTokenNode
            RETURN node != NULL .AND. ! node:IsNull

    END CLASS

    // Nested Types ??
    INTERNAL CLASS NullXSharpTokenNode INHERIT XSharpTokenNode
        // Methods
        CONSTRUCTOR()
            SUPER(TextLocation.Empty, NULL)

        OVERRIDE METHOD AcceptVisitor(visitor AS IAstVisitor) AS VOID
            visitor:VisitNullNode(SELF)

        OVERRIDE METHOD AcceptVisitor<T>(visitor AS IAstVisitor<T>) AS T
            RETURN visitor:VisitNullNode(SELF)

        OVERRIDE METHOD AcceptVisitor<T, S>(visitor AS IAstVisitor<T, S>, data AS T) AS S
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
