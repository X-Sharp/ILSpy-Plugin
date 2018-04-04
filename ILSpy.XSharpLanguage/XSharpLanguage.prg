


USING System
USING System.IO
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.ComponentModel.Composition
USING ICSharpCode.ILSpy
USING ICSharpCode.Decompiler
USING ICSharpCode.Decompiler.CSharp
USING ICSharpCode.Decompiler.CSharp.Syntax
USING ICSharpCode.Decompiler.CSharp.Transforms
USING ICSharpCode.Decompiler.CSharp.OutputVisitor
USING ICSharpCode.Decompiler.TypeSystem
USING Mono.Cecil

BEGIN NAMESPACE ILSpy.XSharpLanguage
    
    [ExportAttribute( TYPEOF(Language) )];
        PUBLIC CLASS XSharpLanguage INHERIT Language
        
        
        CONSTRUCTOR()
            RETURN
            
        PUBLIC OVERRIDE PROPERTY Name AS STRING
            GET             
            RETURN "XSharp"
            END GET
        END PROPERTY
        
        
        PUBLIC OVERRIDE PROPERTY FileExtension AS STRING
            GET
                // used in 'Save As' dialog
            RETURN ".prg"
            END GET
        END PROPERTY
        
        PUBLIC OVERRIDE PROPERTY ProjectFileExtension AS STRING
            GET
            RETURN ".xsproj"
            END GET
        END PROPERTY
        
        PUBLIC OVERRIDE METHOD DecompileMethod( methoddef AS MethodDefinition, output AS ITextOutput, options AS DecompilationOptions) AS VOID
            LOCAL decompiler AS CSharpDecompiler
            LOCAL definitions AS IMemberDefinition[]
            //
            SELF:WriteCommentLine(output, SELF:TypeToString(methoddef:DeclaringType, TRUE, NULL))
            decompiler := SELF:CreateDecompiler( methoddef:Module, options)
            definitions := <IMemberDefinition>{methoddef}
            SELF:WriteCode(output, options:DecompilerSettings, decompiler:Decompile(definitions), decompiler:TypeSystem)
            RETURN
        
        PUBLIC OVERRIDE METHOD DecompileProperty(propDef AS PropertyDefinition, output AS ITextOutput, options AS DecompilationOptions) AS VOID
            LOCAL decompiler AS CSharpDecompiler
            LOCAL definitions AS IMemberDefinition[]
            //
            SELF:WriteCommentLine(output, SELF:TypeToString(propDef:DeclaringType, TRUE, NULL))
            decompiler := SELF:CreateDecompiler( propDef:Module, options)
            definitions := <IMemberDefinition>{propDef}
            SELF:WriteCode(output, options:DecompilerSettings, decompiler:Decompile(definitions), decompiler:TypeSystem)
        
        PUBLIC OVERRIDE METHOD DecompileField(fieldDef AS FieldDefinition, output AS ITextOutput, options AS DecompilationOptions) AS VOID
            LOCAL decompiler AS CSharpDecompiler
            LOCAL definitions AS IMemberDefinition[]
            //
            SELF:WriteCommentLine(output, SELF:TypeToString(fieldDef:DeclaringType, TRUE, NULL))
            decompiler := SELF:CreateDecompiler( fieldDef:Module,  options)
            definitions := <IMemberDefinition>{fieldDef}
            SELF:WriteCode(output, options:DecompilerSettings, decompiler:Decompile(definitions), decompiler:TypeSystem)
        
        PUBLIC OVERRIDE  METHOD DecompileType(typeDef AS TypeDefinition, output AS ITextOutput, options AS DecompilationOptions) AS VOID
            LOCAL decompiler AS CSharpDecompiler
            LOCAL definitions AS IMemberDefinition[]
            //
            SELF:WriteCommentLine(output, SELF:TypeToString(typeDef, TRUE, NULL))
            decompiler := SELF:CreateDecompiler(typeDef:Module, options)
            definitions := <IMemberDefinition>{typeDef}
            SELF:WriteCode(output, options:DecompilerSettings, decompiler:Decompile(definitions), decompiler:TypeSystem)
        
        PUBLIC OVERRIDE METHOD DecompileEvent(ev AS EventDefinition , output AS ITextOutput , options AS DecompilationOptions ) AS VOID
            LOCAL decompiler AS CSharpDecompiler
            //
            WriteCommentLine(output, TypeToString(ev.DeclaringType, TRUE, NULL))
            decompiler := CreateDecompiler(ev.Module, options)
            WriteCode(output, options.DecompilerSettings, decompiler.Decompile(ev), decompiler.TypeSystem)
            
        
        PUBLIC OVERRIDE METHOD DecompileAssembly(assembly AS LoadedAssembly , output AS ITextOutput , options AS DecompilationOptions ) AS VOID
            LOCAL result AS ModuleDefinition
            //LOCAL iLSpyWholeProjectDecompiler AS ILSpyWholeProjectDecompiler
            LOCAL moduleDefinition AS ModuleDefinition
            LOCAL runtimeDisplayName AS STRING
            LOCAL decompiler AS CSharpDecompiler
            LOCAL syntax AS SyntaxTree
            //
            result := assembly:GetModuleDefinitionAsync():Result
            //IF (options:FullDecompilation && options:SaveAsProjectDirectory != NULL)
            //iLSpyWholeProjectDecompiler := ILSpyWholeProjectDecompiler{assembly, options}
            //iLSpyWholeProjectDecompiler:DecompileProject(result, options:SaveAsProjectDirectory, TextOutputWriter{output}, options:CancellationToken)
            //ELSE
            output:WriteLine()
            SUPER:DecompileAssembly(assembly, output, options)
            moduleDefinition := result
            IF (moduleDefinition:Types:Count > 0)
                output:Write("// Global type: ")
                output:WriteReference(moduleDefinition:Types[0]:FullName, moduleDefinition:Types[0], FALSE)
            output:WriteLine()
            ENDIF
            IF (moduleDefinition:EntryPoint != NULL)
                output:Write("// Entry point: ")
                output:WriteReference(moduleDefinition:EntryPoint:DeclaringType:FullName + ":" + moduleDefinition:EntryPoint:Name, moduleDefinition:EntryPoint, FALSE)
            output:WriteLine()
            ENDIF
            output:WriteLine("// Architecture: " + GetPlatformDisplayName(moduleDefinition))
            IF ((moduleDefinition:Attributes & ModuleAttributes:ILOnly) == (ModuleAttributes)0)
            output:WriteLine("// This assembly contains unmanaged code.")
            ENDIF
            runtimeDisplayName := GetRuntimeDisplayName(moduleDefinition)
            IF (runtimeDisplayName != NULL)
            output:WriteLine("// Runtime: " + runtimeDisplayName)
            ENDIF
            output:WriteLine()
            BEGIN USING IIF(options:FullDecompilation , NULL , LoadedAssembly:DisableAssemblyLoad())
                decompiler := CSharpDecompiler{result, options:DecompilerSettings}
                decompiler:CancellationToken := options:CancellationToken
                syntax := IIF((!options:FullDecompilation) , decompiler:DecompileModuleAndAssemblyAttributes() , decompiler:DecompileWholeModuleAsSingleFile())
            WriteCode(output, options:DecompilerSettings, syntax, decompiler:TypeSystem)
            END USING
            //ENDIF
            
        
        PRIVATE METHOD WriteCode(output AS ITextOutput, settings AS DecompilerSettings, syntaxTree AS SyntaxTree, typeSystem AS IDecompilerTypeSystem) AS VOID
            LOCAL visitor AS InsertParenthesesVisitor
            LOCAL writer1 AS TextTokenWriter
            LOCAL decoratedWriter AS TokenWriter
            LOCAL textOutput AS ISmartTextOutput
            //
            visitor := InsertParenthesesVisitor{} 
            visitor:InsertParenthesesForReadability:=TRUE
            syntaxTree:AcceptVisitor(visitor)
            writer1 := TextTokenWriter{output, settings, typeSystem} 
            writer1:FoldBraces:=settings:FoldBraces
            writer1:ExpandMemberDefinitions:=settings:ExpandMemberDefinitions
            decoratedWriter := writer1
            textOutput := output ASTYPE ISmartTextOutput
            IF (textOutput != NULL)
                //
            decoratedWriter := XSharpHighlightingTokenWriter{decoratedWriter, textOutput}
            ENDIF
            syntaxTree:AcceptVisitor(XSharpOutputVisitor{decoratedWriter, settings:CSharpFormattingOptions, typeSystem })
            
            
            
        PRIVATE METHOD CreateDecompiler(module AS ModuleDefinition, options AS DecompilationOptions ) AS CSharpDecompiler
            LOCAL decompiler AS CSharpDecompiler
            //
            decompiler := CSharpDecompiler{module, options:DecompilerSettings}
            decompiler:AstTransforms:Add( EscapeInvalidIdentifiers{})
            RETURN decompiler
            
        PUBLIC METHOD GetPlatformDisplayName(module AS ModuleDefinition ) AS STRING
            LOCAL architecture AS TargetArchitecture
            //
            architecture := module.Architecture
            BEGIN SWITCH architecture
            CASE TargetArchitecture.I386
                IF ((module.Attributes & ModuleAttributes.Preferred32Bit) == ModuleAttributes.Preferred32Bit)
                RETURN "AnyCPU (32-bit preferred)"
                ENDIF
                IF ((module.Attributes & ModuleAttributes.Required32Bit) == ModuleAttributes.Required32Bit)
                RETURN "x86"
                ENDIF
                RETURN "AnyCPU (64-bit preferred)"
            CASE TargetArchitecture.AMD64
                RETURN "x64"
            CASE TargetArchitecture.IA64
                RETURN "Itanium"
            OTHERWISE
                architecture := module.Architecture
                RETURN architecture.ToString()
            END SWITCH
            
        PUBLIC METHOD GetRuntimeDisplayName(module AS ModuleDefinition ) AS STRING
            BEGIN SWITCH module.Runtime
            CASE TargetRuntime.Net_1_0
                RETURN ".NET 1.0"
            CASE TargetRuntime.Net_1_1
                RETURN ".NET 1.1"
            CASE TargetRuntime.Net_2_0
                RETURN ".NET 2.0"
            CASE TargetRuntime.Net_4_0
                RETURN ".NET 4.0"
            OTHERWISE
                RETURN NULL
            END SWITCH
            
            // This is used in the TreeView of the Assembly (Left Window)
        PUBLIC OVERRIDE METHOD FormatMethodName( methd AS MethodDefinition ) AS STRING
            IF (methd == NULL)
            THROW ArgumentNullException{"method"}
            ENDIF
            IF (!methd.IsConstructor)
            RETURN methd.Name
            ENDIF
            //
            RETURN "Constructor"
        
        PUBLIC OVERRIDE METHOD FormatTypeName(type AS TypeDefinition ) AS STRING
            IF (type == NULL)
            THROW ArgumentNullException{"type"}
            ENDIF
            RETURN SELF:TypeToString(ConvertTypeOptions.IncludeTypeParameterDefinitions | ConvertTypeOptions.DoNotUsePrimitiveTypeNames, type, NULL)
        
        PRIVATE METHOD TypeToString(options AS ConvertTypeOptions , typeRef AS TypeReference , typeAttributes := NULL AS ICustomAttributeProvider ) AS STRING
            LOCAL astType AS AstType
            LOCAL compType AS ComposedType
            LOCAL sWriter AS StringWriter
            LOCAL parameterDefinition AS ParameterDefinition
            //
            astType := CSharpDecompiler.ConvertType(typeRef, typeAttributes, options)
            sWriter := StringWriter{}
            IF (typeRef:IsByReference)
                parameterDefinition := typeAttributes ASTYPE ParameterDefinition
                IF ( (parameterDefinition != NULL) .AND. (!parameterDefinition:IsIn .AND. parameterDefinition:IsOut) )
                    sWriter:Write("out ")
                ELSE
                    sWriter:Write("ref ")
                ENDIF
                compType := astType ASTYPE ComposedType
                IF ((compType != null) .AND. (compType:PointerRank > 0))
                    compType:PointerRank--
                ENDIF
            ENDIF
            astType:AcceptVisitor(XSharpOutputVisitor{sWriter, FormattingOptionsFactory.CreateEmpty(), null})
            RETURN sWriter:ToString()
            
            
            
            
    END CLASS
    
    
    
    
END NAMESPACE
