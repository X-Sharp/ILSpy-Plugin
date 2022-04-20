USING System
USING System.IO
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.ComponentModel.Composition
USING ICSharpCode.ILSpy
USING ICSharpCode.Decompiler
USING ICSharpCode.Decompiler.Metadata
USING ICSharpCode.Decompiler.CSharp
USING ICSharpCode.Decompiler.CSharp.Syntax
USING ICSharpCode.Decompiler.CSharp.Transforms
USING ICSharpCode.Decompiler.CSharp.OutputVisitor
USING ICSharpCode.Decompiler.TypeSystem
USING ICSharpCode.Decompiler.DebugInfo
USING Mono.Cecil
USING System.Reflection.Metadata
USING System.Reflection.PortableExecutable
USING ICSharpCode.Decompiler.CSharp.ProjectDecompiler

USING ICSharpCode.Decompiler.Solution
USING System.Threading

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
		
		PUBLIC OVERRIDE METHOD DecompileMethod( methoddef AS IMethod, output AS ITextOutput, options AS DecompilationOptions) AS VOID
			LOCAL decompiler AS CSharpDecompiler
			LOCAL assembly AS PEFile
			LOCAL method2 AS IMethod
			LOCAL isReferenceType AS LOGIC?
			LOCAL definitions AS List<EntityHandle>
			//
			SELF:WriteCommentLine(output, SELF:TypeToString( methoddef:DeclaringType, TRUE))
			//
			assembly := methoddef:ParentModule:PEFile
			decompiler := SELF:CreateDecompiler( assembly, options)
			// Check if we have a Constructor
			method2 := decompiler:TypeSystem:MainModule:ResolveEntity(methoddef:MetadataToken) ASTYPE IMethod
			IF (method2:IsConstructor .AND. method2:DeclaringType:IsReferenceType != FALSE )
				definitions := CollectFieldsAndCtors(method2:DeclaringTypeDefinition, method2:IsStatic)
				decompiler:AstTransforms:Add(SelectCtorTransform{method2})
				SELF:WriteCode(output, options:DecompilerSettings, decompiler:Decompile(definitions), decompiler:TypeSystem)
				RETURN
			ENDIF
			SELF:WriteCode(output, options:DecompilerSettings, decompiler:Decompile(methoddef:MetadataToken), decompiler:TypeSystem)
			RETURN
		
		PUBLIC OVERRIDE METHOD DecompileProperty(propDef AS IProperty, output AS ITextOutput, options AS DecompilationOptions) AS VOID
			LOCAL decompiler AS CSharpDecompiler
			LOCAL assembly AS PEFile
			//
			assembly := propDef:ParentModule:PEFile
			decompiler := SELF:CreateDecompiler(assembly, options)
			SELF:WriteCommentLine(output, SELF:TypeToString(propDef:DeclaringType, TRUE))
			SELF:WriteCode(output, options:DecompilerSettings, decompiler:Decompile(propDef:MetadataToken), decompiler:TypeSystem)
		
		PUBLIC OVERRIDE METHOD DecompileField(fieldDef AS IField, output AS ITextOutput, options AS DecompilationOptions) AS VOID
			LOCAL assembly AS PEFile
			LOCAL decompiler AS CSharpDecompiler
			LOCAL definitions AS List<EntityHandle>
			LOCAL definition AS IField
			//
			assembly := fieldDef:ParentModule:PEFile
			WriteCommentLine(output, SELF:TypeToString(fieldDef:DeclaringType, TRUE))
			decompiler := SELF:CreateDecompiler(assembly, options)
			IF (fieldDef:IsConst)
				SELF:WriteCode(output, options:DecompilerSettings, decompiler:Decompile(fieldDef:MetadataToken), decompiler:TypeSystem)
				RETURN
			ENDIF
			definitions := CollectFieldsAndCtors(fieldDef:DeclaringTypeDefinition, fieldDef:IsStatic)
			definition := decompiler:TypeSystem:MainModule:GetDefinition((FieldDefinitionHandle)fieldDef:MetadataToken)
			decompiler:AstTransforms:Add(SelectFieldTransform{definition})
			SELF:WriteCode(output, options:DecompilerSettings, decompiler:Decompile(definitions), decompiler:TypeSystem)
		
		PUBLIC OVERRIDE  METHOD DecompileType(typeDef AS ITypeDefinition, output AS ITextOutput, options AS DecompilationOptions) AS VOID
			LOCAL assembly AS PEFile
			LOCAL decompiler AS CSharpDecompiler
			//
			assembly := typeDef:ParentModule:PEFile
			WriteCommentLine(output, SELF:TypeToString(typeDef, TRUE))
			decompiler := SELF:CreateDecompiler(assembly, options)
			SELF:WriteCode(output, options:DecompilerSettings, decompiler:Decompile(typeDef:MetadataToken), decompiler:TypeSystem)
		
		PUBLIC OVERRIDE METHOD DecompileEvent(evt AS IEvent , output AS ITextOutput , options AS DecompilationOptions ) AS VOID
			LOCAL assembly AS PEFile
			LOCAL decompiler AS CSharpDecompiler
			//
			assembly := evt:ParentModule:PEFile
			WriteCommentLine(output, SELF:TypeToString(evt:DeclaringType, TRUE))
			decompiler := SELF:CreateDecompiler(assembly, options)
			SELF:WriteCode(output, options:DecompilerSettings, decompiler:Decompile(evt:MetadataToken), decompiler:TypeSystem)
		
		PUBLIC OVERRIDE METHOD DecompileAssembly(asmbly AS LoadedAssembly , output AS ITextOutput , options AS DecompilationOptions ) AS ICSharpCode.Decompiler.Solution.ProjectId
			//            LOCAL result AS ModuleDefinition
			//            //LOCAL iLSpyWholeProjectDecompiler AS ILSpyWholeProjectDecompiler
			//            LOCAL moduleDefinition AS ModuleDefinition
			//            LOCAL runtimeDisplayName AS STRING
			//            LOCAL decompiler AS CSharpDecompiler
			//            LOCAL syntax AS SyntaxTree
			//            //
			//            result := assembly:GetModuleDefinitionAsync():Result
			//            //IF (options:FullDecompilation && options:SaveAsProjectDirectory != NULL)
			//            //iLSpyWholeProjectDecompiler := ILSpyWholeProjectDecompiler{assembly, options}
			//            //iLSpyWholeProjectDecompiler:DecompileProject(result, options:SaveAsProjectDirectory, TextOutputWriter{output}, options:CancellationToken)
			//            //ELSE
			//            output:WriteLine()
			//            SUPER:DecompileAssembly(assembly, output, options)
			//            moduleDefinition := result
			//            IF (moduleDefinition:Types:Count > 0)
			//                output:Write("// Global type: ")
			//                output:WriteReference(moduleDefinition:Types[0]:FullName, moduleDefinition:Types[0], FALSE)
			//                output:WriteLine()
			//            ENDIF
			//            IF (moduleDefinition:EntryPoint != NULL)
			//                output:Write("// Entry point: ")
			//                output:WriteReference(moduleDefinition:EntryPoint:DeclaringType:FullName + ":" + moduleDefinition:EntryPoint:Name, moduleDefinition:EntryPoint, FALSE)
			//                output:WriteLine()
			//            ENDIF
			//            output:WriteLine("// Architecture: " + GetPlatformDisplayName(moduleDefinition))
			//            IF ((moduleDefinition:Attributes & ModuleAttributes:ILOnly) == (ModuleAttributes)0)
			//                output:WriteLine("// This assembly contains unmanaged code.")
			//            ENDIF
			//            runtimeDisplayName := GetRuntimeDisplayName(moduleDefinition)
			//            IF (runtimeDisplayName != NULL)
			//                output:WriteLine("// Runtime: " + runtimeDisplayName)
			//            ENDIF
			//            output:WriteLine()
			//            BEGIN USING IIF(options:FullDecompilation , NULL , LoadedAssembly:DisableAssemblyLoad())
			//                decompiler := CSharpDecompiler{result, options:DecompilerSettings}
			//                decompiler:CancellationToken := options:CancellationToken
			//                syntax := IIF((!options:FullDecompilation) , decompiler:DecompileModuleAndAssemblyAttributes() , decompiler:DecompileWholeModuleAsSingleFile())
			//                WriteCode(output, options:DecompilerSettings, syntax, decompiler:TypeSystem)
			//            END USING
			//            //ENDIF
			LOCAL assembly AS PEFile
			LOCAL prjDecompiler AS XSharpWholeProjectDecompiler
			LOCAL assemblyResolver AS ICSharpCOde.Decompiler.Metadata.IAssemblyResolver
			LOCAL dcmpTypeSystem AS DecompilerTypeSystem
			LOCAL tpeDefinition AS ITypeDefinition
			LOCAL metadata AS MetadataReader
			LOCAL cHeader AS CorHeader
			LOCAL methodReference AS EntityHandle
			LOCAL methodRslvd AS IMethod
			LOCAL runtimeDisplayName AS STRING
			LOCAL asmblyDefinition AS System.Reflection.Metadata.AssemblyDefinition
			LOCAL blbReader AS BlobReader
			LOCAL debugInfoOrNull AS IDebugInfoProvider
			LOCAL syntax AS SyntaxTree
			LOCAL decompiler AS CSharpDecompiler
			//
			assembly := asmbly:GetPEFileOrNull()
			IF ((options:FullDecompilation) .AND. (options:SaveAsProjectDirectory != NULL))
				prjDecompiler := XSharpWholeProjectDecompiler{asmbly, options}
				RETURN prjDecompiler:DecompileProject(assembly, options:SaveAsProjectDirectory, TextOutputWriter{output}, options:CancellationToken)
			ENDIF
			//			SELF:AddReferenceAssemblyWarningMessage(assembly, output)
			//			SELF:AddReferenceWarningMessage(assembly, output)
			output:WriteLine()
			SUPER:DecompileAssembly(asmbly, output, options)
			//BEGIN USING IIF(options:FullDecompilation , NULL , LoadedAssembly.DisableAssemblyLoad())
			assemblyResolver := asmbly:GetAssemblyResolver()
			dcmpTypeSystem := DecompilerTypeSystem{assembly, assemblyResolver, options:DecompilerSettings}
			tpeDefinition := dcmpTypeSystem:MainModule:TypeDefinitions:FirstOrDefault()
			IF (tpeDefinition != NULL)
				output:Write("// Global type: ")
				output:WriteReference(tpeDefinition, tpeDefinition:FullName)
				output:WriteLine()
			ENDIF
			metadata := assembly:Metadata
			cHeader := assembly:Reader:PEHeaders:CorHeader
			methodReference := MetadataTokenHelpers.EntityHandleOrNil(cHeader:EntryPointTokenOrRelativeVirtualAddress)
			IF ((!methodReference:IsNil) .AND. (methodReference:Kind == HandleKind.MethodDefinition))
				methodRslvd := dcmpTypeSystem:MainModule:ResolveMethod(methodReference, DEFAULT(ICSharpCode.Decompiler.TypeSystem.GenericContext))
				IF (methodRslvd != NULL)
					output:Write("// Entry point: ")
					output:WriteReference(methodRslvd, methodRslvd:DeclaringType:FullName + "." + methodRslvd:Name)
					output:WriteLine()
				ENDIF
			ENDIF
			output:WriteLine("// Architecture: " + Language.GetPlatformDisplayName(assembly))
			IF ((cHeader:Flags & CorFlags.ILOnly) == (CorFlags)0)
				output:WriteLine("// This assembly contains unmanaged code.")
			ENDIF
			runtimeDisplayName := Language.GetRuntimeDisplayName(assembly)
			IF (runtimeDisplayName != NULL)
				output:WriteLine("// Runtime: " + runtimeDisplayName)
			ENDIF
			IF ((cHeader:Flags & CorFlags.StrongNameSigned) != 0)
				output:WriteLine("// This assembly is signed with a strong name key.")
			ENDIF
			IF (metadata:IsAssembly)
				asmblyDefinition := metadata:GetAssemblyDefinition()
				IF (asmblyDefinition:HashAlgorithm != 0)
					output:WriteLine("// Hash algorithm: " + asmblyDefinition:HashAlgorithm:ToString():ToUpper())
				ENDIF
				IF (!asmblyDefinition:PublicKey:IsNil)
					output:Write("// Public key: ")
					blbReader := metadata:GetBlobReader(asmblyDefinition:PublicKey)
					WHILE blbReader:RemainingBytes > 0
						output:Write(blbReader:ReadByte():ToString("x"))
					END WHILE
					output:WriteLine()
				ENDIF
			ENDIF
			debugInfoOrNull := assembly:GetDebugInfoOrNull()
			IF (debugInfoOrNull != NULL)
				output:WriteLine("// Debug info: " + debugInfoOrNull:Description)
			ENDIF
			output:WriteLine()
			decompiler := CSharpDecompiler{dcmpTypeSystem, options:DecompilerSettings}
			decompiler:CancellationToken := options:CancellationToken
			syntax := IIF((!options:FullDecompilation) , decompiler:DecompileModuleAndAssemblyAttributes() , decompiler:DecompileWholeModuleAsSingleFile())
			SELF:WriteCode( output, options:DecompilerSettings, syntax, decompiler:TypeSystem)
			//
			RETURN NULL
		
		
		PRIVATE METHOD WriteCode(output AS ITextOutput, settings AS DecompilerSettings, syntaxTree AS SyntaxTree, typeSystem AS IDecompilerTypeSystem) AS VOID
			LOCAL visitor AS InsertParenthesesVisitor
			LOCAL writer1 AS TextTokenWriter
			LOCAL decoratedWriter AS TokenWriter
			LOCAL textOutput AS ISmartTextOutput
			LOCAL xsSettings AS XSharpOptions
			//
			xsSettings := XSharpOptionPage.CurrentXSharpSettings
			//
			visitor := InsertParenthesesVisitor{} 
			visitor:InsertParenthesesForReadability:=TRUE
			syntaxTree:AcceptVisitor(visitor)
			writer1 := TextTokenWriter{output, settings, typeSystem} 
			//writer1:FoldBraces:=settings:FoldBraces
			//writer1:ExpandMemberDefinitions:=settings:ExpandMemberDefinitions
			decoratedWriter := writer1
			textOutput := output ASTYPE ISmartTextOutput
			IF (textOutput != NULL)
				//
				decoratedWriter := XSharpHighlightingTokenWriter{decoratedWriter, textOutput, xsSettings }
			ENDIF
			syntaxTree:AcceptVisitor(XSharpOutputVisitor{decoratedWriter, settings:CSharpFormattingOptions, typeSystem })
		
		
		
		PRIVATE METHOD CreateDecompiler(module AS PEFile, options AS DecompilationOptions ) AS CSharpDecompiler
			LOCAL decompiler AS CSharpDecompiler
			//
			decompiler := CSharpDecompiler{module, module:GetAssemblyResolver(), options:DecompilerSettings}
			decompiler:CancellationToken := options:CancellationToken
			decompiler:DebugInfoProvider := module:GetDebugInfoOrNull()
			WHILE ( decompiler.AstTransforms.Count > INT32.MaxValue )
				decompiler:AstTransforms:RemoveAt(decompiler:AstTransforms:Count - 1)
			ENDDO
			//            decompiler:AstTransforms:Add( EscapeInvalidIdentifiers{})
			RETURN decompiler
		
			//		PUBLIC STATIC METHOD GetPlatformDisplayName(module AS PEFile ) AS STRING
			//			LOCAL pEHeaders AS PEHeaders
			//			LOCAL machine AS Machine
			//			LOCAL characteristics AS Characteristics
			//			LOCAL flags AS CorFlags
			//			//
			//			pEHeaders := module:Reader:PEHeaders
			//			machine := pEHeaders:CoffHeader:Machine
			//			characteristics := pEHeaders:CoffHeader:Characteristics
			//			flags := pEHeaders:CorHeader:Flags
			//			BEGIN SWITCH machine
			//			CASE Machine.I386
			//				IF ((flags & CorFlags.Prefers32Bit) != 0)
			//					RETURN "AnyCPU (32-bit preferred)"
			//				ENDIF
			//				IF ((flags & CorFlags.Requires32Bit) != 0)
			//					RETURN "x86"
			//				ENDIF
			//				IF (((flags & CorFlags.ILOnly) == (CorFlags)0) .AND. ((characteristics & Characteristics.Bit32Machine) != 0))
			//					RETURN "x86"
			//				ENDIF
			//				RETURN "AnyCPU (64-bit preferred)"
			//			CASE Machine.Amd64
			//				RETURN "x64"
			//			CASE Machine.IA64
			//				RETURN "Itanium"
			//			OTHERWISE
			//				RETURN machine:ToString()
			//			END SWITCH
		
			//PUBLIC STATIC METHOD GetRuntimeDisplayName(module AS PEFile ) AS STRING
			//	RETURN module:Metadata:MetadataVersion
		
		PRIVATE STATIC METHOD CollectFieldsAndCtors(type AS ITypeDefinition , isStatic AS LOGIC ) AS List<EntityHandle>
			LOCAL list AS List<EntityHandle>
			LOCAL metadataToken AS EntityHandle
			//
			list := List<EntityHandle>{}
			//metadataToken
			FOREACH ivarFIELD AS IField IN type:Fields 
				metadataToken := ivarFIELD:MetadataToken
				IF ((!metadataToken:IsNil) .AND. (ivarFIELD:IsStatic == isStatic))
					list:Add(ivarFIELD:MetadataToken)
				ENDIF
			NEXT
			FOREACH iVarMETHOD AS IMethod IN type:Methods 
				metadataToken := iVarMETHOD:MetadataToken
				IF (((!metadataToken:IsNil) .AND. (iVarMETHOD:IsConstructor)) .AND. (iVarMETHOD:IsStatic == isStatic))
					list:Add(iVarMETHOD:MetadataToken)
				ENDIF
			NEXT
			RETURN list
			
			// This is used in the TreeView of the Assembly (Left Window)
			/*
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
			RETURN type.Name
			RETURN SELF:ConvertTypeToString(ConvertTypeOptions.IncludeTypeParameterDefinitions | ConvertTypeOptions.DoNotUsePrimitiveTypeNames, type, NULL)
			
			PRIVATE METHOD ConvertTypeToString(options AS ConvertTypeOptions , typeRef AS TypeReference , typeAttributes := NULL AS ICustomAttributeProvider ) AS STRING
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
			IF ((compType != NULL) .AND. (compType:PointerRank > 0))
			compType:PointerRank--
			ENDIF
			ENDIF
			astType:AcceptVisitor(XSharpOutputVisitor{sWriter, FormattingOptionsFactory.CreateEmpty(), NULL})
			RETURN sWriter:ToString()
			
			
			
			
			*/
	END CLASS
	
	
	
	CLASS SelectCtorTransform IMPLEMENTS IAstTransform
		PRIVATE INITONLY ctor AS IMethod
		PRIVATE INITONLY removedSymbols := HashSet<ISymbol>{} AS HashSet<ISymbol>
		
		
		PUBLIC CONSTRUCTOR(ctor AS IMethod )
			SELF:ctor := ctor
		
		
		PUBLIC METHOD Run(rootNode AS AstNode , context AS TransformContext ) AS VOID
			LOCAL ctorDeclaration AS ConstructorDeclaration
			LOCAL currentAstNode AS AstNode
			LOCAL ctorDeclaration2 AS ConstructorDeclaration
			LOCAL fldDeclaration AS FieldDeclaration
			LOCAL fldDeclaration2 AS FieldDeclaration
			LOCAL ctorDeclaration3 AS ConstructorDeclaration
			LOCAL currentAstNode2 AS AstNode
			LOCAL fldDeclaration3 AS FieldDeclaration
			LOCAL fldDeclaration4 AS FieldDeclaration
			//
			ctorDeclaration := NULL
			FOREACH child AS AstNode IN rootNode:Children 
				currentAstNode := child
				IF (currentAstNode != NULL)
					IF ((ctorDeclaration2 := (currentAstNode ASTYPE ConstructorDeclaration)) == NULL)
						IF ((fldDeclaration := (currentAstNode ASTYPE FieldDeclaration)) != NULL)
							fldDeclaration2 := fldDeclaration
							IF (fldDeclaration2:Variables:All({v AS VariableInitializer => v:Initializer:IsNull}))
								fldDeclaration2:Remove()
								SELF:removedSymbols:Add(fldDeclaration2:GetSymbol())
							ENDIF
						ENDIF
					ELSE
						ctorDeclaration3 := ctorDeclaration2
						IF (ctorDeclaration3:GetSymbol() == SELF:ctor)
							ctorDeclaration := ctorDeclaration3
						ELSE
							ctorDeclaration3:Remove()
							SELF:removedSymbols:Add(ctorDeclaration3:GetSymbol())
						ENDIF
					ENDIF
				ENDIF
			NEXT
			IF ((ctorDeclaration != NULL) .AND. (ctorDeclaration:Initializer:ConstructorInitializerType == ConstructorInitializerType.This))
				FOREACH child2 AS AstNode IN rootNode:Children 
					currentAstNode2 := child2
					IF ((currentAstNode2 != NULL) .AND. ((fldDeclaration3 := (currentAstNode2 ASTYPE FieldDeclaration)) != NULL))
						fldDeclaration4 := fldDeclaration3
						fldDeclaration4:Remove()
						SELF:removedSymbols:Add(fldDeclaration4:GetSymbol())
					ENDIF
				NEXT
			ENDIF
			FOREACH child3 AS AstNode IN rootNode:Children 
				IF ((child3 IS Comment) .AND. (SELF:removedSymbols:Contains(child3:GetSymbol())))
					child3:Remove()
				ENDIF
			NEXT
			
			
	END CLASS
	
	
	CLASS SelectFieldTransform IMPLEMENTS IAstTransform
		PRIVATE INITONLY fld AS IField
		
		PUBLIC CONSTRUCTOR(fld AS IField )
			SELF:fld := fld
		
		
		PUBLIC METHOD Run(rootNode AS AstNode , context AS TransformContext ) AS VOID
			LOCAL currentAstNode AS AstNode
			LOCAL entityDeclaration1 AS EntityDeclaration
			LOCAL cmt AS Comment
			LOCAL node AS Comment
			LOCAL entityDeclaration2 AS EntityDeclaration
			//
			FOREACH child AS AstNode IN rootNode:Children 
				currentAstNode := child
				IF (currentAstNode != NULL)
					IF ((entityDeclaration1 := (currentAstNode ASTYPE EntityDeclaration)) == NULL)
						IF ((cmt := (currentAstNode ASTYPE Comment)) != NULL)
							node := cmt
							IF (node:GetSymbol() != SELF:fld)
								child:Remove()
							ENDIF
						ENDIF
					ELSE
						entityDeclaration2 := entityDeclaration1
						IF (child:GetSymbol() != SELF:fld)
							child:Remove()
						ENDIF
					ENDIF
				ENDIF
			NEXT
			
			
	END CLASS	
	
	
	CLASS XSharpWholeProjectDecompiler INHERIT WholeProjectDecompiler
		PRIVATE INITONLY assembly AS LoadedAssembly
		PRIVATE INITONLY options AS DecompilationOptions
		
		PUBLIC CONSTRUCTOR(assembly AS LoadedAssembly , options AS DecompilationOptions )
			SUPER(options:DecompilerSettings, assembly:GetAssemblyResolver(), assembly:GetDebugInfoOrNull() )
			SELF:assembly := assembly
			SELF:options := options
		
		
		PROTECTED OVERRIDE METHOD WriteResourceToFile(fileName AS STRING , resourceName AS STRING , entryStream AS Stream ) AS IEnumerable<ValueTuple<STRING, STRING>>
			//
			FOREACH exportedValue AS IResourceFileHandler IN App.ExportProvider:GetExportedValues<IResourceFileHandler>() 
				IF (exportedValue:CanHandle(fileName, SELF:options))
					entryStream:Position := 0L
					fileName := Path.Combine(targetDirectory, fileName)
					fileName := exportedValue:WriteResourceToFile(SELF:assembly, fileName, entryStream, SELF:options)
					RETURN <ValueTuple<STRING, STRING>>{ ValueTuple.Create<STRING, STRING>(exportedValue:EntryType, fileName) }
				ENDIF
			NEXT
			RETURN SUPER:WriteResourceToFile(fileName, resourceName, entryStream)
		
		/*

		PUBLIC METHOD DecompileProject(moduleDefinition AS PEFile , targetDirectory AS STRING , projectFileWriter AS TextWriter , cancellationToken := DEFAULT(CancellationToken) AS CancellationToken ) AS ProjectId
			LOCAL list AS List<Tuple<STRING, STRING>>
			//
			IF string.IsNullOrEmpty(targetDirectory)
				THROW InvalidOperationException{"Must set TargetDirectory"}
			ENDIF
			SELF:targetDirectory := targetDirectory
			SELF:directories:Clear()
			list := SELF:xsWriteCodeFilesInProject(moduleDefinition, cancellationToken)):ToList()
			list:AddRange(SELF:WriteResourceFilesInProject(moduleDefinition))
			IF SELF:StrongNameKeyFile != NULL
				File.Copy(SELF:StrongNameKeyFile, Path.Combine(targetDirectory, Path.GetFileName(SELF:StrongNameKeyFile)))
			ENDIF
			RETURN SELF:WriteProjectFile(projectFileWriter, list, moduleDefinition)			
		
		
		PRIVATE METHOD xsWriteCodeFilesInProject(module AS PEFile , cancellationToken AS CancellationToken ) AS IEnumerable<Tuple<STRING, STRING>>
			LOCAL metadata AS MetadataReader
			LOCAL list AS List<IGrouping<STRING, TypeDefinitionHandle>>
			LOCAL ts AS DecompilerTypeSystem
			LOCAL val AS ParallelOptions
			//
			metadata := module:Metadata
			list := Enumerable.ToList<IGrouping<STRING, TypeDefinitionHandle>>(Enumerable.GroupBy<TypeDefinitionHandle, STRING>(Enumerable.Where<TypeDefinitionHandle>(module:Metadata:GetTopLevelTypeDefinitions(), (@@Func<TypeDefinitionHandle, LOGIC>)({td AS TypeDefinitionHandle => SELF:IncludeTypeWhenDecompilingProject(module, td)})), (@@Func<TypeDefinitionHandle, STRING>){
			(h AS TypeDefinitionHandle ) =>
			typeDefinition := metadata:GetTypeDefinition(h)
			text := CleanUpFileName(metadata:GetString(typeDefinition:Name)) + ".cs"
			IF string.IsNullOrEmpty(metadata:GetString(typeDefinition:Namespace))
				RETURN text
			ENDIF
			text2 := CleanUpFileName(metadata:GetString(typeDefinition:Namespace))
			IF SELF:directories:Add(text2)
				Directory.CreateDirectory(Path.Combine(SELF:targetDirectory, text2))
			ENDIF
			RETURN Path.Combine(text2, text)
			},
			(IEqualityComparer<STRING>)StringComparer.OrdinalIgnoreCase))
			//
			ts := DecompilerTypeSystem{module, SELF:AssemblyResolver, SELF:settings}
			val := ParallelOptions{}
			val:set_MaxDegreeOfParallelism(SELF:MaxDegreeOfParallelism)
			val:set_CancellationToken(cancellationToken)
			Parallel.ForEach<IGrouping<STRING, TypeDefinitionHandle>>((IEnumerable<IGrouping<STRING, TypeDefinitionHandle>>)list, val, (Action<IGrouping<STRING, TypeDefinitionHandle>>){(@@file AS IGrouping<STRING, TypeDefinitionHandle> ) =>
			val2 := StreamWriter{Path.Combine(SELF:targetDirectory, @@file:get_Key())}
			TRY
				cSharpDecompiler := SELF:CreateDecompiler(ts)
				cSharpDecompiler:CancellationToken := cancellationToken
				syntaxTree := cSharpDecompiler:DecompileTypes(Enumerable.ToArray<TypeDefinitionHandle>((IEnumerable<TypeDefinitionHandle>)@@file))
				syntaxTree:AcceptVisitor(CSharpOutputVisitor{(TextWriter)(OBJECT)val2, SELF:settings:CSharpFormattingOptions})
					
			CATCH ex AS Exception WHEN ((!(ex IS OperationCanceledException)) .AND. (!(ex IS DecompilerException)))
				THROW DecompilerException{module, "Error decompiling for '" + @@file:get_Key() + "'", ex}
				
			FINALLY
				((IDisposable)val2)?:Dispose()
			END TRY
			})
			RETURN Enumerable.Concat<Tuple<STRING, STRING>>(Enumerable.Select<IGrouping<STRING, TypeDefinitionHandle>, Tuple<STRING, STRING>>((IEnumerable<IGrouping<STRING, TypeDefinitionHandle>>)list, (@@Func<IGrouping<STRING, TypeDefinitionHandle>, Tuple<STRING, STRING>>)({f AS IGrouping<STRING, TypeDefinitionHandle> => Tuple.Create(e"Compile", f:get_Key())})), SELF:WriteAssemblyInfo(ts, cancellationToken))
			
			*/

	END CLASS
		
		
END NAMESPACE
