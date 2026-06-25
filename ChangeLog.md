# ChangeLog

## ILSPY10 branch — ILSpy 10.1 / .NET 10 (current)

### In progress
- Port to ILSpy 10.1 and .NET 10
- Project converted to SDK-style (.xsproj), targeting `net10.0` with `UseWPF=true`
- `IModule.PEFile` replaced by `IModule.MetadataFile` (cast to `PEFile` where needed)
- `TypeToString` signature updated: second argument changed from `bool` to `ConversionFlags`
- `ParameterModifier` enum replaced by `ReferenceKind`; `params` keyword via `IsParams` property
- `XSharpTokenNode` now inherits from `CSharpTokenNode` (required by ILSpy 10 visitor pattern)
- `TokenRole` API updated: removed `TokenIndex`/`Tokens`/`TokenLengths`, using `Token`/`Length`
- `IAstVisitor` updated: no `VisitXSharpTokenNode`; using `VisitNullNode`/`VisitCSharpTokenNode`
- `XSharpModifierToken` case-sensitivity fixes for `/cs+` compilation mode
- `PeekOrDefault` extension method call inlined (CLI build limitation)
- Option page UI converted to pure code-behind (WPF XAML markup compiler does not support `Language=X#` in CLI builds)
- Compiler options `/allowdot-` and `/cs+` added for correct member access disambiguation

---

## ILSPY7 branch — ILSpy 7.x / .NET 4.7.2

### Version 1.3.3
- Switched to case-sensitive compilation (`/cs+`) and added `/allowdot-`
- Fixed ambiguity between `Expression.Null` (field) and `Expression` (type) in XsRoles
- Changed some `.` operators to `:` to help the compiler distinguish static vs instance members
- Fixed exception for `FieldDirection.In`; added support for `ParameterModifier.In`

### Previous fixes (ILSPY7)
- Force "standard" SWITCH code generation
- Correction on binary operator output
- Correction on escaped string generation
- Correction in FOREACH variable declaration

---

## ILSPY6 branch — ILSpy 6.x

### Version 6.2.1 (2022-04-21)
- Correction in escaped string generation
- Correction on FOREACH variable declaration
- Removed `DecompileProject` (whole-project decompilation)
- Correcting `LOOP` generation (was incorrectly generating `Continue`)
- Added `DecompileProject` support (later removed)
- Literal strings with all characters between 32–127 are no longer prefixed with `e`
- Correction on string literal generation
- First ILSpy 6 version

---

## ILSPY5 branch — ILSpy 5.x

### Version 5.x (2020)
- Correction on IMPLICIT and EXPLICIT operator syntax
- Added setting to enable/disable parentheses around IF condition
- Correction on SWITCH / END SWITCH
- Correction for fully qualified method names (e.g. `IDisposable.Dispose`)
- Correction on OPERATOR method output
- LOCAL declaration of OUT vars; declaration of LOCALs from FINALLY blocks
- First move to ILSpy 6 (preparation)
- Strings prefixed with `e` (extended string literal, as ILSpy retrieves them)
- `SUPER` generated instead of `Base` in constructor calls
- LINQ keywords colored; semicolons added at end of Query expressions
- DELEGATE: return type fix
- Corrections in EVENT handling
- Upgrade to ILSpy 5.02
- Prefix identifier matching a keyword with `@@`
- Generic constraint WHERE clause fixes
- Generic constraint for generic METHOD moved after return type

---

## Early versions — ILSpy 3.x / 4.x

### ILSpy 4.x support
- First step to ILSpy 4.x support
- Merge from 3.x codebase

### ILSpy 3.x (initial release)
- Update to ILSpy 3.2: added missing declarations to OutputVisitor
- Correction on anonymous methods
- Correction on CATCH clause variable declaration
- Support for AUTO keyword in PROPERTY
- Added Var declaration in FOR...NEXT
- Added Clipper calling convention support
- Corrections for OPERATOR statement
- BREAK statement suppressed in SWITCH structure
- Correction on PROPERTY setter; GET-only syntax fix
- Added XSharpOptionPage with UpperKeyword setting
- Changed PROPERTY generation
- Added Readme.md and License.txt
- Better handling of references (DOT vs COLON selector)
- Handle array creation and initialization; group elements in BinaryExpression
- Added `.AND.` / `.OR.` operators; LOCAL declaration with FOR/FOREACH
- Added `BEGIN CHECKED ... END CHECKED`
- Always use DOT as selector
- Generate LOCALs in WHILE statement
- Corrections on FOR..NEXT and FOREACH..NEXT; added SELF prefix
- First public release
