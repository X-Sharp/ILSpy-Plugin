### XSharp Language Plugin for ILSpy

This plugin adds X# (XSharp) as a decompilation language target in ILSpy.

#### Requirements

- [ILSpy 10.x](https://github.com/icsharpcode/ILSpy) (WPF version, .NET 10)
- [XSharp compiler](https://www.xsharp.eu) 3.x or later
- Visual Studio 2022 or later with the XSharp extension, **or** the XSharp CLI tools

#### Build instructions

1. Clone this repository. You will get:
   ```
   ILSpy-Plugin\
     ILSpy.XSharpLanguage\       ← plugin source
     ILSpy_Core\                 ← place ILSpy 10 binaries here
     ILSpy.XSharpLanguage.sln
   ```

2. Place the ILSpy 10 binaries in the `ILSpy_Core\` folder (copy the contents of the ILSpy installation or build output there). The required assemblies include at minimum:
   - `ILSpy.exe`, `ILSpy.dll`
   - `ICSharpCode.Decompiler.dll`, `ICSharpCode.ILSpyX.dll`
   - `ICSharpCode.AvalonEdit.dll`, `Mono.Cecil.dll`
   - `TomsToolbox.*.dll`, `System.Composition.AttributedModel.dll`

3. Build the solution from Visual Studio (with the XSharp extension installed), or from the command line:
   ```
   dotnet build ILSpy.XSharpLanguage\ILSpy.XSharpLanguage.xsproj -c Debug
   ```

4. The output `ILSpy.XSharpLanguage.Plugin.dll` will be in:
   ```
   ILSpy.XSharpLanguage\bin\Debug\net10.0\
   ```
   When building from Visual Studio with the solution file, the post-build event copies it automatically to `ILSpy_Core\`.

5. Copy `ILSpy.XSharpLanguage.Plugin.dll` alongside the ILSpy binaries, then launch ILSpy. The XSharp language option will appear in the decompiler language selector.

#### Version compatibility

| Plugin branch | ILSpy version | .NET target |
|---|---|---|
| ILSPY10 (current) | 10.x | net10.0 |
| master | 3.x | net4.6.1 |

#### About

This plugin is based on the C# language decompiler source in ILSpy (`ICSharpCode.Decompiler`) and adapts it to emit X# Core dialect syntax.

The generated code follows the **XSharp Core Dialect**.

Lots of things are still to be done, but most is working.

#### Features

- LOCAL variables are all defined on top of statement blocks
- Option page:
  - Set casing for keywords (uppercase/lowercase)
  - Put parentheses around IF condition

#### Known limitations

- Array access is always zero-based
- Only dot (`.`) is used as member selector (`:` is not emitted)
- Whole-project decompilation is not supported
- FOR/NEXT is sometimes generated incorrectly; use WHILE/ENDDO if needed

#### ILSPY Copyright

Copyright (c) 2011 AlphaSierraPapa for the SharpDevelop Team

Permission is hereby granted, free of charge, to any person obtaining a copy of this
software and associated documentation files (the "Software"), to deal in the Software
without restriction, including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
