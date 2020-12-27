### XSharp Language Plugin for ILSpy
In order to build, Create a Folder and clone this repository : You will have a ILSpy.XSharpLanguage.sln file and a ILSpy.XSharpLanguage Folder.
At that level, Create a ILSPY folder, and place there the ILSpy binaries.
After building, you will have a ILSpy.XSharpLanguage.Plugin.DLL in ILSpy.XSharpLanguage\bin\Debug or Release
Either run ILSpy in that folder, or copy/paste the file with the ILSpy binaries.

This version has been built against V5.x

This plugin is based on the source code found in ILSpy (https://github.com/icsharpcode/ILSpy) that provide CSharpLanguage.
It has been used as base, and some modifications haven been made to support some XSharp construction.

The generated code is following the XSharp Core Dialect.

Lots of things are still to be done, but most is working.

### Features
- LOCAL variables are all defined on top of statement blocks.
- Option page : set casing for Keywords

### Warnings
- Currently, Array access are all zero-based
- Only dot (.) is used as selector
- WholeProject decompilation is currently not supported
- FOR/NEXT is sometimes wrongly generated, and must be WHILE/ENDDO

### ILSPY Copyright
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

