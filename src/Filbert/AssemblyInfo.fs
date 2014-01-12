namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("Filbert")>]
[<assembly: AssemblyProductAttribute("Filbert")>]
[<assembly: AssemblyDescriptionAttribute("A BERT serializer and BERT-RPC client for .Net")>]
[<assembly: AssemblyVersionAttribute("0.2.0")>]
[<assembly: AssemblyFileVersionAttribute("0.2.0")>]
[<assembly: InternalsVisibleToAttribute("Filbert.Tests")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.2.0"
