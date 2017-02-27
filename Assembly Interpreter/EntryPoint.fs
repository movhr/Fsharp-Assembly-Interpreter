open AssemblyInterpreter
open System.IO
open System

[<EntryPoint>]
let main args = 
    let thething = new AssemblyInterpreter.Interpreter.Interpreter(File.ReadAllLines args.[0] |> Array.map (fun (s:String) -> s.Replace("\t","")) |> AssemblyInterpreter.Parser.parse)
    
    thething.Run()

    printf "<Stack>\n"
    thething.PrintStack()

    printf "<Symbols>\n"
    thething.PrintSymbolTable()

    failwith "Dirty exit."