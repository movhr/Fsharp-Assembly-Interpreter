open AssemblyInterpreter
open System.IO
open System

[<EntryPoint>]
let main args = 
    let program = [|
        ".data"
        "a 5"
        "b 10"
        ""
        ".code"
        "div a 2"    // = 5N / 2N -> 2N
        "sub b a"    // = 10N - 2N = 8N
        "push a"     // = SP[0] <- 2N
        "mov a 10.0" // = a <- 10R
        "pop b"      // = b <- SP[0] (2N)
        "inc b"      // = b <- 3 (2N + 1N)
        "div a b"    // = a <- 3.33R (10R / 3N)
        "push a"     // = SP[0] <- 3.33R
        "mov a 0"    // = a <- 0
        "mov b 0"    // = b <- 0
    |]
        

    let thething = new AssemblyInterpreter.Interpreter.Interpreter(File.ReadAllLines args.[0] |> Array.map (fun (s:String) -> s.Replace("\t","")) |> AssemblyInterpreter.Parser.parse)
    //let thething = new AssemblyInterpreter.Interpreter.Interpreter( program |> AssemblyInterpreter.Parser.parse)
    
    thething.Run()
    
    printf "<Stack>\n"
    thething.PrintStack()

    printf "<Symbols>\n"
    thething.PrintSymbolTable()
    0