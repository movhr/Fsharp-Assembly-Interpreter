namespace AssemblyInterpreter
open Types
open Expressions
open Parser
open System.IO

module Interpreter =
    type Interpreter(symbolList:Variable array, codeSegment:IExpr array) = 
        member this.SymbolTable = Map<string,Variable>( Seq.ofArray symbolList |> Seq.map (fun s -> s.Name, s) )
        member this.Stack = Functions.stack
        member this.Run() = codeSegment |> Array.iteri (fun i e -> try e.Evaluate() with |_-> failwithf "Error on line %A" i)
        member this.PrintStack() = Seq.iteri (fun i e -> printf "SP:[%i] = %s\n" i (e.ToString()) ) this.Stack
        member this.PrintSymbolTable() = Map.iter (fun k (v:Variable) -> printf "DS:[%s] = %A\n" k (v.Value) ) this.SymbolTable