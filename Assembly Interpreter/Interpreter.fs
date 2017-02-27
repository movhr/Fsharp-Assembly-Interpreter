﻿namespace AssemblyInterpreter
open Types
open Expressions
open Functions

module Interpreter =
    type Interpreter(symbolList:Variable array, codeSegment:IExpr array) = 
        member this.SymbolTable = Map<string,Variable>( Seq.ofArray symbolList |> Seq.map (fun s -> s.Name, s) )
        member this.Stack = Functions.stack
        member this.IP = Functions.IP
        member this.Code = codeSegment
        member this.Run() = codeSegment |> Array.iteri (fun i e -> e.Evaluate())
        member this.PrintStack() = Seq.iteri (fun i e -> printf "SP:[%i] = %s\n" i (e.ToString()) ) this.Stack
        member this.PrintSymbolTable() = Map.iter (fun k (v:Variable) -> printf "DS:[%s] = %A\n" k (v.Value) ) this.SymbolTable