namespace AssemblyInterpreter
open Types
open Expressions
open Parsing

module Tests = 
    let testProgram = 
        let testExpressions = [
            Reg(Constant.FromInt 3)
            Reg(Constant.FromInt 8)

        ]
        executeProgram testExpressions