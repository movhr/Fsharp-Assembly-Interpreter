namespace AssemblyInterpreter
open System
open System.IO
open Types
open Expressions

module Parser =
    //Type aliasing for easy referencing
    

    //Example program:
    //.data
    //  regA 1
    //  var1 5
    //  regB 0

    //.code
    //   _start:
    //      mov regB, var1
    //      div regB, regA
    //      

    //let parseSymbols : Variable list = 
        
    let exprMap str (args:Operand array) : IExpr =
        let pred = fun (s) -> (fst s) = str
        let nakedExprArr = [| ("nop", Nop); ("ret", Ret) |]
        let singleExprArr = [| ("inc", Inc); ("dec", Dec); ("jmp", Jmp); ("call", Call); ("push", Push); ("pop", Pop) |]
        let doubleExprArr = [| ("mov", Mov); ("add", Add); ("sub", Sub); ("mul", Mul); ("div", Div) |]
        if args.Length = 0 then
            match Array.tryFindIndex pred nakedExprArr with
            | Some i -> snd nakedExprArr.[i] () :> IExpr
            | None -> failwith "Invalid expression"
        elif args.Length = 1 then
            match Array.tryFindIndex pred singleExprArr with
            | Some i -> ( (snd singleExprArr.[i]) args.[0]) :> IExpr
            | None -> failwith "Invalid expression"
        elif args.Length = 2 then
            match Array.tryFindIndex pred doubleExprArr with
            | Some i -> ( (snd doubleExprArr.[i]) args.[0] args.[1] ) :> IExpr
            | None -> failwith "Invalid expression"
        else invalidArg "args" "Insufficient number of parameters passed"  
              
    let getValtypeFromString (str:String) : ValType = 
        if Char.IsLetter str.[0] then
            failwith "Operand undefined" // prevent use of undeclared variables
        if Char.IsDigit str.[0] then 
            if str.Contains(".") then ValType.Number(Real(float(str)))
            else ValType.Number(Natural(int(str)))
        else Text(str.Trim('"') )

    let TrimSplitSpace (str:String) = str.Trim(' ').Split([|' '|], StringSplitOptions.RemoveEmptyEntries)

    let parseData (strArr:String array) : Variable array = 
        let parse (str:String) : Variable = 
            let nameAndValue = TrimSplitSpace str
            new Variable(nameAndValue.[0], getValtypeFromString nameAndValue.[1])
        Array.map parse strArr

    let parseCode (strArr:String array) (variableNames:seq<string*Variable>) : IExpr array = 
        let parseOp opStr = 
            match Seq.tryFind (fun (varTpl:string*Variable) -> fst varTpl = opStr) variableNames with
                | Some varTpl -> Operand(OpType.Variable(snd varTpl))
                | None -> Operand(OpType.Value(Constant(getValtypeFromString opStr)))

        let parse (str:String) : IExpr =
            //Example string: mov x,5
            let strSplit = TrimSplitSpace str
            
            //Keep out empty lines
            if strSplit.Length = 0 || str.StartsWith "." then exprMap "nop" [||] //Quick fix for labels and empty lines
            else

            //Example string: [mov] [x, 5]
            let op = strSplit.[0]
            let args = Array.tail strSplit

            //Example string: [mov] | [x] [5]
            if args.Length = 0 then exprMap op [||]
            elif args.Length = 1 then exprMap op [|parseOp args.[0]|]
            else exprMap op [|parseOp args.[0]; parseOp args.[1]|]
        
        Array.map parse strArr

    let parse (input:string[]) = 
        let isValidExpressionString (str:String) = String.IsNullOrWhiteSpace >> not <| str

        let sliceSections (str:String array) : string array*string array = 
            let dataStart = Array.tryFindIndex (fun s -> s = ".data") str
            let codeStart = Array.findIndex (fun s -> s = ".code") str
            let dataSection = match dataStart with
                              | Some(i) -> str |> Array.skip (i+1) |> Array.take (codeStart-i-1) |> Array.where isValidExpressionString
                              | None -> [||]
            
            let codeSection = str |> Array.skip (codeStart+1) |> Array.where isValidExpressionString
            dataSection, codeSection

        let dataSection,codeSection = sliceSections input
        let variables = parseData dataSection
        let code = parseCode codeSection ( Array.map (fun (var:Variable) -> var.Name, var) variables )
        variables,code