namespace AssemblyInterpreter
open System
open System.IO
open Types
open Expressions

module Parser =
    let exprMap str (args:Operand array) : IExpr =
        let pred = fun (s) -> (fst s) = str
        let nakedExprArr = [| ("nop", Nop); ("ret", Ret) |]
        let singleExprArr = [| ("inc", Inc); ("dec", Dec); ("jmp", Jmp); ("call", Call); ("push", Push); ("pop", Pop); ("int", Int); ("je", Je); ("jne", Jne); ("jg", Jg); ("jge", Jge); |]
        let doubleExprArr = [| ("mov", Mov); ("cmp", Cmp); ("add", Add); ("sub", Sub); ("mul", Mul); ("div", Div) |]
        if args.Length = 0 then
            match Array.tryFindIndex pred nakedExprArr with
            | Some i -> snd nakedExprArr.[i] () :> IExpr
            | None -> failwith "Invalid expression"
        elif args.Length = 1 then
            match Array.tryFindIndex pred singleExprArr with
            | Some i -> ( (snd singleExprArr.[i]) args.[0]) :> IExpr
            | None -> failwith "Invalid expression"
        else
            match Array.tryFindIndex pred doubleExprArr with
            | Some i -> ( (snd doubleExprArr.[i]) args.[0] args.[1] ) :> IExpr
            | None -> failwith "Invalid expression"
              
    let getValtypeFromString (str:String) : ValType = 
        if Char.IsLetter str.[0] then
            failwith "Operand undefined" // prevent use of undeclared variables
        if Char.IsDigit str.[0] then 
            if str.Contains(".") then ValType.Number(Real(float(str)))
            else ValType.Number(Natural(int(str)))
        else Text(str.Trim('"').Replace("\\n", "\n" ))

    let TrimSplitSpace (str:String) = str.Trim(' ').Split([|' '|], StringSplitOptions.RemoveEmptyEntries)

    let parseData (strArr:String array) : Variable array = 
        let parse (str:String) : Variable = 
            let nameAndValue = TrimSplitSpace str
            new Variable(nameAndValue.[0], getValtypeFromString (String.Join(" ", Array.skip 1 nameAndValue)))
        Array.map parse strArr

    let parseCode (strArr:String array) (variableNames:(string*Variable) array) (funcs:(string*int) array) : IExpr array = 
        let parseOp opStr = 
            match Seq.tryFind (fun (varTpl:string*Variable) -> fst varTpl = opStr) variableNames with
                | Some varTpl -> Operand(OpType.Variable(snd varTpl))
                | None -> Operand(OpType.Value(Constant(getValtypeFromString opStr)))

        let parse (str:String) : IExpr =
            //Example string: mov x,5
            let strSplit = TrimSplitSpace str
            
            //Keep out empty lines
            if strSplit.Length = 0 || str.StartsWith "." || str.StartsWith "_" then exprMap "nop" [||] //Quick fix for labels and empty lines
            else

            //Example string: [mov] [x, 5]
            let op = Array.head strSplit
            let args = Array.tail strSplit
            if op.Contains "call" || op.Contains("j") then 
                let loc = funcs |> Array.find (fun tpl -> (fst tpl) = strSplit.[1]) |> snd |> Operand.FromIntAsNum
                exprMap op [|loc|]
            else

            //Example string: [mov] | [x] [5]
            if args.Length = 0 then exprMap op [||]
            elif args.Length = 1 then exprMap op [|parseOp args.[0]|]
            else exprMap op [|parseOp args.[0]; parseOp args.[1]|]
        
        Array.map parse strArr

    let parseFuncs (strArr:String array) (codeStartIndex:int) = 
        let parse (str:String) i : ((String*int) option) = 
            if not <| str.Contains("_") then None
            else
            let newStr = str.Trim(' ').ToCharArray() 
                         |> Array.skipWhile (fun c -> c <> '_') //remove everything up till char delim
                         |> Array.skip 1                        //skip delim as well
                         |> Array.takeWhile (fun c -> c <> ':') //take till next char delim
            Some (new String(newStr), i)
        strArr |> Array.mapi (fun i s -> parse s i) |> Array.choose id

    let parse (input:string[]) = 
        let isValidExpressionString (str:String) = String.IsNullOrWhiteSpace >> not <| str

        let sliceSections (str:String array) : int*string array*int*string array = 
            let dataStart = Array.tryFindIndex (fun s -> s = ".data") str
            let codeStart = Array.findIndex (fun s -> s = ".code") str
            let dataSection = match dataStart with
                              | Some(i) -> str |> Array.skip (i+1) |> Array.take (codeStart-i-1) |> Array.where isValidExpressionString
                              | None -> [||]
            
            let codeSection = str |> Array.skip (codeStart+1) |> Array.where isValidExpressionString
            (if Option.isSome dataStart then Option.get dataStart else 0),dataSection,codeStart,codeSection

        let _,dataSection,codeStart,codeSection = sliceSections input
        let variables = parseData dataSection
        let varTplArr = Array.map (fun (var:Variable) -> var.Name, var) variables 
        let funcs = parseFuncs codeSection codeStart
        let code = parseCode codeSection varTplArr funcs
        variables,code