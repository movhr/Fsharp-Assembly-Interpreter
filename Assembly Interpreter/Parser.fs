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
        
    let exprMap str args =
        let pred = fun (s:string*'T->IExpr) -> (fst s) = str
        let nakedExprArr = [| ("nop", Nop); ("ret", Ret) |]
        let singleExprArr = [| ("inc", Inc); ("dec", Dec); ("jmp", Jmp); ("call", Call); ("push", Push); ("pop", Pop) |]
        let doubleExprArr = [| ("mov", Mov); ("add", Add) ("sub", Sub) ("mul", Mul) ("div", Div) |]
        match Array.tryFindIndex pred nakedExprArr with
        | Some i -> nakedExprArr.[i]
        | None ->
            match 
            
                    

    let executeProgram (expressions:IExpr list) = List.iter (fun (e:IExpr) -> e.Evaluate()) expressions
    
    let parseData (strArr:String array) : Variable array = 
        let parse (str:String) : Variable = 
            let nameAndValue = str.Trim(' ').Split(' ')
            let value = 
                if Char.IsDigit nameAndValue.[1].[0] then 
                    if nameAndValue.[1].Contains(".") then ValType.Number(Real(float(nameAndValue.[1])))
                    else ValType.Number(Natural(int(nameAndValue.[1])))
                else Text(nameAndValue.[1].Trim('"') )
            new Variable(nameAndValue.[0], value)
        Array.map (fun (s:String) -> parse(s)) strArr

    let parseCode (strArr:String array) : IExpr list = 
        let rec parse (str:String) : IExpr*Operand list = 
            let strSplit = str.Trim(' ').Split(' ')
            let expr:IExpr = exprMap.[strSplit.[0]]
            if expr :? NakedExpr then expr,[]
            else
                let firstOp = 
                    let operand1:String = strSplit.[1]
                    if not (operand1.StartsWith("reg")) then
                        Operand(OpType.Variable())
                

        Array.map (fun (s:String) -> parse s) strArr

    let parse (input:string[]) = 
        let findStr pattern (input:String) = try Array.findIndex input.Contains pattern with | _ -> -1

        let sliceSections (str:String array) : string array*string array = 
            let dataStart = findStr str ".data"
            let codeStart = findStr str ".code"
            let dataSection = if dataStart > 0 then Array.take codeStart str else []
            let codeSection = if codeStart > 0 then Array.skip codeStart str else failwith "A code section must be present in the file"
            dataSection, codeSection

        let data,code = sliceSections input
        parseData data, parseCode code

    [<EntryPoint>]
    let main argv = 
        if argv.Length = 0 then failwith "Please specify the path to the file to be interpreted."
        else
            let fileDir = argv.[0]
            let fileContents = File.ReadAllLines fileDir
            //let program = evaluate fileContents []
            //executeProgram ( List.rev program )
            0

