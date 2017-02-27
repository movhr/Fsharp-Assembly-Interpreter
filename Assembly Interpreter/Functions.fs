﻿namespace AssemblyInterpreter
open System.Collections.Generic

open Types

module Functions = 
    //Control flow
    let flags = new Flags()
    let mutable IP:Variable = new Variable ("IP", -1 |> Number.Natural |> ValType.Number)
    let mutable SP:int8 = -1y
    let stack:List<ValType> = new List<ValType>(64)
    
    let mov (left:Operand) (right:Operand) =
        match left.Op,right.Op with
        | Variable(l'), Variable(r') -> l'.Value <- r'.Value
        | Variable(l'), Value(r') -> l'.Value <- r'.Data
        | _ -> failtype()
    
    let cmp (left:Operand) (right:Operand) = 
        let result = left.Cmp right
        flags.ZF <- (result = 0)
        flags.SF <- (result > 0)
    
    let push (left:Operand) = 
        stack.Add left.Value
        SP <- SP + 1y
    
    let pop(left:Operand) = 
        left.Value <- stack.[int SP]
        stack.RemoveAt (int SP)
        SP <- SP - 1y
        
    let jmp(loc:Operand) = 
        IP.Value <- loc.Value
    
    let call(loc) = 
        push(Operand(OpType.Variable IP) )
        jmp(loc)
    
    let ret() = pop(Operand(OpType.Variable IP) )
    
    let nop() = ()
    
    let cj loc (c:bool) = if c then jmp loc
    let je loc  = cj loc flags.ZF
    let jz loc  = cj loc flags.ZF
    let jnz loc = cj loc (not flags.ZF)
    let js loc  = cj loc flags.SF
    let jns loc = cj loc (not flags.SF)
    let jne loc = cj loc (not flags.ZF)
    let jg loc  = cj loc (not flags.SF && not flags.ZF)
    let jge loc = cj loc (not flags.SF || flags.ZF)
    //  jl(e) comes with Overflow flag, which not implemented yet
    
    
    //Arithmetic operations
    let AddSub<'T>(l:Operand) (r:ValType) (f:Number->Number->ValType) =
        let l',r' = match l.Value,r with | Number(l''),Number(r'') -> (l'',r'') | _ -> invalidOp ""
        match l.Op with
        | Variable(l'') -> l''.Value <- f l' r'
        | _ -> failtype()
    
    let add (l:Operand) (r:Operand) = AddSub l r.Value (fun a b -> Number(a+b) )
    let sub (l:Operand) (r:Operand) = AddSub l r.Value (fun a b -> Number(a-b) )
    let mul (l:Operand) (r:Operand) = l.Value <- l.Value * r.Value
    let div (l:Operand) (r:Operand) = l.Value <- l.Value / r.Value //Custom division function: original assembly uses EAX,EBX,EDX, which is too much of a hassle
    
    let inc (l:Operand) = add l (Operand.FromIntAsNum 1)
    let dec (l:Operand) = sub l (Operand.FromIntAsNum 1)