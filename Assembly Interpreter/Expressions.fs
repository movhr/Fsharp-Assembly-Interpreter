namespace AssemblyInterpreter
open Types
open Functions
open System.Reflection

module Expressions = 
    type IExpr = 
        abstract member Evaluate:unit->unit

    type NakedExpr(f:unit->unit) = 
        interface IExpr with
            override this.Evaluate() = f()
        override NakedExpr.ToString() = 
            f.GetType().Name

    type SingleExpr(a:Operand, f:Operand->unit) =
        interface IExpr with
            override this.Evaluate() = f a
        override SingleExpr.ToString() = 
            f.GetType().Name + " [" + (a.Value.ToString()) + "]"
                
    type DoubleExpr(a:Operand, b:Operand, f:Operand->Operand->unit) = 
        interface IExpr with
            override this.Evaluate() = f a b
        override SingleExpr.ToString() = 
            f.GetType().Name + " [" + (a.Value.ToString()) + ", " + (b.Value.ToString()) + "]"
        
    type InterruptTable() = 
        static let mutable reservedOp = Operand.FromIntAsNum 0

        static member Read() = //82d = ascii R
            let input = System.Console.ReadLine()
            reservedOp <- Operand.FromStrAsTxt input
            push reservedOp

        static member Print() = //80d = ascii P
            pop reservedOp 
            System.Console.Write(reservedOp.Value.ToString())

        static member Exit() =  //69d = ascii E
            pop reservedOp 
            printf "\nExit code: %i\n" (reservedOp.Value._getInt())
            exit (reservedOp.Cmp(Operand.FromIntAsNum 0))

        static member Call(num:Operand) = 
            let errIntCode() = invalidArg "num" "Invalid interrupt code"
            match num.Value._getInt() with
            | 82 -> InterruptTable.Read()
            | 80 -> InterruptTable.Print()
            | 69 -> InterruptTable.Exit()
            |  _ -> errIntCode()

    //Naked expressions
    let Ret() = new NakedExpr(ret)
    let Nop() = new NakedExpr(nop)

    //Single operand expressions
    let Inc l  = new SingleExpr(l, inc)
    let Dec l  = new SingleExpr(l, dec)
    let Jmp l  = new SingleExpr(l, jmp)
    let Call l = new SingleExpr(l, call)
    let Push l = new SingleExpr(l, push)
    let Pop l  = new SingleExpr(l, pop)
    let Int l  = new SingleExpr(l, InterruptTable.Call)
    
    //Double operand expressions
    let Mov l r = new DoubleExpr(l,r,mov)
    let Add l r = new DoubleExpr(l,r,add)
    let Sub l r = new DoubleExpr(l,r,sub)
    let Mul l r = new DoubleExpr(l,r,mul)
    let Div l r = new DoubleExpr(l,r,div)

    //Control flow
    let Je l = new SingleExpr(l,je)
    let Jg l = new SingleExpr(l,jg)
    let Jne l = new SingleExpr(l,jne)
    let Jge l = new SingleExpr(l,jge)

    let Cmp l r = new DoubleExpr(l, r, cmp)