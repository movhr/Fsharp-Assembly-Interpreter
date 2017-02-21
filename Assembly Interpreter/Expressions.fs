namespace AssemblyInterpreter
open Types
open Functions

module Expressions = 
    type IExpr = 
        abstract member Evaluate:unit->unit

    type NakedExpr(f:unit->unit) = 
        interface IExpr with
            override this.Evaluate() = f()

    type SingleExpr(a, f:Types.Operand->unit) =
        interface IExpr with
            override this.Evaluate() = f a
        
    type DoubleExpr(a, b, f:Operand->Operand->unit) = 
        interface IExpr with
            override this.Evaluate() = f a b

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
    
    //Double operand expressions
    let Mov l r = new DoubleExpr(l,r,mov)
    let Add l r = new DoubleExpr(l,r,add)
    let Sub l r = new DoubleExpr(l,r,sub)
    let Mul l r = new DoubleExpr(l,r,mul)
    let Div l r = new DoubleExpr(l,r,div)
