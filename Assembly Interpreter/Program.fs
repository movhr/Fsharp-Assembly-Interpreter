open System
open System.Collections.Generic

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let failcast() = failwith "Argument cannot be compared to this object."

type Flag = bool

type Flags() = 
    let mutable zf:Flag = false
    let mutable sf:Flag = false
    member this.ZF with get() = zf and set(value) = zf <- value //zero flag
    member this.SF with get() = sf and set(value) = sf <- value //sign flag

type Number =
    |Natural of int
    |Real of float
    member this.Cmp(obj:Number) =   
        match this,obj with
        |Natural(o),Natural(o') -> o.CompareTo o'
        |Real(o),Real(o') -> o.CompareTo o'
        |_ -> failcast()
    
type Value = 
    |Text of string
    |Number of Number
    member this.Cmp(obj:Value) = 
        match this,obj with
        | Text(o), Text(o') -> o.CompareTo this
        | Number(o), Number(o') -> o.Cmp o'
        | _ -> failcast()

type Register(name, initValue) = 
    let mutable value:Value = initValue
    member this.Name:string = name
    member this.Value
        with get() = value
        and set(newVal) = value <- newVal
    member this.Cmp(obj:Register) = this.Value.Cmp obj.Value

type Variable = Register

type Operand = 
    | Variable of Variable
    | Register of Register
    | Value of Value
    member this.Cmp(obj:Operand) =
        match this, obj with
            | Register(o), Register(o') -> o.Cmp o'
            | Value(o), Value(o') -> o.Cmp o'
            | _ -> failcast()
    member this.GetValue() = 
        match this with
        |Variable(t') | Register(t') -> t'.Value
        |Value(t') -> t'

let NumToOp(a':Number) = Operand.Value(Value.Number(a'))
let IntToOp(a': int) = Operand.Value(Value.Number(Number.Natural(a')))
let FloatToOp(a':float) = Operand.Value(Value.Number(Number.Real(a')))

[<AbstractClassAttribute>]
type Instruction(name) = 
    member this.Name:string = name
    abstract member Evaluate:unit -> unit

[<AbstractClass>]
type NakedInstruction(name) = 
    inherit Instruction(name)
    
[<AbstractClass>]
type SingleInstruction(name, m) = 
    inherit Instruction(name)
    let mutable leftOperand:Operand = m
    member this.LeftOperand 
        with get() = leftOperand
        and set(value) = leftOperand <- value
    
[<AbstractClass>]
type DoubleInstruction(name, m, n) =
    inherit SingleInstruction(name, m)
    member this.RightOperand:Operand = n

type CalcDouble(name, left:Number, right:Number, f1:int -> int -> int, f2: float -> float -> float) =
    inherit DoubleInstruction(name, NumToOp left, NumToOp right) with
    override this.Evaluate() = 
        match left, right with
        | Real (r'), Real(r'') -> this.LeftOperand <- FloatToOp (f2 r' r'')
        | Natural (n'), Natural(n'') -> this.LeftOperand <- IntToOp (f1 n' n'')
        | _ -> failwith "Invalid Operation"

type Add(left, right) =
    inherit CalcDouble( "add", left, right, (fun n' n''-> (n' + n'')), (fun r' r'' -> (r' + r'')))
                                                                      
type Sub(left, right) =                                               
    inherit CalcDouble( "sub", left, right, (fun n' n''-> (n' - n'')), (fun r' r'' -> (r' - r'')))
                                                                     
type Mul(left, right) =                                               
    inherit CalcDouble( "mul", left, right, (fun n' n''-> (n' * n'')), (fun r' r'' -> (r' * r'')))
                                                                      
type Div(left, right) =                                               
    inherit CalcDouble( "div", left, right, (fun n' n''-> (n' / n'')), (fun r' r'' -> (r' / r'')))      

type CalcSingle(name, left, f1:int -> int, f2:float -> float) = 
    inherit SingleInstruction("inc", NumToOp left) with
    override this.Evaluate() = 
        match left with 
        |Natural(left') -> this.LeftOperand <- IntToOp (f1 left')
        |Real(left'') -> this.LeftOperand <- FloatToOp (f2 left'')
        ()

type Inc(left) = 
    inherit CalcSingle("inc", left, (fun n' -> n'+1), (fun r' -> r'+1.0) )

type Dec(left) = 
    inherit CalcSingle("inc", left, (fun n' -> n'-1), (fun r' -> r'-1.0) )

//Valid commands
//mov <reg>,<reg>
//mov <reg>,<mem>
//mov <mem>,<reg>
//mov <reg>,<const>
//mov <mem>,<const>
type Mov(left, right) = 
    inherit DoubleInstruction("mov", left, right) with
    override this.Evaluate() = 
        match this.LeftOperand, this.RightOperand with
        | Register(l'), Register(r') | Register(l'), Variable(r') | Variable(l'), Register(r') -> l'.Value <- r'.Value
        | Register(l'), Value(r') | Variable(l'), Value(r') -> l'.Value <- r'
        | _ -> invalidOp "Invalid operation"
     
type Function(name, body) = 
    member this.Name:string = name
    member this.Body:Instruction array = body

let runFunction(a':Instruction[]) = Array.iter (fun (ins:Instruction) -> ins.Evaluate()) a'

let flags = new Flags()
type Cmp(left,right) = 
    inherit DoubleInstruction("cmp", left, right) with
        override this.Evaluate() = 
            let result = left.Cmp right
            flags.ZF <- (result = 0)
            flags.SF <- (result > 0)

let mutable IP:Value = Value.Number(Number.Natural(0)) 
let mutable SP:int8 = -1y
let stack:List<Value> = new List<Value>(64)
type Push(left) = 
    inherit SingleInstruction("push", left) with
    override this.Evaluate() = 
        stack.Add (this.LeftOperand.GetValue())
        SP <- SP + 1y

type Pop(left) = 
    inherit SingleInstruction("pop", left) with
    override this.Evaluate() = 
        let t = new Mov(left, Operand.Value stack.[int SP])
        t.Evaluate()
        SP <- SP - 1y

type Call(``function``) =
    inherit Instruction("call") with
    member this.Label:Function = ``function``
    override this.Evaluate() = 
        let saveIP = new Push(Operand.Value IP)
        saveIP.Evaluate()
        runFunction this.Label.Body

type Jmp(``function``) =
    inherit Instruction("jmp") with
    member this.Label:Function = ``function``
    override this.Evaluate() = runFunction this.Label.Body

type Ret() =
    inherit NakedInstruction("ret") with
    override this.Evaluate() = 
        let setbackIP = new Pop(Operand.Value IP)
        setbackIP.Evaluate()


[<EntryPoint>]
let main argv = 
    let one = new Inc(Number.Natural 1)
    one.Evaluate()
    printfn "%A" one.LeftOperand
    printfn "%A" argv
    0 // return an integer exit code
