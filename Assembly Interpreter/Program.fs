open System
open System.Collections.Generic

//Types
let failcmp() = failwith "Argument cannot be compared to this object."
let failtype() = invalidOp "Types are not compatible"

type Flag = bool

type Flags() = 
    let mutable zf:Flag = false
    let mutable sf:Flag = false
    member this.ZF with get() = zf and set(value) = zf <- value //zero flag
    member this.SF with get() = sf and set(value) = sf <- value //sign flag

type IType<'T> = 
    abstract member Cmp:'T -> int

type Number =
    |Natural of int
    |Real of float
    interface IType<Number> with
        override this.Cmp(obj:Number) =   
            match this,obj with
            |Natural(o),Natural(o') -> o.CompareTo o'
            |Real(o),Real(o') -> o.CompareTo o'
            |_ -> failcmp()
    override this.ToString() = 
        match this with
        |Natural(t') -> t'.ToString()
        |Real(t') -> t'.ToString()
    static member (*) (a:Number,b:Number) : Number = 
        match a,b with
        | Natural a', Natural b' -> Natural (a' * b')
        | Natural a', Real b' -> Real ( (float a') * b' )
        | Real a', Natural b' -> Real ( a' * (float b') )
        | Real a', Real b' -> Real ( a' * b' )    
    static member (/) (a:Number,b:Number) : Number = 
        match a,b with
        | Natural a', Natural b' -> Natural (a' / b')
        | Natural a', Real b' -> Real ( (float a') / b' )
        | Real a', Natural b' -> Real ( a' / (float b') )
        | Real a', Real b' -> Real ( a' / b' )
    static member (+) (a:Number,b:Number) : Number = 
        match a,b with
        | Natural a', Natural b' -> Natural (a' + b')
        | Natural a', Real b' -> Real ( (float a') + b' )
        | Real a', Natural b' -> Real ( a' + (float b') )
        | Real a', Real b' -> Real ( a' + b' )
    static member (-) (a:Number,b:Number) : Number = 
        match a,b with
        | Natural a', Natural b' -> Natural (a' - b')
        | Natural a', Real b' -> Real ( (float a') - b' )
        | Real a', Natural b' -> Real ( a' - (float b') )
        | Real a', Real b' -> Real ( a' - b' )

    
type ValType = 
    |Text of string
    |Number of Number
    interface IType<ValType> with
        override this.Cmp(obj:ValType) = 
            match this,obj with
            | Text(o), Text(o') -> o.CompareTo this
            | Number(o), Number(o') -> (o :> IType<Number>).Cmp o'
            | _ -> failcmp()
    override this.ToString() = 
        match this with
        |Text(t') -> t'
        |Number(t') -> t'.ToString()
    static member (+) (a:ValType, b:ValType) : ValType = 
        match a,b with
        | Number(a'), Number(b') -> ValType.Number( a' + b' )
        | Text(a'), Text(b') -> ValType.Text ( String.Concat(a', b') )
        | _ -> failtype()    
    static member (-) (a:ValType, b:ValType) : ValType = 
        match a,b with
        | Number(a'), Number(b') -> ValType.Number( a' - b' )
        | _ -> failtype()
    static member (*) (a:ValType, b:ValType) : ValType = 
        match a,b with
        | Number(a'), Number(b') -> ValType.Number( a' * b' )
        | _ -> failtype()
    static member (/) (a:ValType, b:ValType) : ValType = 
        match a,b with
        | Number(a'), Number(b') -> ValType.Number( a' / b' )
        | _ -> failtype()

type Constant(value) = 
    let mutable value:ValType = value
    member this.Data
        with get() = value
        and set(newVal) = value <- newVal
    static member FromInt(a':int) = ValType.Number(Number.Natural a')
    static member FromFlt(a':float) = ValType.Number(Number.Real a')

type Register(initValue) = 
    let mutable value:ValType = initValue
    member this.Value
        with get() = value
        and set(newVal) = value <- newVal
    //member this.Cmp(obj:Register) = (value.Data :> IType<ValType>).Cmp obj.Value.Data

type Variable(name, initValue) = 
    inherit Register(initValue) with
    member this.Name:string = name
    
type OpType = 
    | Variable of Variable
    | Register of Register
    | Value of Constant
    interface IType<OpType> with 
        override this.Cmp(obj:OpType) =
            match this, obj with
                | Register(o), Register(o') -> (o.Value :> IType<ValType>).Cmp o'.Value
                | Value(o), Value(o') -> (o.Data :> IType<ValType>).Cmp o'.Data
                | _ -> failtype()

type Operand(op) = 
    let mutable value:OpType = op
    member this.Value
        with get() = 
            match value with
            | Variable(v') -> v'.Value
            | Register(r') -> r'.Value
            | Value(v') -> v'.Data
        and set(newVal) = 
            match value with
            | Variable(v') -> v'.Value <- newVal
            | Register(v') -> v'.Value <- newVal
            | Value(v') -> v'.Data <- newVal
    member this.Op with get() = value
    member this.Cmp(obj:Operand) = (this.Value :> IType<ValType>).Cmp(obj.Value)
    static member FromIntAsNum(a':int) = new Operand(OpType.Value(new Constant(ValType.Number(Number.Natural a'))))
    static member FromFltAsNum(a':float) = new Operand(OpType.Value(new Constant(ValType.Number(Number.Real a'))))


//Control flow
let flags = new Flags()
let mutable IP:Register = new Register ( -1 |> Number.Natural |> ValType.Number)
let mutable SP:int8 = -1y
let stack:List<ValType> = new List<ValType>(64)

//Valid MOV commands
//mov <reg>,<reg>
//mov <reg>,<mem>
//mov <mem>,<reg>
//mov <reg>,<const>
//mov <mem>,<const>

let mov(left, right) =
    match left, right with
    | Register(l'), Register(r') -> l'.Value <- r'.Value
    | Register(l'), Variable(r') -> l'.Value <- r'.Value
    | Variable(l'), Register(r') -> l'.Value <- r'.Value
    | Register(l'), Value(r') -> l'.Value <- r'.Data
    | Variable(l'), Value(r') -> l'.Value <- r'.Data
    | _ -> failtype()

let cmp(left:Operand,right:Operand) = 
    let result = left.Cmp right
    flags.ZF <- (result = 0)
    flags.SF <- (result > 0)

let push(left:Operand) = 
    stack.Add left.Value
    SP <- SP + 1y

let pop(left:Operand) = 
    left.Value <- stack.[int SP]
    stack.RemoveAt (int SP)
    SP <- SP - 1y
    
let jmp(loc) = 
    IP.Value <- loc

let call(loc) = 
    push(Operand(OpType.Register IP) )
    jmp(loc)

let ret() = pop(Operand(OpType.Register IP) )

let nop() = ()

let cj(loc, c:bool) = if c then jmp(loc)
let je(loc)  = cj(loc, flags.ZF)
let jz(loc)  = cj(loc, flags.ZF)
let jnz(loc) = cj(loc, not flags.ZF)
let js(loc)  = cj(loc, flags.SF)
let jns(loc)  = cj(loc, not flags.SF)
let jne(loc) = cj(loc, not flags.ZF)
let jg(loc)  = cj(loc, not flags.SF && not flags.ZF)
let jge(loc) = cj(loc, not flags.SF || flags.ZF)
//  jl(e) comes with Overflow flag, which not implemented yet

let AddSub<'T>(l:Operand, r:Operand, f:Number->Number->ValType) =
    let l',r' = match l.Value,r.Value with | Number(l''),Number(r'') -> (l'',r'') | _ -> invalidOp ""
    match l.Op,r.Op with
    | Register(l''), Register(_) -> l''.Value <- f l' r'
    | Register(l''), Value(_)    -> l''.Value <- f l' r'
    | Register(l''), Variable(_) -> l''.Value <- f l' r'
    | Variable(l''), Register(_) -> l''.Value <- f l' r'
    | Variable(l''), Value(_)    -> l''.Value <- f l' r'
    | _ -> failtype()

let add(l,r) = AddSub(l, r, (fun a b -> Number(a+b) ) )
let sub(l,r) =  AddSub(l, r, (fun a b -> Number(a-b) ) )
let inc(l:Operand) = add (l, Operand.FromIntAsNum 1)
let dec(l:Operand) = sub (l, Operand.FromIntAsNum 1)
let mul(l:Register,r:Operand) =
    if r.GetType() = typedefof<Constant> then failtype()
    else l.Value <- l.Value * r.Value
//let div2(l:Register,r:Operand) = //Custom division function. Original assembly uses EAX,EBX,EDX, which is too much of a hassle
    
[<EntryPoint>]
let main argv = 
    let mutable regA = new Register(Constant.FromInt 0)
    let mutable regB = new Register(Constant.FromInt 1)
    let mutable var1 = new Variable("var1", Constant.FromInt 5)
    let mutable var2 = new Variable("var2", Constant.FromInt 10)
    
    mul(regB, Operand(OpType.Variable var2)) //expects 1 * 10 = 10
    printfn "%A" regB.Value
        
    printfn "%A" regB.Value
    0