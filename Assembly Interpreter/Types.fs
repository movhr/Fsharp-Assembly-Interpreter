namespace AssemblyInterpreter

module Types =
    open System
    open System.Collections.Generic

    
    //Types
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
                |_ -> failtype()
        override Number.ToString() = 
            match Number with
            |Natural(t') -> string(t')
            |Real(t') -> string(t')
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
                | Text(o), Text(o') -> o.CompareTo o'
                | Number(o), Number(o') -> (o :> IType<Number>).Cmp o'
                | Text(t), Number(n) -> ((if t.Contains(".") then Number.Real(float(t)) else Number.Natural(int(t))) :> IType<Number>).Cmp n
                | Number(n), Text(t) -> (n :> IType<Number>).Cmp (if t.Contains(".") then Number.Real(float(t)) else Number.Natural(int(t)))
        override ValType.ToString() = 
            match ValType with
            |Text(t') -> string(t')
            |Number(t') -> t'.ToString()
        member this._getInt() = 
            match this with | Text(_) -> failwith "NaN" | Number(n) -> match n with |Real(_) -> failwith "NaN" |Natural(n') -> n' 
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
    
    type Variable(name, initValue) = 
        let mutable value:ValType = initValue
        member this.Value
            with get() = value
            and set(newVal) = value <- newVal
        member this.Name:string = name
        
    type OpType = 
        | Variable of Variable
        | Value of Constant
        interface IType<OpType> with 
            override this.Cmp(obj:OpType) =
                match this, obj with
                    | Variable(o), Variable(o') -> (o.Value :> IType<ValType>).Cmp o'.Value
                    | Value(o), Value(o') -> (o.Data :> IType<ValType>).Cmp o'.Data
                    | _ -> failtype()
    
    type Operand(op) = 
        let mutable value:OpType = op
        member this.Value
            with get() = 
                match value with
                | Variable(v') -> v'.Value
                | Value(v') -> v'.Data
            and set(newVal) = 
                match value with
                | Variable(v') -> v'.Value <- newVal
                | Value(v') -> v'.Data <- newVal
        member this.Op with get() = value
        member this.Cmp(obj:Operand) = (this.Value :> IType<ValType>).Cmp obj.Value
        static member FromIntAsNum(a':int) = new Operand(OpType.Value(new Constant(ValType.Number(Number.Natural a'))))
        static member FromFltAsNum(a':float) = new Operand(OpType.Value(new Constant(ValType.Number(Number.Real a'))))
        static member FromStrAsTxt(a':string) = new Operand(OpType.Value(new Constant(ValType.Text(a'))))