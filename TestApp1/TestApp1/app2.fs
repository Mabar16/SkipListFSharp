module app2

open FsCheck
open System
open DataStructures.Lists
open FsCheck.Experimental


type Counter(?initial:int) =
    let mutable n = defaultArg initial 0
    member __.Inc() = 
        //silly bug
        if n <= 3  then n <- n + 1 else n <- n + 2
        n
    member __.Dec() = if n <= 0 then failwithf "Precondition fail" else n <- n - 1; n
    member __.Reset() = n <- 0
    override __.ToString() = sprintf "Counter = %i" n

let spec =
    let inc i = 
        { new Operation<Counter,int>() with
            member __.Run m = m + i
            member __.Check (c,m) = 
                let res = c.Inc() 
                m = res 
                |@ sprintf "Inc: model = %i, actual = %i" m res
            override __.ToString() = "inc"}
    let dec i = 
        { new Operation<Counter,int>() with
            member __.Run m = m - i
            override __.Pre m = 
                m > 0
            member __.Check (c,m) = 
                let res = c.Dec()
                m = res 
                |@ sprintf "Dec: model = %i, actual = %i" m res
            override __.ToString() = "dec"}
    let create initialValue = 
        { new Setup<Counter,int>() with
            member __.Actual() = new Counter(initialValue)
            member __.Model() = initialValue }
    { new Machine<Counter,int>() with
        member __.Setup = Gen.choose (0,3) |> Gen.map create |> Arb.fromGen
        member __.Next _ = Gen.choose (0,3) |> Gen.map(fun i -> inc i)}

[<EntryPoint>]
Check.Quick (StateMachine.toProperty spec)