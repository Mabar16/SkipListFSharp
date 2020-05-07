// Learn more about F# at http://fsharp.org
//#r "FsCheck"

open FsCheck
open System
open DataStructures.Lists
open FsCheck.Experimental
//module List

let rec matchList = fun (skipList:SkipList<int>) (fsList:List<int>) ->
    match fsList with
        |  [] -> false
        | a::rest -> skipList.Contains(a) && matchList skipList rest


let removeFromList = fun aList removeItem -> 
    let rec innerRemove = fun theList rItem head ->
        match theList with
            |anItem::rest  -> if anItem = rItem 
                                then List.rev(head)@rest 
                                else innerRemove rest rItem (anItem::head)
            |[] ->  List.rev(head)
    in innerRemove aList removeItem []

let isSkipListEqualToList = fun (skipList:SkipList<int>) (fsList:List<int>) ->
    
    for count in skipList do
        let valueA = skipList.Peek() in
        let valueB = List.head fsList in
        if not(valueA = valueB) then
            false
        else
            skipList.Remove(valueA)
        

   // in let result = false

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


let skipListSpec = 
    let add i = 
        { new Operation<SkipList<int>,List<int>>() with
            member __.Run m = i::m
            member __.Check (c, m) =
                c.Add(i)
                let res = isSkipListEqualToList c m
                   in res = true
                |@ sprintf "Dec: model = %i, actual = %i" m res
            override __.ToString() = "add"}
    let rem i = 
        { new Operation<SkipList<int>,List<int>>() with
            member __.Run m = i
            override __.Pre m = 
                (length m) > 0
            member __.Check (c,m) = 
                let res = c.Remove(i)
                m = res 
                |@ sprintf "Dec: model = %i, actual = %i" m res
            override __.ToString() = "rem"
            }
    let create = 
        { new Setup<SkipList<int>,List<int>>() with
            member __.Actual() = SkipList() : SkipList<int>
            member ___.Model() = [] }
    { new Machine<SkipList<int>, List<int>>() with 
        member __.Setup =  create |> Arb.fromGen
        member __.Next _ = Gen.choose(0,100) |> Gen.map [add i; rem i] }


[<EntryPoint>]
Check.Quick (StateMachine.toProperty spec)



let main argv =
    let theList = [1;2;3] in 
        Console.WriteLine theList;
    let myList = SkipList() : SkipList<int> in 
        myList.Add(5);
     
        let theresult = "HELLO"+myList.DeleteMin().ToString() + "WORLD" in
            Console.WriteLine theresult;
    0 // return an integer exit code


