// Learn more about F# at http://fsharp.org
//#r "FsCheck"

open FsCheck
open System
open DataStructures.Lists
open FsCheck.Experimental
//module List


let removeFromList = fun aList removeItem -> 
    let rec innerRemove = fun theList rItem head ->
        match theList with
            |anItem::rest  -> if anItem = rItem 
                                then List.rev(head)@rest 
                                else innerRemove rest rItem (anItem::head)
            |[] ->  List.rev(head)
    in innerRemove aList removeItem []

let isSkipListEqualToList = fun (skipList:SkipList<int>) (fsList:List<int>) ->
    skipList.Count = fsList.Length 


type Counter(?initial:int) =
    let mutable n = defaultArg initial 0
    member __.Inc() = 
        //silly bug
        if n <= 3  then n <- n + 1 else n <- n + 2
        n
    member __.Dec() = if n <= 0 then failwithf "Precondition fail" else n <- n - 1; n
    member __.Reset() = n <- 0
    override __.ToString() = sprintf "Counter = %i" n


//let spec =
//    let inc = 
//        { new Operation<Counter,int>() with
//            member __.Run m = m + 1
//            member __.Check (c,m) = 
//                let res = c.Inc() 
//                m = res 
//                |@ sprintf "Inc: model = %i, actual = %i" m res
//            override __.ToString() = "inc"}
//    let dec = 
//        { new Operation<Counter,int>() with
//            member __.Run m = m - 1
//            override __.Pre m = 
//                m > 0
//            member __.Check (c,m) = 
//                let res = c.Dec()
//                m = res 
//                |@ sprintf "Dec: model = %i, actual = %i" m res
//            override __.ToString() = "dec"}
//    let create initialValue = 
//        { new Setup<Counter,int>() with
//            member __.Actual() = new Counter(initialValue)
//            member __.Model() = initialValue }
//    { new Machine<Counter,int>() with
//        member __.Setup = Gen.choose (0,3) |> Gen.map create |> Arb.fromGen
//        member __.Next _ = Gen.elements [ inc; dec ] }


let skipListSpec = 
    let add i = 
        { new Operation<SkipList<int>,List<int>>() with
            member __.Run m = i::m
            member __.Check (c, m) =
                c.Add(i)
                let res = isSkipListEqualToList c m
                   in res = true
                |@ sprintf "Dec: model = %i, actual = %i" m.Length c.Count
            override __.ToString() = sprintf "add %i" i }
    let rem i = 
        { new Operation<SkipList<int>,List<int>>() with
            member __.Run m = removeFromList m i
            override __.Pre m = 
                (m.Length) > 0
            member __.Check (c,m) = 
                c.Remove(i)
                let res =isSkipListEqualToList c m 
                in res = true
                |@ sprintf "Dec: model = %i, actual = %i" m.Length c.Count
            override __.ToString() = sprintf "rem %i" i 
            }
    let clear = 
        { new Operation<SkipList<int>, List<int>>() with
            member __.Run m = []
            member __.Check (c,m) = 
                c.Clear()
                let res = isSkipListEqualToList c m
                in res = true
                |@ sprintf "Dec: model = %i, actual%i" m.Length c.Count
            override __.ToString() = sprintf "clear"
            }
    let create = 
        { new Setup<SkipList<int>,List<int>>() with
            member __.Actual() = SkipList() : SkipList<int>
            member ___.Model() = [] }
    { new Machine<SkipList<int>, List<int>>() with 
        member __.Setup =  Gen.constant create |> Arb.fromGen
        member __.Next _ =  Gen.oneof[ Gen.choose(0,100) |> Gen.map2 (fun op i -> op i) (Gen.elements [add;rem;]; Gen.elements {clear}]}


Check.Quick (StateMachine.toProperty skipListSpec)


[<EntryPoint>]
let main argv =
    let theList = [1;2;3] in 
        Console.WriteLine theList;
    let myList = SkipList() : SkipList<int> in 
        myList.Add(15);
        myList.Remove(15);
     
        let theresult = "COUNT : " + myList.Count.ToString() in
            Console.WriteLine theresult;
    0 // return an integer exit code


