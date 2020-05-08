// Learn more about F# at http://fsharp.org
//#r "FsCheck"

open FsCheck
open System
open DataStructures.Lists
open FsCheck.Experimental
//module List

let matchList2 = fun (skipList:SkipList<int>) (fsList:List<int>) ->
    skipList.Count = fsList.Length && 
        let rec matchList = fun (skipList2:SkipList<int>) (fsList2:List<int>) ->
            match fsList2 with
            |  [] -> true
            | a::rest -> skipList2.Contains(a) && matchList skipList2 rest 
        in matchList skipList fsList

let inList = fun (fsList:List<int>) i -> 
    let rec isInList = fun (fsList2:List<int>) i -> 
        match fsList2 with 
        | [] -> false
        | a::rest -> a = i || isInList rest i
    in isInList fsList i

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
        

let skipListSpec = 
    let add i = 
        { new Operation<SkipList<int>,List<int>>() with
            member __.Run m = i::m
            member __.Check (c, m) =
                c.Add(i)
                let res = matchList2 c m//c.Contains(i)
                   in res = true
                |@ sprintf "Add: model = %i, actual = %i CONTENT model = %A, actual = %A" m.Length c.Count m c
            override __.ToString() = sprintf "add %i" i }
    let rem i = 
        { new Operation<SkipList<int>,List<int>>() with
            member __.Run m = removeFromList m i
            override __.Pre m = 
                (m.Length) > 0
            member __.Check (c,m) = 
                c.Remove(i)
                let res = matchList2 c m 
                in res = true
                |@ sprintf "Remove: model = %i, actual = %i" m.Length c.Count
            override __.ToString() = sprintf "rem %i" i 
            }
    let clear = 
        { new Operation<SkipList<int>, List<int>>() with
            member __.Run m = []
            member __.Check (c,m) = 
                c.Clear()
                let res = matchList2 c m && c.IsEmpty
                in res = true
                |@ sprintf "Clear: model = %i, actual = %i CONTENT model = %A, actual = %s" m.Length c.Count m (c.ToString())
            override __.ToString() = sprintf "clear"
            }
    let count = 
        { new Operation<SkipList<int>, List<int>>() with
            member __.Run m = m
            member __.Check (c, m) =
                let res = c.Count = m.Length
                in res = true
                |@ sprintf "Count: model = %i, actual = %i" m.Length c.Count
            override __.ToString() = sprintf "count"
        }
    let isEmpty = 
        { new Operation<SkipList<int>, List<int>>() with
            member __.Run m = m
            member __.Check (c, m) =
                let res = (c.IsEmpty = (m.Length = 0))
                in res = true
                |@ sprintf "IsEmpty: model = %b, actual = %b" (m.Length = 0) c.IsEmpty
             override __.ToString() = sprintf "isEmpty"
        }
    let contains i =
        { new Operation<SkipList<int>, List<int>>() with
            member __.Run m = m
            member __.Check(c, m) =
                let res = (inList m i) = (c.Contains(i))
                in res = true
                |@ sprintf "Contains %i: model = %b, actual %b" i (inList m i) (c.Contains(i))
            override __.ToString() = sprintf "contains"
        }
    let create = 
        { new Setup<SkipList<int>,List<int>>() with
            member __.Actual() = SkipList() : SkipList<int>
            member ___.Model() = [] }
    { new Machine<SkipList<int>, List<int>>() with 
        member __.Setup =  Gen.constant create |> Arb.fromGen
        member __.Next _ =  Gen.oneof[ Gen.elements [count;isEmpty]; Gen.choose(0,100) |> Gen.map2 (fun op i -> op i) (Gen.elements [add;rem;contains]); Gen.elements [clear]]}


[<EntryPoint>]
Check.Quick (StateMachine.toProperty skipListSpec)



let main argv =
    let theList = [1;2;3] in 
        Console.WriteLine theList;
    let myList = SkipList() : SkipList<int> in 
        myList.Add(5);
     
        let theresult = "HELLO"+myList.DeleteMin().ToString() + "WORLD" in
            Console.WriteLine theresult;
    0 // return an integer exit code


