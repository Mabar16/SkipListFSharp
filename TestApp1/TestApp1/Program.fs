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
    skipList.Count = fsList.Length && 
        let rec matchList = fun (skipList2:SkipList<int>) (fsList2:List<int>) ->
            match fsList2 with
            |  [] -> true
            | a::rest -> skipList2.Contains(a) && matchList skipList2 rest 
        in matchList skipList fsList

let skipListSpec = 
    let add i = 
        { new Operation<SkipList<int>,List<int>>() with
            member __.Run m = List.sort (i::m)
            member __.Check (c, m) =
                c.Add(i)
                let res = isSkipListEqualToList c m//c.Contains(i)
                   in res = true
                |@ sprintf "Add: model = %s, actual = %s" (m.ToString()) (c.ToString())
            override __.ToString() = sprintf "add %i" i }
    let rem i = 
        { new Operation<SkipList<int>,List<int>>() with
            member __.Run m = removeFromList m i
            override __.Pre m = 
                (m.Length) > 0
            member __.Check (c,m) = 
                c.Remove(i) |> ignore;
                let res =isSkipListEqualToList c m 
                in res = true
                |@ sprintf "Rem: model = %s, actual = %s" (m.ToString()) (c.ToString())
            override __.ToString() = sprintf "rem %i" i 
            }
    let clear = 
        { new Operation<SkipList<int>, List<int>>() with
            member __.Run m = []
            member __.Check (c,m) = 
                c.Clear()
                let res = isSkipListEqualToList c m && c.IsEmpty
                in res = true
                |@ sprintf "Clear: model = %s, actual = %s" (m.ToString()) (c.ToString())
            override __.ToString() = sprintf "clear"
            }
    let peek = 
        { new Operation<SkipList<int>, List<int>>() with
            member __.Run m = m
            override __.Pre m = 
                (m.Length) > 0
            member __.Check (c,m) = 
                
                let res = c.Peek()
                in let mres = m.Head
                    in res = mres
                |@ sprintf "Peek returned: model = %i, actual = %i" mres res
            override __.ToString() = sprintf "peek"
            }
    let find i= 
        { new Operation<SkipList<int>, List<int>>() with
            member __.Run m = m
            member __.Check (c,m) = 
                
                let res = c.Find(i) // res is a bool * int tuple
                in let mres = List.contains i m
                    in ((mres = false && (fst res) = false) ||                    
                        ((fst res) = mres && (snd res) = i))
                |@ sprintf "Find returned: model = %A, actual = %A" (mres,i) res
            override __.ToString() = sprintf "find %i" i
            }
    let contains i= 
          { new Operation<SkipList<int>, List<int>>() with
              member __.Run m = m
              member __.Check (c,m) = 
                  
                  let res = c.Contains(i) // res is a bool
                  in let mres = List.contains i m
                      in (res = mres)
                  |@ sprintf "Contains returned: model = %b, actual = %b" mres res
              override __.ToString() = sprintf "contains %i" i
              }
    let create = 
        { new Setup<SkipList<int>,List<int>>() with
            member __.Actual() = SkipList(123) : SkipList<int>
            member ___.Model() = [] }
    { new Machine<SkipList<int>, List<int>>() with 
        member __.Setup =  Gen.constant create |> Arb.fromGen
        member __.Next _ =  Gen.oneof[ Gen.choose(0,100) |> Gen.map2 (fun op i -> op i) (Gen.elements [add;rem;find;contains]); Gen.elements [clear; peek]]}


[<EntryPoint>]
Check.Quick (StateMachine.toProperty skipListSpec)



let main argv =
 
    0 // return an integer exit code


