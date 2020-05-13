﻿// Learn more about F# at http://fsharp.org
//#r "FsCheck"

open FsCheck
open System
open DataStructures.Lists
open FsCheck.Experimental
//module List
let mutable statslist = [] :List<int>

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

let betterEquals = fun (skipList:SkipList<int>) (fsList:List<int>) ->
    let mutable alist = fsList
    let mutable equalsSoFar = true
    let mutable count = 0
    for item in skipList do
        count <- count+1
        match alist with 
        item2::rest -> if item <> item2 then equalsSoFar <- false else alist <- rest
        |[] -> equalsSoFar <- false
    equalsSoFar && count = List.length fsList

let numberGen = Gen.frequency([(8,Arb.generate<int>);(1,Gen.constant(System.Int32.MinValue));(1,Gen.constant(System.Int32.MaxValue))])

let skipListSpec = 
    let add i = 
        { new Operation<SkipList<int>,List<int>>() with
            member __.Run m = List.sort (i::m)
            member __.Check (c, m) =
                c.Add(i)
                statslist <- i::statslist
                let res = betterEquals c m//c.Contains(i)
                   in res = true
                |@ sprintf "Add: model = %s, actual = %s" (m.ToString()) (c.ToString())
            override __.ToString() = sprintf "add %i" i }
    let rem i = 
        { new Operation<SkipList<int>,List<int>>() with
            member __.Run m = removeFromList m i
            override __.Pre m = 
                (m.Length) > 0
            member __.Check (c,m) = 
                statslist <- i::statslist
                c.Remove(i) |> ignore;
                let res =betterEquals c m 
                in res = true
                |@ sprintf "Rem: model = %s, actual = %s" (m.ToString()) (c.ToString())
            override __.ToString() = sprintf "rem %i" i 
            }
    let clear = 
        { new Operation<SkipList<int>, List<int>>() with
            member __.Run m = []
            member __.Check (c,m) = 
                c.Clear()
                let res = betterEquals c m && c.IsEmpty
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
                statslist <- i::statslist
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
                  statslist <- i::statslist
                  let res = c.Contains(i) // res is a bool
                  in let mres = List.contains i m
                      in (res = mres)
                  |@ sprintf "Contains returned: model = %b, actual = %b" mres res
              override __.ToString() = sprintf "contains %i" i
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
    let create = 
        { new Setup<SkipList<int>,List<int>>() with
            member __.Actual() = SkipList(123) : SkipList<int>
            member ___.Model() = [] }
    { new Machine<SkipList<int>, List<int>>() with 
        member __.Setup =  Gen.constant create |> Arb.fromGen
        member __.Next _ =  Gen.oneof[ Gen.choose(-100,100) |> Gen.map2 (fun op i -> op i) (Gen.elements [add;rem;find;contains]); Gen.elements [clear; peek;count;isEmpty]]}


let configuration = {Config.Quick with MaxTest = 10; Config.Name = "SkipList test" } in
Check.One(configuration,  StateMachine.toProperty skipListSpec)

let rec countelemsininterval list min max accum = match list with
    [] -> accum
    | i::rest -> 
    if (i >= min && i < max) 
    then countelemsininterval rest min max (1+accum) 
    else countelemsininterval rest min max accum
    

let printstats list =  
    Console.WriteLine("less than 0: " + (countelemsininterval list -999999 0 0).ToString())
    Console.WriteLine("0-5: " + (countelemsininterval list 0 5 0).ToString())
    Console.WriteLine("5-100: " + (countelemsininterval list 5 100 0).ToString())
    Console.WriteLine("100-1000: " + (countelemsininterval list 100 1000 0).ToString())
    Console.WriteLine("LARGE: " + (countelemsininterval list 1000 99999999 0).ToString())
    
printstats statslist

[<EntryPoint>]
let main argv =
    let sl = SkipList<int>(123) in sl.Add(1); sl.Add(1); sl.Add(2);
        Console.WriteLine(betterEquals sl [2;2;1])
    0 // return an integer exit code


