namespace UnitTestProject1

open FsCheck
open FsCheck.Experimental
open DataStructures.Lists
open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestMethodPassing () =
        
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
        
        let removeMin = fun aList -> match aList with 
        | [] -> []
        | item::rest -> rest
        
        let listEquals = fun (skipList:SkipList<int>) (fsList:List<int>) ->
            let mutable alist = fsList
            let mutable equalsSoFar = true
            let mutable count = 0
            for item in skipList do
                count <- count+1
                match alist with 
                item2::rest -> if item <> item2 
                                then equalsSoFar <- false 
                                else alist <- rest
                |[] -> equalsSoFar <- false
            equalsSoFar && count = List.length fsList
        
        let numberGen = Gen.frequency([(8,Arb.generate<int>);(1,Gen.constant(System.Int32.MinValue));(1,Gen.constant(System.Int32.MaxValue))])
        
        let mutable addCounter = 0
        let mutable remCounter = 0
        let mutable findCounter = 0
        let mutable peekCounter = 0
        let mutable clearCounter = 0
        let mutable emptyCounter = 0
        let mutable containsCounter = 0
        let mutable countCounter = 0
        let mutable deleteMinCounter = 0
        
        let add i = 
            { new Operation<SkipList<int>,List<int>>() with
                member __.Run m = List.sort (i::m)
                member __.Check (c, m) =
                    addCounter <- addCounter + 1
                    c.Add(i) //|> Prop.collect(i)
                    statslist <- i::statslist
                    let res = listEquals c m//c.Contains(i)
                       in res = true
                    |@ sprintf "Add: model = %s, actual = %s" (m.ToString()) (c.ToString())
                override __.ToString() = sprintf "add %i" i }
        let rem i = 
            { new Operation<SkipList<int>,List<int>>() with
                member __.Run m = removeFromList m i
                override __.Pre m = 
                    (m.Length) > 0
                member __.Check (c,m) = 
                    remCounter <- remCounter + 1
                    statslist <- i::statslist
                    c.Remove(i) |> ignore;
                    let res =listEquals c m 
                    in res = true
                    |@ sprintf "Rem: model = %s, actual = %s" (m.ToString()) (c.ToString())
                override __.ToString() = sprintf "rem %i" i 
                }
        let clear = 
            { new Operation<SkipList<int>, List<int>>() with
                member __.Run m = []
                member __.Check (c,m) = 
                    c.Clear()
                    clearCounter <- clearCounter + 1
                    let res = listEquals c m && c.IsEmpty
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
                    peekCounter <- peekCounter + 1
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
                    findCounter <- findCounter + 1
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
                      containsCounter <- containsCounter + 1
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
                    countCounter <- countCounter + 1
                    let res = c.Count = m.Length
                    in res = true
                    |@ sprintf "Count: model = %i, actual = %i" m.Length c.Count
                override __.ToString() = sprintf "count"
            }
        let deleteMin = 
            { new Operation<SkipList<int>, List<int>>() with
                member __.Run m = removeMin (List.sort m)
                override __.Pre m = 
                    (m.Length) > 0
                member __.Check (c, m) =
                    deleteMinCounter <- deleteMinCounter + 1
                    c.DeleteMin() |> ignore
                    let res = listEquals c m
                    in res = true
                    |@ sprintf "Delete Min: model = %A, actual = %A" m c
                override __.ToString() = sprintf "delete min"
            }
        let isEmpty = 
            { new Operation<SkipList<int>, List<int>>() with
                member __.Run m = m
                member __.Check (c, m) =
                    emptyCounter <- emptyCounter + 1
                    let res = (c.IsEmpty = (m.Length = 0))
                    in res = true
                    |@ sprintf "IsEmpty: model = %b, actual = %b" (m.Length = 0) c.IsEmpty
                 override __.ToString() = sprintf "isEmpty"
            }
        
        let opsGen1 = Gen.elements [clear; peek;count;isEmpty]
        let opsGen2 = Gen.elements [add;rem;find;contains]
        let opsGen2withArguments = 
            Gen.choose(-100,100) 
            |> Gen.map2 (fun op i -> op i) opsGen2
        let operationsGen = Gen.oneof[opsGen2withArguments; opsGen1]
        
        let skipListSpec = 
            let create seed= 
                { new Setup<SkipList<int>,List<int>>() with
                    member __.Actual() = SkipList(seed) : SkipList<int>
                    member ___.Model() = [] }
            { new Machine<SkipList<int>, List<int>>() with 
                member __.Setup =  Arb.generate<int> |> Gen.map create |> Arb.fromGen
                member __.Next _ =  operationsGen}
        
        
        //let configuration = {Config.Quick with MaxTest = 1000; Config.Name = "SkipList test" } in
        Check.QuickThrowOnFailure(StateMachine.toProperty skipListSpec)

