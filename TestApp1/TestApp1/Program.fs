// Learn more about F# at http://fsharp.org
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



let removeMin = fun aList -> match aList with 
    | [] -> []
    | item::rest -> rest

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
            //lazy 
            c.Remove(i) |> ignore;
            let res =listEquals c m 
            in  res = true 
            //|> Prop.within 1000 
              
           
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
let deleteMin = 
    { new Operation<SkipList<int>, List<int>>() with
        member __.Run m = removeMin (List.sort m)
        override __.Pre m = 
            (m.Length) > 0
        member __.Check (c, m) =
            c.DeleteMin() |> ignore
            deleteMinCounter <- deleteMinCounter + 1
            let res = listEquals c m
            in res = true
            |@ sprintf "Delete Min: model = %A, actual = %A" m c
        override __.ToString() = sprintf "delete min"
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

let numberGen = Gen.frequency([(8,Arb.generate<int>);(3, Gen.choose(-3,3));(1,Gen.constant(System.Int32.MinValue));(1,Gen.constant(System.Int32.MaxValue))])
let opsGen1 = Gen.frequency([(1,Gen.constant clear); (2,Gen.constant peek);(2,Gen.constant count);(2,Gen.constant isEmpty);(3,Gen.constant deleteMin)])
let opsGen2 = Gen.frequency([(5,Gen.constant add);(3,Gen.constant rem);(2,Gen.constant find);(2,Gen.constant contains)])
let opsGen2withArguments = 
    numberGen 
    |> Gen.map2 (fun op i -> op i) opsGen2
let operationsGen = Gen.frequency[(1,opsGen1);(3,opsGen2withArguments)]

let skipListSpec = 
    let create seed= 
        { new Setup<SkipList<int>,List<int>>() with
            member __.Actual() = SkipList(seed) : SkipList<int>
            member ___.Model() = [] }
    { new Machine<SkipList<int>, List<int>>() with 
        member __.Setup =  Arb.generate<int> |> Gen.map create |> Arb.fromGen
        member __.Next _ =  operationsGen}


let configuration = {Config.Quick with MaxTest = 10000; Config.Name = "SkipList test" } in
Check.One(configuration,  StateMachine.toProperty skipListSpec)

let rec countelemsininterval list min max accum = match list with
    [] -> accum
    | i::rest -> 
    if (i >= min && i <= max) 
    then countelemsininterval rest min max (1+accum) 
    else countelemsininterval rest min max accum
    
let commandSum = addCounter + remCounter + clearCounter + countCounter + emptyCounter + containsCounter + findCounter + peekCounter + deleteMinCounter
let printstats list =  
    
    Console.WriteLine("System.Int32.MinValue: " + (countelemsininterval list (System.Int32.MinValue) System.Int32.MinValue 0).ToString())
    Console.WriteLine("System.Int32.MinValue+1 -200: " + (countelemsininterval list (System.Int32.MinValue+1) -200 0).ToString())
    Console.WriteLine("-200 -5: " + (countelemsininterval list -200 -5 0).ToString())
    Console.WriteLine("-5 0: " + (countelemsininterval list -5 0 0).ToString())
    Console.WriteLine("0 5: " + (countelemsininterval list 0 5 0).ToString())
    Console.WriteLine("5 200: " + (countelemsininterval list 5 200 0).ToString())
    Console.WriteLine("200 System.Int32.MaxValue: " + (countelemsininterval list 200 (System.Int32.MaxValue-1) 0).ToString())
    Console.WriteLine("System.Int32.MaxValue: " + (countelemsininterval list System.Int32.MaxValue System.Int32.MaxValue 0).ToString())
    Console.WriteLine("\n")
    Console.WriteLine("Adds: "+ float(float(addCounter)/float(commandSum)).ToString())
    Console.WriteLine("Rems: "+ float(float(remCounter)/float(commandSum)).ToString())
    Console.WriteLine("Clears: "+ float(float(clearCounter)/float(commandSum)).ToString())
    Console.WriteLine("Counts: "+ float(float(countCounter)/float(commandSum)).ToString())
    Console.WriteLine("Empties: "+ float(float(emptyCounter)/float(commandSum)).ToString())
    Console.WriteLine("Contains: "+ float(float(containsCounter)/float(commandSum)).ToString())
    Console.WriteLine("Finds: "+ float(float(findCounter)/float(commandSum)).ToString())
    Console.WriteLine("Peeks: "+ float(float(peekCounter)/float(commandSum)).ToString())
    Console.WriteLine("DeleteMins: "+ float(float(deleteMinCounter)/float(commandSum)).ToString())
    
printstats statslist

[<EntryPoint>]
let main argv =
    let sl = SkipList<int>(123) in 
    sl.Contains(64);
    sl.Clear();
    sl.Add(-60); 
    sl.Add(93); 
    sl.Remove(-69);
    //Console.WriteLine(listEquals sl [-59;-57;29])
    sl.DeleteMin();
    sl.Remove(-33);
    sl.DeleteMin();
    sl.Contains(42);
    sl.Contains(-52);
    sl.Add(14);
    Console.WriteLine(listEquals sl [14])
    sl.Find(4);
    Console.WriteLine(listEquals sl [14])
    Console.WriteLine(sl)
    0 // return an integer exit code


