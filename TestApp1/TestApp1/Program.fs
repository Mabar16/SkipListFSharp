// Learn more about F# at http://fsharp.org
//#r "FsCheck"

open FsCheck
open System
open DataStructures.Lists



[<EntryPoint>]
let main argv =
    let myList = SkipList() : SkipList<int> in 
        myList.Add(5);
     
        let theresult = "HELLO"+myList.DeleteMin().ToString() + "WORLD" in
            Console.WriteLine theresult;
    0 // return an integer exit code
