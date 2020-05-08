// Learn more about F# at http://fsharp.org
#r "fsCheck"

open FsCheck
open System
open DataStructures.Lists
open FsCheck.Experimental
//module List

let rec matchList = fun (skipList:SkipList<int>) (fsList:List<int>) ->
    match fsList with
        |  [] -> false
        | a::rest -> skipList.Contains(a) && matchList skipList rest