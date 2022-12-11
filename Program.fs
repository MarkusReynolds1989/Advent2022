﻿module Program

open System.IO

[<EntryPoint>]
let main args =
    let lines =
        File.ReadAllLines("C:\Users\marku\Code\F#\Advent2022\Data\day_2.txt")

    //let lines = Day2.testOne.Split("\n")

    let result =
        Day2.nameThrows lines |> Day2.calculateAllScores
        
    printfn $"{result}"

    0