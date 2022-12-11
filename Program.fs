open System
open System.IO

let rec createElfSupplies (lines: list<string>) (acc: list<int>) : list<list<int>> =
    match lines with
    | [] -> []
    | value :: _ when String.IsNullOrWhiteSpace(value) -> (createElfSupplies (List.tail lines) List.empty) @ [ acc ]
    | _ -> createElfSupplies (List.tail lines) (acc @ [ int (List.head lines) ])

let elfSuppliesMax lines =
    
    createElfSupplies lines []
    |> List.map List.sum
    |> List.max

let elfSuppliesTopThree lines =
    
    createElfSupplies lines []
    |> List.map List.sum
    |> List.toArray
    |> Array.sortDescending
    |> Array.take 3
    |> Array.sum

[<EntryPoint>]
let main args =
    let lines =
        File.ReadAllText("C:\Users\marku\Code\F#\Advent2022\elf_calories.txt")
        |> (fun lines -> Seq.append lines "\n" |> String.Concat)
        |> (fun lines -> lines.Split("\n"))
        |> Seq.toList
        
    printfn $"{elfSuppliesTopThree lines}"
    0
