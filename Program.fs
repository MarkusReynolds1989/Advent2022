open System
open System.IO

let createElfSupplies (lines: string []) =
    seq {
        let tempCalories = ResizeArray<int>()

        for line in lines do
            if String.IsNullOrWhiteSpace(line) then
                yield Seq.sum tempCalories
                tempCalories.Clear()
            else
                tempCalories.Add(int line)
    }

let elfSuppliesMax lines = createElfSupplies lines |> Seq.max

let elfSuppliesTopThree lines =
    createElfSupplies lines
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum

[<EntryPoint>]
let main args =
    let lines =
        File.ReadAllLines("C:\Users\marku\Code\F#\Advent2022\elf_calories.txt")

    printfn $"{elfSuppliesTopThree lines}"
    0
