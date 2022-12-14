module Day1

open System

let filePath =
    "C:\Users\marku\Code\F#\Advent2022\Data\elf_calories.txt"

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
