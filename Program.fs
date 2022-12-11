module Program

open System.IO

[<EntryPoint>]
let main args =
    let lines =
        File.ReadAllLines("C:\Users\marku\Code\F#\Advent2022\elf_calories.txt")

    printfn $"{Day1.elfSuppliesTopThree lines}"
    0
