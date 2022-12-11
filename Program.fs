module Program
open System.IO

[<EntryPoint>]
let main args =

    let lines = File.ReadAllLines(Day3.path)
    //let lines = Day3.test.Split("\n")
    printfn $"%A{Day3.calculatePrioritiesOfUnions lines}"
    0
