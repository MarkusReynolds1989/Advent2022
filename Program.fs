module Program
open System
open System.IO

[<EntryPoint>]
let main args =

    let lines = File.ReadAllLines(Day4.path)
    let test = Day4.test.Split("\n") 
    Console.WriteLine($"{Day4.calculateContainingPairs lines}")
    0
