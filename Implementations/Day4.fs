module Day4

open System
open System.IO

let path = "day_4.txt"
let lines = File.ReadAllLines(path)


type Pair = { X: int; Y: int }

let test =
    "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"

let generatePairs (lines: string[]) =
    Array.map
        (fun (line: string) ->
            let pairs = line.Split(",")
            let firstPair = pairs[0].Split("-")
            let secondPair = pairs[1].Split("-")

            ({ X = int firstPair[0]
               Y = int firstPair[1] },
             { X = int secondPair[0]
               Y = int secondPair[1] }))
        lines

let intoFirst (pair: Pair * Pair) =
    let first, second = pair
    first.X <= second.X && first.Y >= second.Y

let intoSecond (pair: Pair * Pair) =
    let first, second = pair
    second.X <= first.X && second.Y >= first.Y

let containsLogicStrict (pair: Pair * Pair) = intoFirst pair || intoSecond pair

let containsPair (pairsCollection: (Pair * Pair)[]) =
    Array.map containsLogicStrict pairsCollection

let calculateContainingPairs input =
    input
    |> generatePairs
    |> containsPair
    |> Array.filter (fun value -> value = true)
    |> Array.length
