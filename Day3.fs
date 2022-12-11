module Day3

open System
open System.Text

let path = "C:\Users\marku\Code\F#\Advent2022\Data\day_3.txt"
let test =
    "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"

let lowerLetters = [ 'a' .. 'z' ]
let upperLetters = [ 'A' .. 'Z' ]
let letters = lowerLetters @ upperLetters

let lettersTable =
    List.fold
        (fun acc value ->
            let currentIndex = fst acc
            let nextIndex = fst acc + 1
            let listState = snd acc
            (nextIndex, List.append [ (value, currentIndex) ] listState))
        (1, [])
        letters
    |> snd
    |> Map.ofList

let parseRucksackContents (lines: string []) : seq<string * string> =
    seq {
        for line in lines do
            let line = line.Replace("\r", "")
            let half = line.Length / 2
            let length = line.Length
            (line.Substring(0, half), line.Substring(half, length - half))
    }

let parseRucksackContentsToPriorities (rucksacks: seq<string * string>) : seq<seq<int> * seq<int>> =
    rucksacks
    |> Seq.map (fun rucksack ->
        ((fst rucksack)
         |> Seq.map (fun value -> Map.find value lettersTable),
         (snd rucksack)
         |> Seq.map (fun value -> Map.find value lettersTable)))

let calculateCommonPriorities (rucksacks: seq<seq<int> * seq<int>>) =
    rucksacks
    |> Seq.map (fun rucksack ->
        let firstRuck, secondRuck = rucksack
        let firstRuck = firstRuck |> Set.ofSeq
        let secondRuck = secondRuck |> Set.ofSeq

        Set.intersect firstRuck secondRuck
        |> Set.toArray
        |> (fun value -> value[0]))

let calculatePrioritiesOfUnions lines =
    parseRucksackContents lines
    |> parseRucksackContentsToPriorities
    |> calculateCommonPriorities
    |> Seq.sum
