module Day3

let path =
    "C:\Users\marku\Code\F#\Advent2022\Data\day_3.txt"

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

// It would be better if we could just convert the characters to ascii without having to use a lookup table, however
// there are a lot of problems with taking that approach.
// First off, what if the characters we get are unicode? UTF-8 maps the letters to ASCII usually correctly.
// When I tried to do that here, it didn't work.
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
            let line = line.ReplaceLineEndings()
            let half = line.Length / 2
            let length = line.Length
            (line.Substring(0, half), line.Substring(half, length - half))
    }

let parseRucksackContentsChunk (lines: string []) =
    lines
    |> Seq.map (fun line -> line.ReplaceLineEndings())
    |> Seq.chunkBySize 3

let parseRucksackContentsToPrioritiesByChunk (rucksacks: seq<string []>) =
    let lookupPriority value = Map.find value lettersTable

    rucksacks
    |> Seq.map (fun rucksack ->
        seq {
            yield rucksack[0] |> Seq.map lookupPriority
            yield rucksack[1] |> Seq.map lookupPriority
            yield rucksack[2] |> Seq.map lookupPriority
        }
        |> Seq.map Set.ofSeq
        |> Set.intersectMany
        |> (fun result -> result |> Set.toArray |> (fun value -> value[0])))

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

let sumPrioritiesOfIntersections lines =
    parseRucksackContents lines
    |> parseRucksackContentsToPriorities
    |> calculateCommonPriorities
    |> Seq.sum

let sumPrioritiesOfIntersectionsByChunk lines =
    parseRucksackContentsChunk lines
    |> parseRucksackContentsToPrioritiesByChunk
    |> Seq.sum
