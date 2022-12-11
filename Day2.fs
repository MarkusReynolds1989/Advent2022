module Day2

let testOne =
    "A Y
B X
C Z"

type Throw =
    | Rock
    | Paper
    | Scissors

let calculateScore throw =
    let win = 6
    let tie = 3
    let loss = 0
    let scissorsBonus = 3
    let paperBonus = 2
    let rockBonus = 1

    match throw with
    | Rock, Rock -> tie + rockBonus
    | Rock, Paper -> win + paperBonus
    | Rock, Scissors -> loss + scissorsBonus
    | Paper, Rock -> loss + rockBonus
    | Paper, Paper -> tie + paperBonus
    | Paper, Scissors -> win + scissorsBonus
    | Scissors, Rock -> win + rockBonus
    | Scissors, Paper -> loss + paperBonus
    | Scissors, Scissors -> tie + scissorsBonus

// Memoize all throws here to make the work quicker.
let calculateAllScores throws =
    let result =
        Seq.fold
            (fun (state: Map<Throw * Throw, int>) (throw: Throw * Throw) ->
                match Map.containsKey throw state with
                | true -> state
                | _ -> Map.add throw (calculateScore throw) state)
            Map.empty
            throws

    throws
    |> Seq.map (fun throw -> Map.find throw result)
    |> Seq.sum

let nameThrows lines : seq<Throw * Throw> =
    let matchThrow throw =
        match throw with
        | 'A'
        | 'X' -> Rock
        | 'B'
        | 'Y' -> Paper
        | 'C'
        | 'Z' -> Scissors
        | _ -> failwith $"Invalid input {throw}"

    seq {
        for line in (lines: string []) do
            yield (matchThrow line[0], matchThrow line[2])
    }
