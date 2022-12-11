module Day2

let test =
    "A Y
B X
C Z"

type Throw =
    | Rock
    | Paper
    | Scissors

type RoundFinish =
    | Lose
    | Draw
    | Win

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
    // There are very few correct states, so the memoization will be extremely fast for this problem.
    let result =
        Seq.fold
            (fun (state: Map<Throw * Throw, int> * int) (throw: Throw * Throw) ->
                match Map.containsKey throw (fst state) with
                | true -> (fst state, (snd state) + Map.find throw (fst state))
                | _ -> (Map.add throw (calculateScore throw) (fst state)), ((snd state) + (calculateScore throw)))
            (Map.empty, 0)
            throws

    snd result

let matchThrowToResult (throwResult: Throw * RoundFinish) : Throw * Throw =
    throwResult
    |> function
        | Rock, Lose -> Rock, Scissors
        | Rock, Draw -> Rock, Rock
        | Rock, Win -> Rock, Paper
        | Paper, Lose -> Paper, Rock
        | Paper, Draw -> Paper, Paper
        | Paper, Win -> Paper, Scissors
        | Scissors, Lose -> Scissors, Paper
        | Scissors, Draw -> Scissors, Scissors
        | Scissors, Win -> Scissors, Rock

let nameThrowsStarOne lines : seq<Throw * Throw> =
    let matchThrow throw =
        throw
        |> function
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

let nameThrowsStarTwo lines : seq<Throw * RoundFinish> =
    // In this case, instead of knowing what the second throw is, we know what the finish should be.
    // Then we just match up the throw with what the throw should be and do the same thing as before.

    let matchThrow throw =
        throw
        |> function
            | 'A' -> Rock
            | 'B' -> Paper
            | 'C' -> Scissors
            | _ -> failwith "Invalid input"

    let matchResult result =
        result
        |> function
            | 'X' -> Lose
            | 'Y' -> Draw
            | 'Z' -> Win
            | _ -> failwith "Invalid input"

    seq {
        for line in (lines: string []) do
            yield (matchThrow line[0], matchResult line[2])
    }

let calculateScoreStarTwo lines =
    nameThrowsStarTwo lines
    |> Seq.map matchThrowToResult
    |> calculateAllScores
