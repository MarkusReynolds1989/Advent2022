[<RequireQualifiedAccess>]
module Day2

// Constants
[<Literal>]
let test =
    "A Y
B X
C Z"

[<Literal>]
let path = "day_2.txt"

[<Literal>]
let win = 6

[<Literal>]
let tie = 3

[<Literal>]
let loss = 0

[<Literal>]
let scissorsBonus = 3

[<Literal>]
let paperBonus = 2

[<Literal>]
let rockBonus = 1

type Throw =
    | Rock
    | Paper
    | Scissors

type RoundResult =
    | Lose
    | Draw
    | Win

type ThrowCouple = { Left: Throw; Right: Throw}

let calculateScore (throw: ThrowCouple) =
    match throw with
    | {Left = Rock; Right = Rock} -> tie + rockBonus
    | {Left = Rock; Right = Paper} -> win + paperBonus
    | {Left = Rock; Right = Scissors} -> loss + scissorsBonus
    | {Left = Paper; Right = Rock} -> loss + rockBonus
    | {Left = Paper; Right = Paper} -> tie + paperBonus
    | {Left = Paper; Right = Scissors} -> win + scissorsBonus
    | {Left = Scissors; Right = Rock} -> win + rockBonus
    | {Left = Scissors; Right = Paper} -> loss + paperBonus
    | {Left = Scissors; Right = Scissors} -> tie + scissorsBonus

let matchThrowToRoundResult (throwAndRoundResult: Throw * RoundResult) : Throw * Throw =
    throwAndRoundResult
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

let nameThrowsStarOne (lines: string seq) : seq<Throw * Throw> =
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
        for line in (lines: string seq) do
            yield (matchThrow line[0], matchThrow line[2])
    }

let nameThrowsStarTwo lines : seq<Throw * RoundResult> =
    // In this case, instead of knowing what the second throw is, we know what the finish should be.
    // Then we just match up the throw with what the throw should be and do the same thing as before.

    let matchThrow throw =
        throw
        |> function
            | 'A' -> Rock
            | 'B' -> Paper
            | 'C' -> Scissors
            | _ -> failwith "Invalid input"

    let matchRoundResult roundResult =
        roundResult
        |> function
            | 'X' -> Lose
            | 'Y' -> Draw
            | 'Z' -> Win
            | _ -> failwith "Invalid input"

    seq {
        for line in (lines: string seq) do
            yield (matchThrow line[0], matchRoundResult line[2])
    }

let calculateScoreStarOne lines =
    nameThrowsStarOne lines |> calculateAllScores

let calculateScoreStarTwo lines =
    nameThrowsStarTwo lines |> Seq.map matchThrowToRoundResult |> calculateAllScores
