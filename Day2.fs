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
