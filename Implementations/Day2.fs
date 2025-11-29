[<RequireQualifiedAccess>]
module Day2

// Constants
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

type ThrowMap =
    | A
    | B
    | C
    | X
    | Y
    | Z

type MappingStrategy = ThrowMap -> Throw option * RoundResult option

type ThrowMapGroup = { Left: ThrowMap; Right: ThrowMap }

type ThrowMatch = { Left: Throw; Right: Throw }

let mapOpponentThrowStrategy: MappingStrategy =
    function
    | A -> (Some Throw.Rock, None)
    | B -> (Some Throw.Paper, None)
    | C -> (Some Throw.Scissors, None)
    | X
    | Y
    | Z -> (None, None)

let mapRoundResultStrategy: MappingStrategy =
    function
    | X -> (None, Some RoundResult.Lose)
    | Y -> (None, Some RoundResult.Draw)
    | Z -> (None, Some RoundResult.Win)
    | A
    | B
    | C -> (None, None)

let mapStarOneStrategy: MappingStrategy =
    function
    | A
    | X -> (Some Throw.Rock, None)
    | B
    | Y -> (Some Throw.Paper, None)
    | C
    | Z -> (Some Throw.Scissors, None)

let getMappingStrategy (star: Day1.StarSwitch) (isRightColumn: bool) : MappingStrategy =
    match star, isRightColumn with
    | Day1.StarOne, _ -> mapStarOneStrategy
    | Day1.StarTwo, false -> mapOpponentThrowStrategy
    | Day1.StarTwo, true -> mapRoundResultStrategy

let calculateMatchScore: ThrowMatch -> int =
    function
    | { Left = Rock; Right = Rock } -> tie + rockBonus
    | { Left = Rock; Right = Paper } -> win + paperBonus
    | { Left = Rock; Right = Scissors } -> loss + scissorsBonus
    | { Left = Paper; Right = Rock } -> loss + rockBonus
    | { Left = Paper; Right = Paper } -> tie + paperBonus
    | { Left = Paper; Right = Scissors } -> win + scissorsBonus
    | { Left = Scissors; Right = Rock } -> win + rockBonus
    | { Left = Scissors; Right = Paper } -> loss + paperBonus
    | { Left = Scissors; Right = Scissors } -> tie + scissorsBonus

let matchThrowToRoundResult: Throw * RoundResult -> ThrowMatch =
    function
    | Rock, Lose -> { Left = Rock; Right = Scissors }
    | Rock, Draw -> { Left = Rock; Right = Rock }
    | Rock, Win -> { Left = Rock; Right = Paper }
    | Paper, Lose -> { Left = Paper; Right = Rock }
    | Paper, Draw -> { Left = Paper; Right = Paper }
    | Paper, Win -> { Left = Paper; Right = Scissors }
    | Scissors, Lose -> { Left = Scissors; Right = Paper }
    | Scissors, Draw -> { Left = Scissors; Right = Scissors }
    | Scissors, Win -> { Left = Scissors; Right = Rock }

let calculateAllScores throws =
    throws |> Seq.map calculateMatchScore |> Seq.sum

let mapThrows (lines: string seq) : ThrowMapGroup seq =
    let matchThrow =
        function
        | 'A' -> A
        | 'X' -> X
        | 'B' -> B
        | 'Y' -> Y
        | 'C' -> C
        | 'Z' -> Z
        | _ -> failwithf "Fails"

    seq {
        for line in (lines: string seq) do
            yield
                { Left = matchThrow line[0]
                  Right = matchThrow line[2] }
    }

let nameThrowsStarOne (parsedLines: ThrowMapGroup seq) : seq<ThrowMatch> =
    let matchThrow =
        function
        | A
        | X -> Rock
        | B
        | Y -> Paper
        | C
        | Z -> Scissors

    seq {
        for throw in (parsedLines: ThrowMapGroup seq) do
            yield
                { Left = matchThrow throw.Left
                  Right = matchThrow throw.Right }
    }

let nameThrowsStarTwo lines : seq<Throw * RoundResult> =
    // In this case, instead of knowing what the second throw is, we know what the finish should be.
    // Then we just match up the throw with what the throw should be and do the same thing as before.

    let matchThrow =
        function
        | A -> Rock
        | B -> Paper
        | C -> Scissors

    let matchRoundResult =
        function
        | X -> Lose
        | Y -> Draw
        | Z -> Win

    seq {
        for line in (lines: ThrowMapGroup seq) do
            yield (matchThrow line.Left, matchRoundResult line.Right)
    }

let mapInputLine (star: Day1.StarSwitch) (parsedLines: ThrowMapGroup seq) =
    let mapLeft = getMappingStrategy star false
    let mapRight = getMappingStrategy star true
    
    seq {
        for throwMap in parsedLines do
            let leftThrow = mapLeft throwMap.Left |> function (Some t, _ ) -> t | _ -> failwith "Bad left map"
            
            match star with
            | Day1.StarOne ->
               let rightThrow = mapRight throwMap.Right |> function (Some t, _ ) -> t | _ -> failwith "Bad right map"
               yield Choice1Of2 {Left = leftThrow; Right = rightThrow}
            | Day1.StarTwo ->
                let requiredResult = mapRight throwMap.Right |> function (_, Some t) -> t | _ -> failwith "Bad right map"
                yield Choice2Of2 (leftThrow, requiredResult)
    }
    
let calculateScoreStarOne lines =
    lines |> mapThrows |> nameThrowsStarOne |> calculateAllScores

let calculateScoreStarTwo lines =
    lines
    |> mapThrows
    |> nameThrowsStarTwo
    |> Seq.map matchThrowToRoundResult
    |> calculateAllScores
