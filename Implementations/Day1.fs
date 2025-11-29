[<RequireQualifiedAccess>]
module Day1

open System
open Microsoft.Extensions.Logging

let filePath = "elf_calories.txt"

type ListLine =
    | End
    | Line of int

type StarSwitch =
    | StarOne
    | StarTwo

type CalculationStrategy = ILogger -> int list -> Result<int, string>

let maxStrategy (logger: ILogger) (elfSupplies: int list) : Result<int, string> =
    logger.LogInformation("Getting the maximum elf supplies")
    let final = elfSupplies |> Seq.max
    logger.LogInformation("Maximum is: {Final}", final)
    Ok final

let topThreeSumStrategy (logger: ILogger) (elfSupplies: int list) : Result<int, string> =
    logger.LogInformation("Getting the top three elf supplies")
    let final = elfSupplies |> Seq.sortDescending |> Seq.take 3 |> Seq.sum
    logger.LogInformation("Sum of top three is: {Final}", final)
    Ok final

let getStrategy (starSwitch: StarSwitch) : CalculationStrategy =
    match starSwitch with
    | StarOne -> maxStrategy
    | StarTwo -> topThreeSumStrategy

let parseLine (logger: ILogger) (line: string) : Result<ListLine, string> =
    let mutable output = ref 0

    if String.IsNullOrWhiteSpace(line) then
        Ok End
    else
        Int32.TryParse(line, output)
        |> function
            | true -> Ok(Line output.Value)
            | false ->
                let failureError = $"Failed to parse {line}"
                logger.LogError(failureError)
                Error failureError

let parseElfSuppliesInput (logger: ILogger) (lines: string seq) : Result<ListLine list, string> =
    logger.LogInformation("Parsing elf supplies list")

    lines
    |> Seq.fold
        (fun accResult line ->
            match accResult with
            | Error err -> Error err
            | Ok accumulatedList ->
                match parseLine logger line with
                | Ok parsedValue -> Ok(parsedValue :: accumulatedList)
                | Error err -> Error err)
        (Ok [])
    |> Result.map List.rev

let createElfSupplies (logger: ILogger) (lines: ListLine list) : int list =
    let rec loop (returnList: int list) (acc: int) (lines: ListLine list) =
        match lines with
        | head :: tail ->
            match head with
            | End -> loop (acc :: returnList) 0 tail
            | Line number -> loop returnList (acc + number) tail
        | [] ->
            logger.LogInformation("Completed creating elf supplies")
            returnList

    logger.LogInformation("Creating elf supplies")
    loop [] 0 lines

let calculateElfSupplies (logger: ILogger) (starSwitch: StarSwitch) (lines: string seq) : Result<int, string> =
    let strategy = getStrategy starSwitch

    parseElfSuppliesInput logger lines
    |> Result.bind (fun parsedSupplies ->
        let totalsList = createElfSupplies logger parsedSupplies
        Ok totalsList)
    |> Result.bind (strategy logger)
