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

let createElfSupplies (logger: ILogger) (lines: ListLine seq) : int seq =
    logger.LogInformation("Creating elf supplies")
    let mutable accumulator = 0

    seq {
        for line in lines do
            match line with
            | End ->
                yield accumulator
                accumulator <- 0
            | Line number -> accumulator <- accumulator + number
    }

let calculateElfSupplies (logger: ILogger) (starSwitch: StarSwitch) (lines: string seq) : Result<int, string> =
    parseElfSuppliesInput logger lines
    |> Result.bind (fun parsedSupplies -> Ok(createElfSupplies logger parsedSupplies))
    |> Result.bind (fun elfSupplies ->
        match starSwitch with
        | StarOne ->
            logger.LogInformation("Getting the maximum elf supplies")
            let final = elfSupplies |> Seq.max
            logger.LogInformation("Maximum is: {Final}", final)
            Ok final
        | StarTwo ->
            logger.LogInformation("Getting the top three elf supplies")
            let final = elfSupplies |> Seq.sortDescending |> Seq.take 3 |> Seq.sum
            logger.LogInformation("Sum of top three is: {Final}", final)
            Ok final)
