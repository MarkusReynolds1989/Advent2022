module Tests

open System.IO

open Xunit

open Microsoft.Extensions.Logging

type DayOneFixture() =
    let lines = File.ReadAllLines Day1.filePath |> Array.toSeq

    let loggerFactory =
        LoggerFactory.Create(fun builder ->
            builder.AddConsole() |> ignore
            builder.AddDebug() |> ignore
            builder.SetMinimumLevel(LogLevel.Debug) |> ignore)

    let logger = loggerFactory.CreateLogger<DayOneFixture>()

    member this.Logger = logger
    member this.Lines = lines

    interface System.IDisposable with
        member this.Dispose() = loggerFactory.Dispose()


[<Collection("Day1Tests")>]
type Day1Tests(fixture: DayOneFixture) =
    let data = fixture.Lines
    let logger = fixture.Logger

    interface IClassFixture<DayOneFixture>

    [<Fact>]
    member this.``Day 1 Star 1``() =
        match Day1.calculateElfSupplies logger Day1.StarSwitch.StarOne data with
        | Ok result -> Assert.Equal(69_883, result)
        | Error error -> Assert.Fail(error)

    [<Fact>]
    member this.``Day 1 Star 2``() =
        match Day1.calculateElfSupplies logger Day1.StarSwitch.StarTwo data with
        | Ok result -> Assert.Equal(207_576, result)
        | Error error -> Assert.Fail(error)
