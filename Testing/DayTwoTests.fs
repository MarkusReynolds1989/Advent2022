module DayTwoTests

open System.IO

open Xunit

open Microsoft.Extensions.Logging

type DayTwoFixture() =
    let lines = File.ReadAllLines Day2.path |> Array.toSeq

    let loggerFactory =
        LoggerFactory.Create(fun builder ->
            builder.AddConsole() |> ignore
            builder.AddDebug() |> ignore
            builder.SetMinimumLevel(LogLevel.Debug) |> ignore)

    let logger = loggerFactory.CreateLogger<DayTwoFixture>()

    member this.Logger = logger
    member this.Lines = lines

    interface System.IDisposable with
        member this.Dispose() = loggerFactory.Dispose()


[<Collection("Day1Tests")>]
type Day1Tests(fixture: DayTwoFixture) =
    let data = fixture.Lines
    let logger = fixture.Logger

    interface IClassFixture<DayTwoFixture>

    [<Fact>]
    member this.``Day 2 Star 1``() =
        let result = Day2.calculateScoreStarOne data
        Assert.Equal(14_297, result)

    [<Fact>]
    member this.``Day 2 Star 2``() =
        let result =  Day2.calculateScoreStarTwo data
        Assert.Equal(10_498, result)
