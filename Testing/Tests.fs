module Tests

open Xunit
open System.IO

[<Fact>]
let ``Day 1 Star 1`` () =
    let lines = File.ReadAllLines Day1.filePath
    let result = Day1.elfSuppliesMax lines
    Assert.Equal(69_883, result)

[<Fact>]
let ``Day 1 Star 2`` () =
    let lines = File.ReadAllLines Day1.filePath
    let result = Day1.elfSuppliesTopThree lines
    Assert.Equal(207_576, result)
