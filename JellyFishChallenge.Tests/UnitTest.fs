module UnitTest

open NUnit.Framework
open JellyFishWorld.Model
open JellyFishWorld.Control
open JellyFishWorld.IO
open System


[<Test>]
let ``check JF commands``() =
  let jf = {x=0; y=0; direction=North; dead=false}

  let r1 = forward jf
  Assert.AreEqual((0, 1), (r1.x, r1.y), "Position #1")

  let r2 = r1 |> right |> forward
  Assert.AreEqual((1,1), (r2.x, r2.y), "Position #2")

  // left and forward 4*N times is NOP
  for n in [1..20] do
      let r3 = Seq.init n (fun _ -> left >> forward) |> Seq.fold (|>) r2
      if n % 4 = 0 then
        Assert.AreEqual(r2, r3, "Position #3.1")
       else
        Assert.AreEqual(1, max (abs (r2.x - r3.x)) (abs (r2.y - r3.y)), "Position #3.2")


[<Test>]
let ``check Tank operations``() =
    let tank = { width=4; height=6; scents=Set.empty }
    let jf = { x=2; y=2; direction=South; dead=false }
    Assert.IsTrue(tank.contains jf)

    let jf_out = [{jf with x = 4}; {jf with y = 60}; {jf with x = -1}; {jf with y = -1}; {jf with x = 0; y = -1}]
    let nFalses = Seq.init (Seq.length jf_out) (fun _ -> false)
    let checkResults = Seq.map tank.contains jf_out 
    Assert.AreEqual(nFalses, checkResults)

    // scents can be in the outer cells only
    Assert.Throws<ArgumentException>(fun () -> tank.addScent jf |> ignore) |> ignore

    let jf_border = jf |> left |> forward
    let tank2 = tank.addScent jf_border 
    Assert.IsFalse(tank.hasScent jf)
    Assert.IsFalse(tank.hasScent jf_border)
    Assert.IsFalse(tank2.hasScent jf)
    Assert.IsTrue(tank2.hasScent jf_border)


[<Test>]
let ``Check tank string parsing``() =
    let tankStrs = [|
        "9 19"
        " 9   19 "
        " +9  +19"
        "009   +019"
    |]
    let goodTank = {width=10; height=20; scents=Set.empty}
    for s in tankStrs do
        Assert.AreEqual(goodTank, parseTank(s))
    
    for s in //wrong number of fields
        [|
        ""
        "53"
        " 5 5 5"
        |] do Assert.Throws<Exception>(fun () -> parseTank s |> ignore) |> ignore

    for s in //wrong number format
        [|
        "---"
        "a1 1a"
        " 5a 5a"
        "abc; de"
        |] do Assert.Throws<FormatException>(fun () -> parseTank s |> ignore) |> ignore

[<Test>]
let ``Check result string``() =
    let data = [|
        "3 5 E"
        "5 2 W"
        "20 31 S"
        "0 0 N"
        |]
    for s in data do 
        Assert.AreEqual(s, s |> parseJellyFish |> resultString)
        Assert.AreEqual(s + " LOST", {parseJellyFish s with dead = true} |> resultString)

[<Test>]
let ``Check Jellyfish string parsing``() =
    let jf = {x=9; y=19; dead=false; direction=North}
    let fishData = [|
        "9 19 W",           { jf with direction = West}
        " 9   19  E",       { jf with direction = East}
        " +9  +19   S" ,    { jf with direction = South}
        "009   +019    N",  { jf with direction = North}
    |]
    for s,jf in fishData do
        Assert.AreEqual(jf, parseJellyFish(s))

[<Test>]
let ``Check command sequence processing``() =
    let data = 
        [|
        "1 1 E"
        "RFRFRFRF"
        "3 2 N"
        "FRRFLLFFRRFLL"
        "0 3 W"
        "LLFFFLFLFL"
    |]
    let expected = 
        [|
        data.[0] |> parseJellyFish, data.[1]
        data.[2] |> parseJellyFish, data.[3]
        data.[4] |> parseJellyFish, data.[5]
    |]
    data |> jfFromStrings
    |> Seq.zip expected
    |> Seq.iter (fun (r, e) -> Assert.AreEqual(e, r))

[<Test>]
let ``Check base scenario from the task description``() =
    let tank = "5 3" |> parseTank
    let jf1 = "1 1 E" |> parseJellyFish
    let t1, r1 =  "RFRFRFRF" |> commandString commandMap |> runCommands tank jf1
    Assert.AreEqual("1 1 E", r1 |> resultString)

    let jf2 = "3 2 N" |>parseJellyFish
    let t2, r2 = "FRRFLLFFRRFLL" |> commandString commandMap |> runCommands t1 jf2 
    Assert.AreEqual("3 3 N LOST", r2 |> resultString)

    let jf3 = "0 3 W" |>parseJellyFish
    let _, r3 = "LLFFFLFLFL" |> commandString commandMap |> runCommands t2 jf3 
    Assert.AreEqual("2 3 S", r3 |> resultString)
