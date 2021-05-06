namespace JellyFishWorld
open System

module Model =
    type Direction = 
      | North    | East    | South    | West
      member this.right = match this with |North -> East   |East -> South  |South -> West  |West -> North
      member this.left =  match this with |North -> West   |East -> North  |South -> East  |West -> South


    type JellyFish =
        {
        x: int
        y: int
        direction: Direction
        dead: bool
        }

    let resultString (jf:JellyFish) :string = 
        let maybeLost = if jf.dead then ["LOST"] else list.Empty
        jf.x.ToString() :: jf.y.ToString() :: String([|jf.direction.ToString().[0]|]) :: maybeLost
            |> String.concat " "

    type Tank =
        {
        width: int
        height: int
        scents: Set<int*int> 
        }
        member this.contains (jf: JellyFish) :bool =
            jf.x >= 0 && jf.y >=0 && jf.x < this.width && jf.y < this.height
        member this.hasScent (jf:JellyFish) :bool =
            this.scents.Contains (jf.x, jf.y)
        member tank.addScent (jf: JellyFish) :Tank =
            if jf.x = 0 || jf.y = 0 || jf.x = tank.width - 1 || jf.y = tank.height - 1 then
                { tank with scents = tank.scents.Add((jf.x, jf.y)) }
            else
                invalidArg "jf" "Scent must be on the tank limit"

module Control =
    open Model
    
    type Command = JellyFish -> JellyFish

    /// rotate counterclockwise
    let left :Command = fun jf ->
        { jf with direction = jf.direction.left }
    
    /// rotate clockwise
    let right :Command = fun jf ->
        { jf with direction = jf.direction.right }
    
    /// move JF forward in the direction it is facing
    let forward :Command = fun jf ->
        match jf.direction with
        | North -> {jf with y = jf.y + 1 }
        | East -> {jf with x = jf.x + 1 }
        | South -> {jf with y = jf.y - 1 }
        | West -> {jf with x = jf.x - 1 }

    let kill jf = { jf with dead = true }


    /// Apply single command to a JF in the tank; return new Tank*JF state
    let applyCommand (tank_jf:Tank*JellyFish) (cmd:Command) : Tank*JellyFish =
        let tank, jf = tank_jf
        if jf.dead then tank_jf //dead JF ignore the command
        else 
            let newJf = cmd jf
            if tank.contains newJf then tank, newJf
            else // suicide command
                if tank.hasScent jf then tank_jf  //ignore on scented point
                else tank.addScent jf, kill jf

    /// Apply sequence of commands to Tank*JF
    let runCommands (tank:Tank) (jf:JellyFish) : Command seq -> Tank*JellyFish =
        (tank, jf) |> Seq.fold applyCommand 

module IO =
    open System.IO
    open Model
    open Control

    /// parse JF string "<x> <y> <D>"
    let parseJellyFish (s:string) : JellyFish = 
        let directionMap = [| 'N', North; 'E', East; 'S', South; 'W', West |] |> Map.ofArray

        match s.Split(' ', StringSplitOptions.RemoveEmptyEntries) with
        | [| x; y; d |] -> { x = Int32.Parse(x); y = Int32.Parse(y); direction = directionMap.[d.[0]]; dead = false}
        | _ -> failwith <| "Bad jellyfish position: " + s


    /// parse tank string "<right> <top>"
    let parseTank (s:string) : Tank = 
        match s.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map Int32.Parse with
        | [| right; top |] when right>0 && top>0 ->
                    { width = right+1; height = top+1; scents = Set.empty }
        | _ -> failwith <| "Bad tank definition: " + s


    type CommandMap = Map<char, Command>

    /// default map of command chars to command functions
    let commandMap : CommandMap = 
        [|
        'L', left
        'R', right
        'F', forward
        |] |> Map.ofArray

    /// Convert string of chars to seq of commands, skipping the unknown commands
    let commandString (m:CommandMap) : string -> Command seq =
        Seq.choose m.TryFind

    /// make pairs (JF, commands) from a seq of alternating strings [JF, cmds, JF, cmds,...]
    let rec jfFromStrings (xs: string seq) = seq {
          if not <| Seq.isEmpty xs
          then
              yield  Seq.head xs |> parseJellyFish, Seq.tail xs |> Seq.head
              yield! Seq.skip 2 xs |> jfFromStrings
    }

    /// present lines from TextReader as seq<JF*string>
    /// memoize stateful sequence so elements can be accessed repeatedly
    let rec jfFromStream (r:TextReader) = Seq.cache <| seq {
        let s = r.ReadLine()
        if s <> null 
        then 
            yield (s |> parseJellyFish), r.ReadLine()
            yield! jfFromStream r
    } 


    /// run sequence of JF*commands through the given tank
    let rec runAll tank (input:seq<JellyFish*seq<Command>>) = seq {
        if not(Seq.isEmpty input)
        then
            let jf, cmds = Seq.head input
            let t1, jf1 = runCommands tank jf cmds
            yield resultString jf1
            yield! Seq.tail input |> runAll t1  // and again, with new tank scents
    }


    [<EntryPoint>]
    let main args =

        let textInput : TextReader = 
            if Seq.isEmpty args then Console.In
            else new StreamReader(args.[0]) :> TextReader

        let commandMapper = function
            | jf, cmds -> 
                jf, (commandString commandMap cmds)
    
        let tank = textInput.ReadLine() |> parseTank

        if false
        then textInput |> jfFromStream |> Seq.iter (printfn "%A")
        else 
            textInput |> jfFromStream               // shape text input into seq<JF*string>
                |> Seq.map commandMapper            // convert character commands to functions
                |> runAll tank                      // run all JF through the tank
                |> Seq.iter Console.WriteLine       // print the result
        Console.Read() |> ignore
        0
  