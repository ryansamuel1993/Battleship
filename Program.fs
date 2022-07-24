open System
 
let numberOfBattleShips = 1
let numberOfDestoyers = 2
let battleshipLength = 5
let destroyerLength = 4
let gridWidth = 20
let gridHeight = 20
let arrSunkShips = [false;false;false]
let random = new System.Random()
let mutable boolhit=false
let mutable positionGuess = 0,0

type typeOfShip = Battleship | Destroyer1 | Destroyer2
type orientation = Vertical | Horizontal
 
let move orientation (x,y) : int * int = 
    match orientation with
    | Vertical when x + 1 < gridHeight -> x + 1, y
    | Vertical when x - 1 > 0 -> x - 1, y
    | Horizontal when y + 1 < gridWidth ->  x, y + 1
    | Horizontal when y - 1 > 0 -> x, y - 1
    | _ -> x, y

let mutable listPossiblePositions  =
    let xPos =  [1..gridWidth]
    let yPos =  [1..gridHeight]
    let pos = xPos |> List.collect (fun x -> yPos |> List.map (fun y -> (x, y)))
    pos 

let mutable listRandomPos =
    listPossiblePositions
    |> Seq.sortBy(fun _ -> random.Next(1, listPossiblePositions.Length))
    |> Seq.distinct
    |> Seq.take(3)
    |> Seq.toList

let mutable battleshipPos, destroyer1Pos, destroyer2Pos = listRandomPos.[0], listRandomPos.[1], listRandomPos.[2]
let mutable readyInfo = false
let mutable reattempt = 0
let ListBattleshipPositions =ResizeArray<(int*int )>()
let ListDestroyer1Positions =ResizeArray<(int*int )>()
let ListDestroyer2Positions =ResizeArray<(int*int )>()
let listPositions = ResizeArray<(int*int )>()

let battleshipOrienatation = Horizontal
let destroyer1Orientation = Horizontal
let destroyer2Orientation = Horizontal
 
let checkRestart () =
    if reattempt = 10  then
        listPossiblePositions
        |> List.sortBy(fun _ -> random.Next(1, listPossiblePositions.Length))
        |> Seq.distinct
        |> Seq.take(3) 
        |> Seq.toList
        |> function v -> 

                    if ListBattleshipPositions.Count < battleshipLength then 
                        
                        battleshipPos <- v.[0]
                        for i in ListBattleshipPositions do 
                            listPositions.Remove(i) |> ignore

                        ListBattleshipPositions.Clear()

                    if ListDestroyer1Positions.Count < destroyerLength then 
                        destroyer1Pos <- v.[1]
                        for i in ListDestroyer1Positions do 
                            listPositions.Remove(i) |> ignore

                        ListDestroyer1Positions.Clear()
                    
                    if ListDestroyer2Positions.Count < destroyerLength then
                        destroyer2Pos <- v.[2]
                        printfn "Reset destroyer2Pos"
                        for i in ListDestroyer2Positions do 
                            listPositions.Remove(i) |> ignore

                        ListDestroyer2Positions.Clear()
                    
                    
                    reattempt <- 0
        
    else reattempt <- reattempt + 1

let checkPosition (coordinates, shipType) =  
    
    match coordinates, shipType with
    | (x, y),  Battleship ->  if listPositions.Contains(coordinates) |> not && ListBattleshipPositions.Count <battleshipLength then
                                                                                  ListBattleshipPositions.Add(coordinates)  
                                                                                  listPositions.Add(coordinates)   
    | (x, y),  Destroyer1 ->  if listPositions.Contains(coordinates) |> not && ListDestroyer1Positions.Count < destroyerLength then
                                                                                  ListDestroyer1Positions.Add(coordinates)
                                                                                  listPositions.Add(coordinates) 

    | (x, y),  Destroyer2 ->  if listPositions.Contains(coordinates) |> not && ListDestroyer2Positions.Count < destroyerLength then
                                                                                  ListDestroyer2Positions.Add(coordinates)
                                                                                  listPositions.Add(coordinates) 
 
let checkReady () = 
    if ((listPositions.Count) = ((destroyerLength*numberOfDestoyers) + (battleshipLength*numberOfBattleShips))) then
        printfn "Ready"
        readyInfo <- true

while ((listPositions.Count) < ((destroyerLength*numberOfDestoyers) + (battleshipLength*numberOfBattleShips))) do 

   
   //incriments x or y position based on oritentation. As there is potentail for duplicate randoms on a smaller board, we restart after x amount of attempts
   destroyer1Pos <- move destroyer1Orientation destroyer1Pos
   destroyer2Pos <- move destroyer2Orientation destroyer2Pos
   battleshipPos <- move battleshipOrienatation battleshipPos

   checkPosition(battleshipPos, Battleship)
   checkPosition(destroyer1Pos, Destroyer1)
   checkPosition(destroyer2Pos, Destroyer2)
   
   checkRestart()
   checkReady()
 
done


let inputValidate (inputO: string option) =
    let  alphabet = ['A' .. 'Z'] |> List.take gridWidth |> string
    match inputO with
    | None -> None
    | Some input ->
        match input |> String.IsNullOrWhiteSpace with
        | true -> None
        | false ->
            match input.[0..0] |> alphabet.Contains,input.[1..] |> Int32.TryParse |> snd < 11,input.[1..] |> Int32.TryParse |> snd > 0 with
            | true, true, true -> Some input
            | _, _, _ -> None
 
 
let coordsTranslate (str:string option) =
    match str with
    | None -> None
    | Some coords -> Some((coords.[0] |> int) - 65, (coords.[1..] |> int) - 1)
      
let checkSunk (shipType) =  
     
     //check to see if ship is sunk
     match shipType with
     |   Battleship ->  if ListBattleshipPositions.Count=0 = true then printfn " you have sunk my battleship!"            
     |   Destroyer1 ->  if ListBattleshipPositions.Count=0 = true then printfn " you have sunk my destroyer!"
     |   Destroyer2 ->  if ListDestroyer2Positions.Count=0 = true then printfn " you have sunk my destroyer2!"
  

let checkHit (coordEntered: (int*int) option) =
//checks if coordinates are hit/missm, m,  
    boolhit <-false
    match coordEntered with
    | None -> coordEntered |> ignore
    | Some (x,y) ->

        
        let CoordGuessed = coordEntered.Value
        printfn "%s"(coordEntered.Value.ToString())

        if ListDestroyer1Positions.Contains(CoordGuessed) then 
            boolhit<-true
            ListDestroyer1Positions.Remove(CoordGuessed) |> ignore
            printfn " You have hit my destroyer1!"

            checkSunk(Destroyer1)

        if ListDestroyer2Positions.Contains(CoordGuessed) then
             boolhit<-true
             ListDestroyer2Positions.Remove(CoordGuessed) |> ignore
             printfn " You have hit my destroyer2!"
             checkSunk(Destroyer2)

        if ListBattleshipPositions.Contains(CoordGuessed)  then
             boolhit<-true
             ListBattleshipPositions.Remove(CoordGuessed) |> ignore
             printfn " You have hit my Battleship!"
             checkSunk(Battleship)

        if  boolhit = false then
            printfn "Miss!"
    

Console.ReadLine() |> ignore

            
while ListDestroyer2Positions.Count + ListDestroyer1Positions.Count + ListBattleshipPositions.Count > 0 do
  Some (Console.ReadLine().ToUpper()) |> inputValidate |> coordsTranslate |> checkHit 

printfn "You have sunk all my ships. you monster."

Console.ReadLine() |> ignore
    