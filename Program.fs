open System
open System
open System.Security.Claims
open System
 
type typeOfShip = Battleship | Destroyer1 | Destroyer2
type orientation = Vertical | Horizontal

let numberOfBattleShips = 1
let numberOfDestoyers = 2
let battleshipLength = 5
let destroyerLength = 4
let gridWidth = 10
let gridHeight = 10
let arrSunkShips = [false;false;false]
let random = new System.Random()
let mutable boolhit=false
let mutable positionGuess = 0,0
let listRandomPos  =
    let rnd = System.Random()
    let pos = Seq.initInfinite (fun _ -> random.Next (0, 10),random.Next (0, 10) ) 
    pos

    |> Seq.distinct
    |> Seq.take(3)
    |> Seq.toList

 
let mutable rndBattleshipPos, rndDestroyer1Pos, rndDestroyer2Pos =listRandomPos.[0],listRandomPos.[1],listRandomPos.[2]
let mutable battleshipPos, destroyer1Pos, destroyer2Pos = (0,0), (0,0), (0,0)
let xBattleshipPos,yBattleShipPos = rndBattleshipPos
let xDestroyer1Pos,yDestroyer1Pos = rndDestroyer1Pos
let xDestroyer2Pos,yDestroyer2Pos = rndDestroyer2Pos
let mutable xNewBattleshipPos,yNewBattleshipPos = rndBattleshipPos
let mutable xNewDestroyer1Pos,yNewDestroyer1Pos = rndBattleshipPos
let mutable xNewDestroyer2Pos,yNewDestroyer2Pos = rndBattleshipPos

let ListBattleshipPositions =ResizeArray<(int*int )>()
let ListDestroyer1Positions =ResizeArray<(int*int )>()
let ListDestroyer2Positions =ResizeArray<(int*int )>()
let listPositions =ResizeArray<(int*int )>()

let battleshipOrienatation = if xBattleshipPos + battleshipLength < gridWidth then Vertical else Horizontal
let destroyer1Orientation = if xDestroyer1Pos + destroyerLength < gridWidth then Vertical else Horizontal
let destroyer2Orientation = if xDestroyer2Pos + destroyerLength < gridWidth then Vertical else Horizontal
 
 

let checkPosition (coordinates,shipType) =  
 match coordinates,  shipType with

 //check to see if square is used
| (x, y),  Battleship ->  if listPositions.Contains(coordinates)  = false && ListBattleshipPositions.Count <battleshipLength = true then
                                                                              ListBattleshipPositions.Add(coordinates)  
                                                                              listPositions.Add(coordinates)                                                   
| (x, y),  Destroyer1 ->  if listPositions.Contains(coordinates) = false && ListDestroyer1Positions.Count <destroyerLength = true then
                                                                              ListDestroyer1Positions.Add(coordinates)
                                                                              listPositions.Add(coordinates)  
| (x, y),  Destroyer2 ->  if listPositions.Contains(coordinates) = false  && ListDestroyer2Positions.Count <destroyerLength = true then
                                                                              ListDestroyer2Positions.Add(coordinates)
                                                                              listPositions.Add(coordinates)  
 

while ((listPositions.Count-1) <=((destroyerLength+numberOfDestoyers)+(battleshipLength*numberOfBattleShips))) do 

//incriments x or y position based on oritentation. Tried with random number but this seems to be faster
   xNewBattleshipPos <-   if (xNewBattleshipPos+1)<10 = true then (xNewBattleshipPos+1) else (xNewBattleshipPos-1)
   yNewBattleshipPos<- if (yNewBattleshipPos+1)<10 = true then (yNewBattleshipPos+1) else (yNewBattleshipPos-1)
   xNewDestroyer1Pos <-  if (xNewDestroyer1Pos+1)<10 = true then (xNewDestroyer1Pos+1) else (xNewDestroyer1Pos-1)
   yNewDestroyer1Pos <-  if (yNewDestroyer1Pos+1)<10 = true then (yNewDestroyer1Pos+1) else (yNewDestroyer1Pos-1)
   xNewDestroyer2Pos <-  if (xNewDestroyer2Pos+1)<10 = true then (xNewDestroyer2Pos+1) else (xNewDestroyer2Pos-1)
   yNewDestroyer2Pos <-  if (yNewDestroyer2Pos+1)<10 = true then (yNewDestroyer2Pos+1) else (yNewDestroyer2Pos-1)
   battleshipPos <- if  battleshipOrienatation =  Horizontal then(xNewBattleshipPos, yBattleShipPos) else (xBattleshipPos, yNewBattleshipPos) 
   destroyer1Pos <- if  destroyer1Orientation =   Horizontal then (xNewDestroyer1Pos, yDestroyer1Pos) else (xDestroyer1Pos, yNewDestroyer1Pos)
   destroyer2Pos <- if   destroyer1Orientation =   Horizontal then (xNewDestroyer2Pos, yDestroyer2Pos) else (xDestroyer2Pos, yNewDestroyer2Pos)
   checkPosition(battleshipPos,Battleship)
   checkPosition(destroyer1Pos,Destroyer1)
   checkPosition(destroyer2Pos,Destroyer2)

done

 
//for word in ListBattleshipPositions do
//    printf "%s" (word.ToString()) //  

//for word in ListDestroyer1Positions do
//    printf "%s" (word.ToString()) // 
//for word in ListDestroyer2Positions do
//    printf "%s" (word.ToString()) // 



let inputValidate (inputO: string option) =
    match inputO with
    | None -> None
    | Some input ->
        match input |> String.IsNullOrWhiteSpace with
        | true -> None
        | false ->
            match input.[0..0] |> "ABCDEFGHIJ".Contains,input.[1..] |> Int32.TryParse |> snd < 11,input.[1..] |> Int32.TryParse |> snd > 0 with
            | true, true, true -> Some input
            | _, _, _ -> None
 

 
  
 
let coordsTranslate (str:string option) =
    match str with
    | None -> None
    | Some coords -> Some((coords.[0] |> int) - 65, (coords.[1..] |> int) - 1)
    //This code isnt mine
      

      
let checkSunk (shipType) =  
    match shipType with

 //check to see if ship is sunk
|   Battleship ->  if ListBattleshipPositions.Count=0 = true then printfn " you have sunk my battleship!"            
|   Destroyer1 ->  if ListBattleshipPositions.Count=0 = true then printfn " you have sunk my destroyer!"
|   Destroyer2 ->  if ListDestroyer2Positions.Count=0 = true then printfn " you have sunk my destroyer2!"
 

 






let CheckHit (coordEntered: (int*int) option) =
//checks if coordinates are hit/missm, m,  
    boolhit <-false
    match coordEntered with
    | None -> coordEntered |> ignore
    | Some (x,y) ->
      
 
    
   
    let CoordGuessed = coordEntered.Value
    printfn "%s"(CoordGuessed.ToString())

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
             
while ListDestroyer2Positions.Count+ListDestroyer1Positions.Count+ListBattleshipPositions.Count > 0 do
 

  Some (Console.ReadLine().ToUpper()) |> inputValidate |> coordsTranslate |> CheckHit
printfn "You have sunk all my ships. you monster."

Console.ReadLine() |> ignore
    