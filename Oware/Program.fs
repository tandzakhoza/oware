module Oware

type StartingPosition =
    | South
    | North

type states=
|Southturn
|Northturn
|Win of string 
|Draw of string 



type Player = { 
    houses: int*int*int*int*int*int
    storehouse: int
    }

type board = { 
    state: states
    northplayer: Player
    southplayer: Player
    }


let getSeeds n board = // failwith "Not implemented"
    match board.state with
    |Northturn | Southturn -> 
        let (a',b',c',d',e',f')= board.northplayer.houses 
        let (a,b,c,d,e,f) = board.southplayer.houses in
            match n with 
            |1->a
            |2->b
            |3->c
            |4->d
            |5->e
            |6->f  
            |7->a'
            |8->b'
            |9->c'
            |10->d'
            |11->e'
            |12->f'
            |_-> failwith "Not implemented"
    |_-> failwith "Not implemented"

//useHouse, which accepts a house number and a board,
//and makes a move using
//that house.

let checkhouse currhouse =
          match currhouse with
          | 13 -> 1
          | _ -> currhouse

//let rec nextHouse n =
//          match n  with 
//          |12 -> 1
//          | n -> n+1 


let setseeds n num board =
     let (a',b',c',d',e',f') = board.northplayer.houses 
     let (a ,b ,c ,d ,e ,f ) = board.southplayer.houses in
         match n with
         |1-> {board with southplayer={board.southplayer with houses=(num,b,c,d,e,f)}}
         |2->{board with southplayer={board.southplayer with houses=(a,num,c,d,e,f)}}
         |3->{board with southplayer={board.southplayer with houses=(a,b,num,d,e,f)}}
         |4->{board with southplayer={board.southplayer with houses=(a,b,c,num,e,f)}}
         |5->{board with southplayer={board.southplayer with houses=(a,b,c,d,num,f)}}
         |6->{board with southplayer={board.southplayer with houses=(a,b,c,d,e,num)}}
         |7->{board with northplayer={board.northplayer with houses=(num,b',c',d',e',f')}}
         |8->{board with northplayer={board.northplayer with houses=(a',num,c',d',e',f')}}
         |9->{board with northplayer={board.northplayer with houses=(a',b',num,d',e',f')}}
         |10->{board with northplayer={board.northplayer with houses=(a',b',c',num,e',f')}}
         |11->{board with northplayer={board.northplayer with houses=(a',b',c',d',num,f')}}
         |12->{board with northplayer={board.northplayer with houses=(a',b',c',d',e',num)}}
         |_-> failwith "Not implemented"

let  capture n board=
        let seedinhouse= getSeeds n board
        let south = board.southplayer.storehouse
        let north = board.northplayer.storehouse
        let position = board.state
        match position,seedinhouse with
        |Southturn,(3|2) -> 
            let nboard={board with southplayer={board.southplayer with storehouse=(south+n)}}
            let board={nboard with northplayer={nboard.northplayer with storehouse=(north)}}
            setseeds n 0 board
        |Northturn,(3|2) -> 
            let nboard={board with southplayer={board.southplayer with storehouse=(south)}}
            let board={nboard with northplayer={nboard.northplayer with storehouse=(north+n)}}
            setseeds n 0 board
        |_-> board
 
let changestate position board =
    match position with 
    |Southturn -> {board with state= Northturn}
    |Northturn -> {board with state = Southturn}
    |_-> failwith "uh oh"


let useHouse n board =
    let (a,b,c,d,e,f)= board.southplayer.houses
    let (a',b',c',d',e',f')= board.northplayer.houses
    let numseeds = getSeeds n board
    let newboard = setseeds n 0 board
    let nexthouse= n+1
    let position = board.state

    let rec updateboard numseeds1 nexthouse1 nboard =
        let currenthouse = checkhouse nexthouse1
        let nseeds= getSeeds currenthouse nboard
        
        match numseeds1>0 with
        |true->
            let board = setseeds currenthouse (nseeds+1) nboard
            let currboard = changestate position board
            updateboard (numseeds1-1) (currenthouse+1) currboard 
        |_->nboard
        
    updateboard numseeds nexthouse newboard

//failwith "Not implemented"

let start position = 
    let intialboard= {houses=(4,4,4,4,4,4); storehouse=0}
    match position with 
    |South-> {state= Southturn; southplayer= intialboard; northplayer= intialboard}
    |North-> {state= Northturn; northplayer= intialboard; southplayer= intialboard}
//failwith "Not implemented"



let score board = //failwith "Not implemented
    let south=board.southplayer.storehouse
    let north= board.northplayer.storehouse
    match (south, north) with 
    |24,24 -> 
        let nboard = {board with state = Draw "Game ended in a draw"}
        (south,north)
    |_,24 -> 
        let nboard = {board with state = Win "North won"}
        (south,north)
    |24,_ -> 
        let nboard = {board with state = Win "South won"}
        (south,north)
    |_-> (south,north)


let gameState board = //failwith "Not implemented"
    //intial position/state, using usehouse declares the house 
    match board.state with 
    |Southturn -> "South's turn"
    |Northturn-> "North's turn"
    |Draw a-> a
   // |Win a-> a
    |_-> failwith "uh oh"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
