namespace Solved.FSharp

//rock paper scissors

module RockPaperScissors =    
    let aiMove (m:string list)(loser:bool) =
        let h = m |> Seq.ofList |> Seq.groupBy (fun a -> a) |> Seq.sortBy(fun a-> snd a|> Seq.length)
        //snd h |> Seq.iter(fun a -> printf "moves %O" a)
        if loser then
            h |> Seq.skip 1 |> Seq.take 1 |> Seq.head |> fst
        else
            h |> Seq.head |> fst

    let playRPS = 
        let rec promptUser (moves:string list) = 
            printf "Make a move\n"
            let input = (System.Console.ReadLine(),aiMove moves false)            
            match input with
                | ("R","R") -> printf "tie"; promptUser ((fst input)::moves)
                | ("R","S") -> printf "You win! %O" input; promptUser ((fst input)::moves)
                | ("R","P") -> printf "You lose! %O" input; promptUser ((fst input)::moves)
                | ("S","S") -> printf "tie"; promptUser ((fst input)::moves)
                | ("S","R") -> printf "You lose! %O" input; promptUser ((fst input)::moves)
                | ("S","P") -> printf "You win! %O" input; promptUser ((fst input)::moves)
                | ("P","P") -> printf "tie"; promptUser ((fst input)::moves)
                | ("P","R") -> printf "You win! %O" input; promptUser ((fst input)::moves)
                | ("P","S") -> printf "You lose! %O" input; promptUser ((fst input)::moves)  
                | _ -> printf "Bad move"; promptUser moves
            ()        
        promptUser ["R";"P";"S"]
        ()
    