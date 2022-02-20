[<AutoOpen>]
module WordScorer

open System

let wordCompare (guess: string) (solution: string) =
    //Initialise the result as all greys
    let mutable clueString = Array.create 5 '-'

    //Find all character matches, i.e. greens
    for i in 0..4 do
        if guess.[i] = solution.[i] then clueString.[i] <- 'g'
    
    //Now look for yellows
    for i in 0..4 do
        if guess.[i] <> solution.[i] then
            let nonGCharCount = Array.fold2 (fun count char clue -> if char = guess.[i] && clue <> 'g' then count + 1 else count) 0 (solution.ToCharArray()) clueString
            if nonGCharCount > 0 then
                if i = 0 then
                    clueString.[i] <- 'y'
                else
                    let mutable yCount = 0
                    for j in 0..(i-1) do
                        if solution.[j] = guess.[i] && clueString.[j] <> 'g' then
                            yCount <- yCount + 1
                    if yCount <= nonGCharCount then
                        clueString.[i] <- 'y'
    
    String(clueString)

//Calculate the score of every word in the wordList and return a ranked list with the highest score first
let rankWords (wordList: string[]) =
    
    //Associate each char with a number so that we can represent clueStrings as base 3 numbers
    let charToNum c =
        match c with
            | 'g' -> 0
            | 'y' -> 1
            | '-' -> 2
            | _ -> failwith "Unexpected char passed to number conversion"   
    
    let clueNumber (clueString: string) =
        Array.fold (fun s char -> 3*s + charToNum char) 0 (clueString.ToCharArray())

    let wordScore (word: string) =
        //Create an array to keep a tally of how many times we see each clue
        //Clues are mapped to indexes using the clueNumber function
        let mutable clueCounts = Array.create (pown 3 WordleCode.stringLength) 0
        Array.iter (fun candidate -> let clueNumber = clueNumber (wordCompare word candidate)
                                     clueCounts.[clueNumber] <- clueCounts.[clueNumber] + 1
                   ) wordList

        //Calculate the total score by summing the weighted uncertainty reduction across all clue values
        Array.fold (fun score value -> if value = 0 then score 
                                       else let p = (float value / float wordList.Length) 
                                            score + p*Math.Log2(1.0/p)
                   ) 0.0 clueCounts

    
    Array.Parallel.map (fun (word:string) -> let score = wordScore word
                                             //Console.WriteLine(word + " - " + (score.ToString()))
                                             (word, score) ) wordList
    |> Array.sortByDescending (fun x -> snd x)

let printTopN n (scoreList: (string * float)[]) =
    let count = min n (Array.length scoreList)
    Console.WriteLine("\nTop " + (count.ToString()) + " suggestions:\n")

    for i in 0..(count - 1) do
        let (word, score) = scoreList.[i]
        Console.WriteLine(word + " : " + score.ToString("0.000"))

    Console.Write("\n")


