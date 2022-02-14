[<AutoOpen>]
module WordScorer

open System

let clueList =
    let numToChar n = 
        match n with
            | 0 -> 'g'
            | 1 -> 'y'
            | 2 -> '-'
            | _ -> failwith "Unexpected number passed to char conversion"

    let mutable (result: string[]) = Array.create (pown 3 5) "0"
    let mutable counter = 0
    for i0 in 0..2 do
        for i1 in 0..2 do
            for i2 in 0..2 do
                for i3 in 0..2 do
                    for i4 in 0..2 do
                        result.[counter] <- String([|numToChar i0 ; numToChar i1 ; numToChar i2 ; numToChar i3 ; numToChar i4|])
                        counter <- counter + 1
    result

let rankWords (wordList: string[]) =
    
    let wordScore (word: string) =
        Array.map (fun clue -> WordleCode.filterList wordList word clue) clueList
        |> Array.map (fun filteredList -> if filteredList.Length = 0 then 0.0 else
                                          let p = float filteredList.Length / float wordList.Length
                                          p * Math.Log2 (1.0 / p))
        |> Array.sum

    Array.Parallel.map (fun (word:string) -> let score = wordScore word
                                             Console.WriteLine(word + " - " + (score.ToString()))
                                             (word, score) ) wordList
    |> Array.sortByDescending (fun x -> snd x)

let printTopN n (scoreList: (string * float)[]) =
    let count = min n (Array.length scoreList)
    Console.WriteLine("\nTop " + (count.ToString()) + " suggestions:\n")

    for i in 0..(count - 1) do
        let (word, score) = scoreList.[i]
        Console.WriteLine(word + " : " + score.ToString())

    Console.Write("\n")