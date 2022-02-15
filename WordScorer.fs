[<AutoOpen>]
module WordScorer

open System

let rankWords (wordList: string[]) =
    
    let wordScore (word: string) =
        WordleCode.generateAllClues word
        |> Array.map (fun clue -> WordleCode.filterListByClues wordList word clue)
        |> Array.fold (fun sum filteredList -> sum + if filteredList.Length = 0 then 0.0 else
                                                        let p = float filteredList.Length / float wordList.Length
                                                        p * Math.Log2 (1.0 / p)) 0.0

    Array.Parallel.map (fun (word:string) -> let score = wordScore word
                                             Console.WriteLine(word + " - " + (score.ToString()))
                                             (word, score) ) wordList
    |> Array.sortByDescending (fun x -> snd x)

let printTopN n (scoreList: (string * float)[]) =
    let count = min n (Array.length scoreList)
    Console.WriteLine("\nTop " + (count.ToString()) + " suggestions:\n")

    for i in 0..(count - 1) do
        let (word, score) = scoreList.[i]
        Console.WriteLine(word + " : " + score.ToString("0.000"))

    Console.Write("\n")