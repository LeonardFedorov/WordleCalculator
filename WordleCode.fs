[<AutoOpen>]
module WordleCode

open System
open System.IO

type WordleClue =
    | Green of char * int // char * position
    | Yellow of char * (int list) * int // char * (list of yellow/grey positions of char) * count of G/Y clues for char
    | Grey of char * int // char * count of G/Y clues for char

let countChar char (word: string) =
    Array.fold (fun s c -> if c = char then s + 1 else s) 0 (word.ToCharArray())

//Tests if word is consistent with the given clue
let applyClue (word: string) clue =
    match clue with
        | Green(c, i) -> if word.[i] = c then true else false
        | Grey(c, n) -> if countChar c word = n then true else false
        | Yellow(c, posList, n) -> if
                                      List.fold (fun s i -> word.[i] <> c && s) true posList && 
                                      countChar c word >= n
                                   then
                                      true
                                   else 
                                      false


//Tests if a word is consistent with all of a given list of clues
let testWord (word: string) (clues: WordleClue list) =
    List.map (fun clue -> applyClue word clue ) clues
    |> List.exists (fun x -> x = false)
    |> not

//Filter a list of words to only those that satisfy all of the clues
let filterList (wordList: string[]) (clues: WordleClue list) =
    Array.filter (fun word -> testWord word clues) wordList

let printWordList (wordList: string[]) =
    let (output: string) = String.concat ", " wordList
    Console.WriteLine output

let importWordList targetDir =
    let fileStream = new StreamReader(targetDir + "\\WordList.txt")
    fileStream.ReadToEnd().Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries)