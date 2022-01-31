[<AutoOpen>]
module WordleCode

open System
open System.IO

let stringLength = 5

type WordleClue =
    | Green of char * int // char * position
    | Yellow of char * (int list) * int // char * (list of yellow/grey positions of char) * count of G/Y clues for char
    | Grey of char * int // char * count of G/Y clues for char

//Parses the guessed word and the returned clue string into a list of WordleClues
let parseClueList (word: string) (clueString: string) =

    let parseClueLetter (clueIndex: int) =
 
        let countGYs (targetChar: char) =
            Array.fold2 (fun count char clue -> if char = targetChar && (clue = 'g' || clue = 'y') then count + 1 else count) 0 (word.ToCharArray()) (clueString.ToCharArray())
         
        let findYGreys (targetChar: char) =
            let mutable indexList = List.Empty
            for i in 0 .. (stringLength - 1) do
                if word.[i] = targetChar && (clueString.[i] = 'y' || clueString.[i] = '-') then indexList <- i :: indexList else ()
            indexList
 
        let currentClue = clueString.[clueIndex]
        let currentChar = word.[clueIndex]
        match currentClue with
            | 'g' -> Green(currentChar, clueIndex)
            | 'y' -> Yellow (currentChar, findYGreys currentChar, countGYs currentChar)
            | '-' -> Grey(currentChar, countGYs currentChar)
            | _ -> failwith "Unexpected clue type"    
            
    List.init stringLength (fun i -> parseClueLetter i)
    |> List.distinct //Greys and Yellows can create repeated clues, so trim these to keep the filter efficient
    

//Tests if word is consistent with the given clue
let applyClue (word: string) clue =

    let countChar char (word: string) =
        Array.fold (fun s c -> if c = char then s + 1 else s) 0 (word.ToCharArray())

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
    List.map (fun clue -> applyClue word clue) clues
    |> List.exists (fun x -> x = false)
    |> not

//Filter a list of words to only those that satisfy all of the clues
let filterList (wordList: string[]) (clues: WordleClue list) =
    Array.filter (fun word -> testWord word clues) wordList


//Miscellaneous functions, mostly I/O
let printWordList (wordList: string[]) =
    let (output: string) = String.concat ", " wordList
    Console.WriteLine output
    Console.Write ("\n")

let importWordList targetDir =
    let fileStream = new StreamReader(targetDir + "\\WordList.txt")
    fileStream.ReadToEnd().Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries)