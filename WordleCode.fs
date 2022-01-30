[<AutoOpen>]
module WordleCode

open System

type WordleClue =
    | Green of char * int // char * position
    | Yellow of char * (int list) * int // char * (list of yellow/grey positions of char) * count of G/Y clues for char
    | Grey of char * int // char * count of G/Y clues for char

let countChar char (word: string) =
    Array.fold (fun s c -> if c = char then s + 1 else s) 0 (word.ToCharArray())

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

