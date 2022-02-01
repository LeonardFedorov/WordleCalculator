open System

//Start a new wordle session
let rec RunSession targetDir =

    let rec SessionIter wordList =

        //Functions to test the validity of inputs
        let validWord (word: string) =
            if word.Length <> WordleCode.stringLength then false
            elif Array.tryFind (fun c -> int c < 97 || int c > 122) (word.ToCharArray()) <> None then false
            else true

        let validClues (clues: string) =
            if clues.Length <> WordleCode.stringLength then false
            elif Array.tryFind (fun c -> c <> 'g' && c <> 'y' && c <> '-') (clues.ToCharArray()) <> None then false
            else true

        //Get the next guess and clue result from the user
        Console.WriteLine("Input guess (all lower case):")
        let guess = Console.ReadLine()
        if not (validWord guess) then failwith "Word input invalid"

        Console.WriteLine("Input clue string (g = Green, y = Yellow, - = Grey):")
        let clueString = Console.ReadLine()
        if not (validClues clueString) then failwith "Clue input invalid"

        let revisedList = WordleCode.parseClueList guess clueString
                          |> WordleCode.filterList wordList
        Console.WriteLine("\nRevised list contains " + (Array.length revisedList).ToString() + " words.")

        let getNextStep =
            let rec getNextStepIter (nextStep: string) = 
                
                match nextStep.[0] with
                    | 'G' | 'g' -> nextStep.[0]
                    | 'P' | 'p' -> WordleCode.printWordList revisedList
                                   Console.WriteLine("(G)uess another word, (P)rint valid word list, (R)estart or (E)nd?")
                                   getNextStepIter (Console.ReadLine())
                    | 'R' | 'r' -> nextStep.[0]
                    | 'E' | 'e' -> nextStep.[0]
                    | _ -> Console.WriteLine("Invalid input, please input again:")
                           getNextStepIter (Console.ReadLine())

            Console.WriteLine("(G)uess another word, (P)rint valid word list, (R)estart or (E)nd?")
            getNextStepIter (Console.ReadLine())
            
        let nextStep = getNextStep
        match nextStep with
            | 'G' | 'g' -> SessionIter revisedList
            | 'R' | 'r' -> RunSession targetDir
            | 'E' | 'e' -> ()
            | _ -> failwith "Unknown user response parsed"

        Console.Write("\n")

        ()

    //Import the fresh word list and then start the iterator
    Console.WriteLine("Starting New Session")

    let wordList = WordleCode.importWordList targetDir
    Console.WriteLine("Word List Imported with " + (Array.length wordList).ToString() + " words\n")

    SessionIter wordList
    

[<EntryPoint>]
let main argv =
    Console.WriteLine("Wordle Simulator")
    let currentDir = Environment.CurrentDirectory.ToString()
    RunSession currentDir

    0 // return an integer exit code