open System

//Start a new wordle session
let rec RunSession baseWordList =
    
    let rec SessionIter wordList =

        //Functions to test the validity of inputs
        let validWord wordList (word: string) =
            //Technically, these first two tests are redundant given the third - but they execute much faster so serve as useful sieves
            if word.Length <> WordleCode.stringLength then false
            elif Array.tryFind (fun c -> int c < 97 || int c > 122) (word.ToCharArray()) <> None then false
            elif not (Array.exists (fun x -> x = word) wordList) then false
            else true

        let validClues (clues: string) =
            if clues.Length <> WordleCode.stringLength then false
            elif Array.exists (fun c -> c <> 'g' && c <> 'y' && c <> '-') (clues.ToCharArray()) then false
            else true

        let validNextStep (nextStep: string) =
            match nextStep.[0] with
                | 'G' | 'g' | 'P' | 'p' | 'R' | 'r' | 'E' | 'e' -> true
                | _ -> false

        //Get and validate an input, reprompting the user to re-enter invalid inputs before returning
        let rec getInput (validator: string -> bool) =
            let input = Console.ReadLine()
            if validator input then input 
            else Console.WriteLine("Invalid input, please input again:")
                 getInput validator

        //Get the User's next action and move execution to perform it
        let rec getNextStep revisedList =
            
            Console.WriteLine("(G)uess another word, (P)rint valid word list, (R)estart or (E)nd?")
            let nextStep = getInput validNextStep
                
            match nextStep.[0] with
                | 'G' | 'g' -> SessionIter revisedList //Iterate the session forward with a new guess
                | 'P' | 'p' -> WordleCode.printWordList revisedList //Print the current list, and then ask again for next action
                               getNextStep revisedList
                | 'R' | 'r' -> RunSession baseWordList //Start a new session
                | 'E' | 'e' -> 0 //Return to caller, thereby ending the execution
                | _ -> failwith "Invalid step input"

        //Get the next guess and clue result from the user
        Console.WriteLine("Input guess (all lower case):")
        let guess = getInput (validWord baseWordList)

        Console.WriteLine("Input clue string (g = Green, y = Yellow, - = Grey):")
        let clueString = getInput validClues

        let revisedList = WordleCode.filterList wordList guess clueString
        Console.WriteLine("\nRevised list contains " + (Array.length revisedList).ToString() + " words.")      

        getNextStep revisedList

    //Start the iterator
    Console.WriteLine("\nStarting New Session\n")
    SessionIter baseWordList
    
[<EntryPoint>]
let main argv =
    Console.WriteLine("Wordlemancer")
    Console.WriteLine("By Oliver Ingamells\n")
    let targetDir = Environment.CurrentDirectory.ToString()
    let baseWordList = WordleCode.importWordList (targetDir + "\\WordList.txt")

    if baseWordList.IsNone then
        Console.WriteLine "Could not find source file - expected WordList.txt to be present in executable directory. Press Enter to exit."
        Console.ReadLine() |> ignore
        0
    else
        Console.WriteLine("Word List Imported with " + (Array.length baseWordList.Value).ToString() + " words")
        RunSession baseWordList.Value