module BankOcr

open digitConstants

let scanDigit ocrDigit = 
  match ocrDigit with
  | x when x = zero -> "0"
  | x when x = one  -> "1"
  | x when x = two -> "2"
  | x when x = three -> "3"
  | x when x = four -> "4"
  | x when x = five-> "5"
  | x when x = six -> "6"
  | x when x = seven -> "7"
  | x when x = eight -> "8"
  | x when x = nine -> "9"
  | _ -> "?"

let threeListTo3Tuple list = 
  let array = list |> List.toArray
  (array.[0],array.[1],array.[2])
  
let toLines (str:string) = 
  let split (s:string) = 
    s.Split([|System.Environment.NewLine|], System.StringSplitOptions.RemoveEmptyEntries)

  str |> split |> List.ofSeq

let toChars (string:string) =
  string.ToString().ToCharArray()

let stringToThreeCharacters text =
  text.ToString().ToCharArray()
  |> Seq.windowed 3
  |> Seq.mapi (fun i x -> (i, x)) 
  |> Seq.where (fun (i,_) -> i % 3 = 0)
  |> Seq.map (fun (_, x) -> new System.String(x))

let newLine = System.Environment.NewLine 

let joinLines lines =
  System.String.Join (newLine, lines |> Seq.toList)
  
let splitTextInto3by3digitStrings ocrText =
  let lines = ocrText 
              |> toLines 
              |> Seq.map stringToThreeCharacters
              |> Seq.toArray

  let line1 = lines.[0]
  let line2 = lines.[1]
  let line3 = lines.[2]

  Seq.zip3 line1 line2 line3
  |> Seq.map (fun (a,b,c) -> joinLines [a;b;c])

let scanDigits ocrText =
  ocrText 
  |> splitTextInto3by3digitStrings
  |> Seq.map (fun c -> newLine + c)
  |> Seq.map scanDigit
  |> Seq.reduce (+)
  
let numberStringToInts (text:string) : int seq =
  text.ToCharArray()
  |> Seq.map(fun c -> c.ToString())
  |> Seq.map (fun c -> System.Int32.Parse(c))

let containsInvalidChars (number:string) =
  number.ToCharArray() 
  |> Seq.exists (fun c -> c = '?')

type status = |Valid|Invalid|Illegal

let getNumberStatus (number:string) = 
  let calculateChecksum number = 
    number 
    |> numberStringToInts 
    |> Seq.mapi (fun i n -> ( 9-i) * n)
    |> Seq.sum
    |> (function x -> x%11)

  if containsInvalidChars number then Illegal
  else  match calculateChecksum number with
        | 0 -> Valid
        | _ -> Invalid

let processNumberWithStatus number = 
  match getNumberStatus number with
  | Valid -> number
  | Invalid -> sprintf "%s ERR" number
  | Illegal -> sprintf "%s ILL" number
  