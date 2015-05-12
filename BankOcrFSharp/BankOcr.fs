module BankOcr

open NUnit.Framework
open FsUnit
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
   
[<Test>]
let ``scan multiple digits``()=
  let text = @"
    _ 
 |  _|
 | |_ "

  scanDigits text |> should equal "12"

[<Test>]
let ``scan 0-9 text``()=
  let text = @"
 _     _  _     _  _  _  _  _ 
| | |  _| _||_||_ |_   ||_||_|
|_| | |_  _|  | _||_|  ||_| _|"

  scanDigits text |> should equal "0123456789"

[<Test>]
let ``text to 3 character strings``()=
  "123456" |> stringToThreeCharacters|> should equal ["123";"456"]


[<Test>]
let ``split text into 3 by 3 digit strings``()=
  let text = @"
abcjkl
defmno
ghipqr"

  text |> splitTextInto3by3digitStrings |> should equal ["abc\r\ndef\r\nghi";"jkl\r\nmno\r\npqr"]


[<Test>]
let ``scratch``()=
  let text12 = @"
    _ 
 |  _|
 | |_ "

  let text1 = @"
   
 | 
 | "

  let text = @"
ABCDEF
GHIJKL
MNOPQR"
  text12 |> scanDigits |> (printfn "%s %A" text12)
  text1 |> scanDigits |> (printfn "%s %A" text1)
  text |> scanDigits |> (printfn "%s %A" text)

[<Test>]
let ``join lines``()=
  let expected = @"ab
cd"

  ["ab";"cd"] |> joinLines |> should equal expected

[<Test>]
let ``abc to chars``()=
  "abc" |> toChars |> should equal ['a';'b';'c']

[<Test>]
let ``three list to three tuple``()=
  ["a";"b";"c"] |> threeListTo3Tuple |> should equal ("a", "b", "c")


[<Test>]
let ``scan a 1 digit`` () =
  one |> scanDigit |> should equal "1"

[<Test>]
let ``scan a 2 digit`` () =
  two |> scanDigit |> should equal "2"

[<Test>]
let ``scan a 3 digit`` () =
  three|> scanDigit |> should equal "3"