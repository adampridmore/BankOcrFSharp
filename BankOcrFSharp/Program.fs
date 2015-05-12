open BankOcr

[<EntryPoint>]
let main argv = 

  let text = "xxx\r\nxxx\r\nxxx"

  scanDigits text
  |> (printfn "%s")

  0 // return an integer exit code
