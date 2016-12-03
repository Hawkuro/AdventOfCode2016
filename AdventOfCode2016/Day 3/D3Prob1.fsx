let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D3input.txt")

let (|Integer|_|) (str: string) =
   let mutable intvalue = int 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let parseTriangle (str : string) =
    match str.Split([|' '|],System.StringSplitOptions.RemoveEmptyEntries) with
    | [|Integer a; Integer b; Integer c|] -> [a;b;c]
    | _ -> failwith ("failed to parse string: "+str)

let parsedInput = input |> Seq.map parseTriangle |> Seq.map List.sort |> Seq.filter (fun [a;b;c] -> a+b > c) |> Seq.length |> printfn "%A"