let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D3input.txt")

let (|Integer|_|) (str: string) =
   let mutable intvalue = int 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let parseTriangle (str : string) =
    match str.Split([|' '|],System.StringSplitOptions.RemoveEmptyEntries) with
    | [|Integer a; Integer b; Integer c|] -> [a;b;c]
    | _ -> failwith ("failed to parse string: "+str)

let parsedInput = input |> Seq.map parseTriangle |> Seq.toList

let flipTriplets triangleList =
    let rec flipTriplets' triangleList out =
        match triangleList with
        | [] -> out
        // a1 b1 c1    a1 a2 a3
        // a2 b2 c3 -> b1 b2 b3
        // a3 b3 c3    c1 c2 c3
        | [a1;b1;c1]::[a2;b2;c2]::[a3;b3;c3]::tail -> flipTriplets' tail ([a1;a2;a3]::[b1;b2;b3]::[c1;c2;c3]::out)
        | _ -> failwith ""
    flipTriplets' triangleList []

parsedInput |> flipTriplets |> Seq.map List.sort |> Seq.filter (fun [a;b;c] -> a+b > c) |> Seq.length |> printfn "%A"