let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D8input.txt")

let charsToString cs =
    new string(Seq.toArray cs)

type CircList<'T>(_collection : seq<'T>) =
    inherit System.Collections.Generic.List<'T> (_collection)
    member l.ItemAt (n:int) = base.Item ((n%(l.Count) + l.Count)%l.Count)

let (|Integer|_|) (str: string) =
   let mutable intvalue = int 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let shiftRow row shift (display: bool list list) =
    let shiftedRow = CircList display.[row]
    [for i in 0..(List.length display-1) ->
        match i with
        | a when a = row -> [for j in 0..(List.length display.[i]-1) -> shiftedRow.ItemAt (j-shift)]
        | _ -> display.[i]
    ]

let shiftColumn column shift (display: bool list list) = 
    let shiftedColumn = CircList (display |> List.map (List.item column))
    [for i in 0..(List.length display-1) -> 
        [for j in 0..(List.length display.[i]-1) -> 
            match j with
            | a when a=column -> shiftedColumn.ItemAt (i-shift)
            | _ -> display.[i].[j]
        ]
    ]

let turnOnRect x y (display: bool list list) = 
    [for i in 0..(List.length display-1) -> 
        [for j in 0..(List.length display.[i]-1) -> 
            match (i,j) with
            | (a,b) when b < x && a < y -> true
            | _ -> display.[i].[j]
        ]
    ]

let parseStringAndApply (display : bool list list) (str: string) = 
    match str.Split([|' ';'=';'x';'y'|],System.StringSplitOptions.RemoveEmptyEntries) with
    | [|"rect";Integer x;Integer y|] -> turnOnRect x y display
    | [|"rotate";"row";Integer row;_;Integer shift|] -> shiftRow row shift display
    | [|"rotate";"column";Integer column;_; Integer shift|] -> shiftColumn column shift display
    | _ -> failwith ("could not parse string: "+str+String.concat "|" (str.Split([|' ';'=';'x';'y'|],System.StringSplitOptions.RemoveEmptyEntries)|> Array.toSeq))

input |> Seq.fold parseStringAndApply [for i in 1..6 -> [for j in 1..50 -> false]] |> Seq.map ((Seq.map (function | true -> '#' | false -> ' '))>>charsToString) |> String.concat "\n" |> printfn "%s"