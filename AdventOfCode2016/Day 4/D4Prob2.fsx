let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D4input.txt")

let targetName = "north"

let (|Integer|_|) (str: string) =
    let mutable intvalue = int 0
    if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
    else None

type CircList<'T>(_collection : seq<'T>) =
    inherit System.Collections.Generic.List<'T> (_collection)
    member l.ItemAt (n:int) = base.Item ((n%(l.Count) + l.Count)%l.Count)

let charsToString cs =
    new string(Seq.toArray cs)

let letters = CircList[for c in 'a'..'z' -> c]

let parseRoomString (str : string) =
    let lastDash = str.LastIndexOf '-'
    match str.Substring(lastDash+1).Split([|'[';']'|],System.StringSplitOptions.RemoveEmptyEntries) with
    | [|Integer sector;checksum|] -> (str.Substring(0,lastDash).Replace("-",""), sector, checksum,str.Substring(0,lastDash))

let getChecksum (str : string) =
    str |> Seq.countBy id
    |> Seq.sortWith (fun (c1,n1) (c2,n2) -> if n1 = n2 then (int)c1 - (int)c2 else n2 - n1)
    |> Seq.take 5
    |> Seq.map fst
    |> charsToString

let decodeString (str:string) sector =
    let decodeLetter c =
        match c with
        | '-' -> ' '
        | _ -> letters.ItemAt (letters |> Seq.findIndex ((=) c) |> (+) sector)
    new string(str |> Seq.map decodeLetter |> Seq.toArray)

input |> Seq.map parseRoomString
|> Seq.filter (fun (s,_,cs,_) -> getChecksum s = cs)
//|> Seq.map (fun (_,s,_,ts) -> decodeString ts s)
//|> Seq.filter (fun s -> s.Contains "north")
//|> Seq.iter (printfn "%A")
|> Seq.filter (fun (_,s,_,ts) -> (decodeString ts s).Contains targetName)
|> Seq.head |> (fun (str,sec,checks,trueStr) -> sec) |> printfn "%A"