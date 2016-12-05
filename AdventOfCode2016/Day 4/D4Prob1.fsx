let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D4input.txt")
let (|Integer|_|) (str: string) =
    let mutable intvalue = int 0
    if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
    else None

let parseRoomString (str : string) =
    let lastDash = str.LastIndexOf '-'
    match str.Substring(lastDash+1).Split([|'[';']'|],System.StringSplitOptions.RemoveEmptyEntries) with
    | [|Integer sector;checksum|] -> (str.Substring(0,lastDash).Replace("-",""), sector, checksum)

let getChecksum (str : string) =
    let charsToString cs =
        new string(Seq.toArray cs)
    str |> Seq.countBy id
    |> Seq.sortWith (fun (c1,n1) (c2,n2) -> if n1 = n2 then (int)c1 - (int)c2 else n2 - n1)
    |> Seq.take 5
    |> Seq.map fst
    |> charsToString

input |> Seq.map parseRoomString |> Seq.filter (fun (s,_,cs) -> getChecksum s = cs) |> Seq.map (fun (_,s,_) -> s) |> Seq.sum |> printfn "%A"