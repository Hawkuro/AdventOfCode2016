open System.Security.Cryptography
open System.Text

let (|Integer|_|) (str: string) =
    let mutable intvalue = int 0
    if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
    else None

let charsToString cs =
    new string(Seq.toArray cs)

let input = "ugkcyxxp"

let md5 (data : byte array) : string =
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

let md5Check str (i : int) =
    let hash = (md5 (str + i.ToString() |> System.Text.Encoding.ASCII.GetBytes))
    (hash.StartsWith("00000"),hash)

let nStar = Seq.initInfinite id

let validHashes = Seq.map (md5Check input) nStar |> Seq.filter fst |> Seq.map snd
let importantBits =
    validHashes
    |> Seq.map (fun hash -> (hash.Substring(5,1),hash.Chars 6))
    |> Seq.choose (fun (ind,c) -> 
        match ind with
        | Integer i when i <= 7 -> Some(i,c)
        | _ -> None)

let findPassword tupleSeq =
    let rec findPassword' (tSeq:seq<int*char>) (out:Map<int,char>) =
        if out.Count = 8 then out else
            let (ind,c) = Seq.head tSeq
            if out.ContainsKey ind
            then findPassword' (Seq.tail tSeq) out
            else
                printfn "%A" (ind, c)
                findPassword' (Seq.tail tSeq) (out.Add (ind,c))
    findPassword' tupleSeq Map.empty |> Map.toList |> List.sortBy fst |> List.map snd |>  charsToString

findPassword importantBits |> printfn "%A"