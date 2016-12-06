open System.Security.Cryptography
open System.Text

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

let ans = Seq.map (md5Check input) nStar |> Seq.filter fst |> Seq.map snd |> Seq.take 8 |> Seq.map (fun s -> s.Chars 5)

printfn "%s" (new string(Seq.toArray ans))