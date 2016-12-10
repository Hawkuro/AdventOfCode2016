let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D7input.txt")

open System.Text.RegularExpressions

let hypernetRegex = new Regex("\[[^\]]*\]")
let matchedStrings (r:Regex) (s : string) = r.Matches s |> Seq.cast |> Seq.map (fun m -> m.ToString())

let hasABA (str:string) =
    let rec hasABA' cList =
        match cList with
        | a::b::c::tail when a <> b && a = c -> (true,(a,b)::(snd (hasABA' (b::c::tail))))
        | a::b::c::[] -> (false,[])
        | head::tail -> hasABA' tail
        | _ -> (false,[])
    hasABA' (str |> Seq.toList)

let hasBAB a b (str:string)=
    let rec hasBAB' cList =
        match cList with
        | x::y::z::tail when x = b && y = a && z = b -> true
        | x::y::z::[] -> false
        | head::tail -> hasBAB' tail
        | _ -> false
    hasBAB' (str |> Seq.toList)

let hasSSL (str:string) =
    let superNet = hypernetRegex.Split str
    let hyperNet = matchedStrings hypernetRegex str |> Seq.map (fun (s:string) -> s.Substring(1,s.Length-2))
    let superNetWithABA = superNet |> Seq.map hasABA |> Seq.filter fst
    if Seq.length superNetWithABA > 0
    then
        Seq.map snd superNetWithABA
        |> Seq.collect id
        |> Seq.exists (fun (a,b) -> hyperNet |> Seq.exists (hasBAB a b) )
    else false

input |> Seq.filter hasSSL |> Seq.length |> printfn "%A"