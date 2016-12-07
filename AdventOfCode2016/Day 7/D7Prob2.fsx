let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D7input.txt")

open System.Text.RegularExpressions

let hypernetRegex = new Regex("\[[^\]]*\]")
let matchedStrings (r:Regex) (s : string) = r.Matches s |> Seq.cast |> Seq.map (fun m -> m.ToString())

let hasABA (str:string) =
    let rec hasABA' cList =
        match cList with
        | a::b::c::tail when a <> b && a = c -> true
        | a::b::c::[] -> false
        | head::tail -> hasABA' tail
        | _ -> false
    hasABA' (str |> Seq.toList)

let isStringValid (str:string) =
    (hypernetRegex.Split str,matchedStrings hypernetRegex str |> Seq.map (fun (s:string) -> s.Substring(1,s.Length-2)))
    |> (fun (a,b) -> (a |> Seq.exists hasABA,b |> Seq.exists hasABA))
    |> (=) (true,false)

input |> Seq.filter isStringValid |> Seq.length |> printfn "%A"