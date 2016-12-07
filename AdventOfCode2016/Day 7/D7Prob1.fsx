let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D7input.txt")

open System.Text.RegularExpressions

let hypernetRegex = new Regex("\[[^\]]*\]")
let matchedStrings (r:Regex) (s : string) = r.Matches s |> Seq.cast |> Seq.map (fun m -> m.ToString())

let hasABBA (str:string) =
    let rec hasABBA' cList =
        match cList with
        | a::b::c::d::tail when a <> b && a = d && b = c -> true
        | a::b::c::d::[] -> false
        | head::tail -> hasABBA' tail
        | _ -> false
    hasABBA' (str |> Seq.toList)

let isStringValid (str:string) =
    (hypernetRegex.Split str,matchedStrings hypernetRegex str |> Seq.map (fun (s:string) -> s.Substring(1,s.Length-2)))
    |> (fun (a,b) -> (a |> Seq.exists hasABBA,b |> Seq.exists hasABBA))
    |> (=) (true,false)

input |> Seq.filter isStringValid |> Seq.length |> printfn "%A"