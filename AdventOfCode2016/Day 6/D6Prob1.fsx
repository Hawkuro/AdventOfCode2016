let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D6input.txt")

let rec transpose = function
    | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []

let charsToString cs =
    new string(Seq.toArray cs)

input |> Array.map Seq.toList |> Array.toList |> transpose |> List.map (List.countBy id >> List.maxBy snd) |> List.map fst |> charsToString |> printfn "%A"