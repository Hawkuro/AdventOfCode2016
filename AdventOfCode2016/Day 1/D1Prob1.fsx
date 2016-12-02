open System
let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__+ @"\D1input.txt")

let (|Integer|_|) (str: string) =
   let mutable intvalue = int 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

type Pair(x:int,y:int) =
    member this.x = x
    member this.y = y
    static member (+) (a:Pair,b:Pair) = (Pair((a.x + b.x), (a.y + b.y)))
    static member (*) (a:Pair,m:int) = (Pair(a.x*m,a.y*m))
    static member (*) (m:int,a:Pair) = (Pair(a.x*m,a.y*m))
    override this.ToString() =
        "(" + this.x.ToString() + ", " + this.y.ToString() + ")"

type CircList<'T>(_collection : seq<'T>) =
    inherit System.Collections.Generic.List<'T> (_collection)
    member l.ItemAt (n:int) = base.Item ((n%(l.Count) + l.Count)%l.Count)

let travel (directions:string list) =
    let directionList = CircList[Pair(0,1);Pair(1,0);Pair(0,-1);Pair(-1,0)]
    let rec step directions (position:Pair) dirInd =
        match directions with
        | [] -> position
        | head::tail -> 
            let newDir = 
                match Seq.head head with 
                | 'R' -> dirInd + 1
                | 'L' -> dirInd - 1
            let dist = 
                match new string(Seq.tail head|>Array.ofSeq) with
                | Integer i -> i
            step tail (position + dist*(directionList.ItemAt newDir)) newDir
    step directions (Pair(0,0)) 0

input.Split([|',';' '|],System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
|> travel |> (fun p -> Math.Abs(p.x) + Math.Abs(p.y)) |> printfn "%A"