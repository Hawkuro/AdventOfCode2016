let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D2input.txt")

let getNumAtIndex (x,y) =
//    x: 0   1   2
// y: +-------------
//  0 | [1] [2] [3]
//  1 | [4] [5] [6]
//  2 | [7] [8] [9]
    1+x+3*y

let constrainToNumPad (x,y)=
    (min 2 (max 0 x),min 2 (max 0 y))

let getDigitAndLocation startPos (dirs : string)=
    let rec getDigitLocation (x,y) (dirs : char list) =
        match dirs with
        | [] -> (x,y)
        | head::tail -> 
            let nextLoc = 
                match head with
                | 'U' -> constrainToNumPad (x,y-1)
                | 'D' -> constrainToNumPad (x,y+1)
                | 'L' -> constrainToNumPad (x-1,y)
                | 'R' -> constrainToNumPad (x+1,y)
            getDigitLocation nextLoc tail
    let loc = getDigitLocation startPos (Seq.toList dirs)
    (getNumAtIndex loc, loc)

let unfoldable ((loc, list) as state) = 
    match list with
    | [] -> None
    | head::tail -> 
        let (newNum, newLoc) = getDigitAndLocation loc head
        Some(newNum,(newLoc,tail))
Seq.unfold unfoldable ((1,1),input|>Array.toList) |> Seq.toArray |> printfn "%A"