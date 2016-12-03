let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D2input.txt")

let keyPad = 
    [
        ['X';'-';'1';'+';'X'];
        ['-';'2';'3';'4';'+'];
        ['5';'6';'7';'8';'9'];
        ['/';'A';'B';'C';'*'];
        ['X';'/';'D';'*';'X']
    ]

let getNumAtIndex (x,y) =
    keyPad.[y].[x]

let constrainToNumPad (x,y)=
    (min 4 (max 0 x),min 4 (max 0 y))

let constrainXToNumPad pos =
    let (x,y) = constrainToNumPad pos
    match getNumAtIndex (x,y) with
    | '+' | '*' -> (x-1,y)
    | '-' | '/' -> (x+1,y)
    | 'X' -> failwith "managed to get out of bounds"
    | _ -> (x,y)

let constrainYToNumPad pos =
    let (x,y) = constrainToNumPad pos
    match getNumAtIndex (x,y) with
    | '-' | '+' -> (x,y+1)
    | '/' | '*' -> (x,y-1)
    | 'X' -> failwith "managed to get out of bounds"
    | _ -> (x,y)
    
let getDigitAndLocation startPos (dirs : string)=
    let rec getDigitLocation (x,y) (dirs : char list) =
        match dirs with
        | [] -> (x,y)
        | head::tail -> 
            let nextLoc = 
                match head with
                | 'U' -> constrainYToNumPad (x,y-1)
                | 'D' -> constrainYToNumPad (x,y+1)
                | 'L' -> constrainXToNumPad (x-1,y)
                | 'R' -> constrainXToNumPad (x+1,y)
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