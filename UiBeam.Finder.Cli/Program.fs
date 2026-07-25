open System
open UiBeam.Finder.Core

let rec isUiBeam (uibeamList: (int*int) list list) ((l, c): int*int) (uibeamIndex: int) (charIndex: int) =
    if uibeamIndex < List.length uibeamList then
        let uibeam = uibeamList.[uibeamIndex]
        if charIndex < List.length uibeam then
            let (ul, uc) = uibeam.[charIndex]
            if l = ul && c = uc then
                true
            else
                // 一致しなかったら次の文字へ
                let nextCharIndex = charIndex + 1
                isUiBeam uibeamList (l,c) uibeamIndex nextCharIndex
        else
            // 次のういビームへ
            let nextUiBeamIndex = uibeamIndex + 1
            isUiBeam uibeamList (l,c) nextUiBeamIndex 0
    else
        false 

let test = @"ういビームとは、
イラストレーター兼 VTuber のしぐれうい氏が放つ必殺技である(というのは嘘で、ファンの間での身内ネタである)。ちなみに、
微細構造定数とは、電磁相互作用の強さを示す定数であり、
1を137で割った値に近い、
無次元量である。"

let arr = TextMatrix.ToCharMatrix test
printfn "[探索する文字列]\r\n%s" test
printfn ""

let finder = new Finder(arr)
let uibeamList = finder.Start()
//printfn "uibeamList = %A" uibeamList

let uibeamCount = uibeamList |> List.length
if uibeamCount > 0 then
    printfn "%d 件のういビームが発見されました!" uibeamCount
    printfn ""
    let mutable lineIndex = 0
    for line in arr do
        let mutable charIndex = 0
        for c in line do
            let isUiBeamResult = isUiBeam uibeamList (lineIndex, charIndex) 0 0
            if isUiBeamResult then
                Console.ForegroundColor <- ConsoleColor.Red
                Console.Write(c)
                Console.ResetColor()
            else
                Console.Write(c)
            charIndex <- charIndex + 1
        Console.Write("\r\n")
        lineIndex <- lineIndex + 1
    Console.Write("\r\n")
else
    printfn "ういビーム検出器はういビームを発見できませんでした．"