open System
open System.IO
open System.Diagnostics
open System.Reflection
open UiBeam.Finder.Core

// その文字の座標が "ういビーム" の文字座標のどれかに引っかかるかを調べます
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

// テキストからういビームを探索して結果を出力します
let find (text: string) =
    let arr = TextMatrix.ToCharMatrix text
    printfn "[探索する文字列]\r\n%s" text
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
        printfn "ういビーム検出器はういビームを発見できませんでした．\r\n"

let rec mainLoop () =
    printfn "テキストファイルのパスを入力 (または :q で終了):"

    let input = Console.ReadLine().Trim()
    match input.ToLower() with
    | ":q" -> 
        0
    |_ ->
        printfn ""
        if File.Exists(input) then
            try
                File.ReadAllText(input) |> find
            with
            |_ as ex -> printfn "エラー\r\n%s\r\n" ex.Message

            mainLoop()
        else
            printfn "ファイル '%s' は存在しません．\r\n" input
            mainLoop()
    

// The Main Entry as follow:
[<EntryPoint>]
let main args =
    let assemblyFilePath = Assembly.GetExecutingAssembly().Location

    let appName = FileVersionInfo.GetVersionInfo(assemblyFilePath).ProductName
    let appVersion = FileVersionInfo.GetVersionInfo(assemblyFilePath).ProductVersion
    let developerName = FileVersionInfo.GetVersionInfo(assemblyFilePath).CompanyName

    printfn "---------------------------------------------------------------------------"
    printfn "%s ver.%s" appName appVersion
    printfn "by %s" developerName
    printfn "---------------------------------------------------------------------------"

    if args |> Array.length = 0 then
        printfn "テキストファイルの文字列からういビームを発見します．"
        mainLoop()
    else if args |> Array.length = 1 then
        let filePath = args.[0]
        if File.Exists(filePath) then
            try
                File.ReadAllText(filePath) |> find
                0
            with
            |_ as ex -> 
                printfn "エラー\r\n%s\r\n" ex.Message
                1
        else
            printfn "ファイル '%s' は存在しません．\r\n" filePath
            1
    else
        printfn "コマンドライン引数が多すぎます．"
        printfn "%s では，コマンドライン引数を指定しないか，またはファイルパスをコマンドライン引数として指定可能です．" appName
        printfn "[例] PS> ./uibf \"D:\\test\\sample.txt\""
        1