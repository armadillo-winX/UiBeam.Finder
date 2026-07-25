namespace UiBeam.Finder.Core

open System

module TextMatrix =
    /// 指定した文字列を行ごとに分割して、各行を文字単位の配列に変換します。
    /// 空白行 (空文字、空白のみの行) はスキップされます。

    let private isBlankLine (s: string) : bool =
        String.IsNullOrWhiteSpace(s)

    /// テキストを受け取り、改行で行分割し、空白行を除去してから
    /// 各行を char の配列に変換して配列の配列を返します。
    /// null が与えられた場合は空の配列を返します。
    let ToCharMatrix (text: string) : char[][] =
        if isNull text then
            Array.empty<char[]>
        else
            let lines = text.Split([|"\r\n"; "\n"; "\r"|], StringSplitOptions.None)
            lines
            |> Array.filter (fun s -> not (isBlankLine s))
            |> Array.map (fun s -> s.ToCharArray())
