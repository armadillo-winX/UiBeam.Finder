namespace UiBeam.Finder.Core

// UiBeam.Finder では，文字を座標(タプル型)で管理します．
// 文字列は行列で管理し，(0, 1) は0行2文字目の文字を表します．
type Finder(charMatrix: char[][]) =
    // 'う' に相当する文字のすべての座標をリストで格納
    let mutable uCharsList : (int*int) list = []

    // "ういビーム" の座標セットをリストで格納
    // e.g. [ [ (0,1); (0,2); ... ] ; ... ]
    let mutable uibeamList: ((int*int) list) list = []

    member val CharMatrix = charMatrix with private get

    // 座標から文字を取得
    member private this.getChar (lineIndex: int) (charIndex: int) =
        if lineIndex >= 0 &&  lineIndex < Array.length this.CharMatrix then
            let line = this.CharMatrix.[lineIndex]
            if charIndex >= 0 && charIndex < Array.length line then
                Some line.[charIndex]
            else
                None
        else
            None

    member private this.checkChar (state: int) (c: char) =
        match state with
        | 0 -> CharUtil.isU c
        | 1 -> CharUtil.isI c
        | 2 -> CharUtil.isBi c
        | 3 -> CharUtil.isPrlMark c || CharUtil.isI c
        | 4 -> CharUtil.isMu c
        |_ -> false

    // 再帰的に 'う' に相当する文字のすべての座標を取得して uCharsList に格納
    member private this.getUChars (lineIndex: int) (charIndex: int) =
        if lineIndex < Array.length this.CharMatrix then
            let line = this.CharMatrix.[lineIndex]
            if charIndex < Array.length line then
                let char = line.[charIndex]
                if CharUtil.isU char then
                    uCharsList <- uCharsList @ [(lineIndex, charIndex)]

                // 行の次の文字に移る
                this.getUChars lineIndex (charIndex + 1)
            else
                // 次の行に移る
                this.getUChars (lineIndex + 1) 0

    /// <summary>
    /// "ういビーム" を探索します
    /// </summary>
    member this.Start() =
        this.getUChars 0 0

        if uCharsList |> List.length > 0 then
            let mutable nUiBeamList: ((int*int) list) list = []
            for ucharCoordinate in uCharsList do
                nUiBeamList <- nUiBeamList @ [[ucharCoordinate]]

            uibeamList <- nUiBeamList

            // state はいまどの文字を探索しているかを示す変数
            // 0 -> う
            // 1 -> い
            // 2 -> ビ
            // 3 -> ー
            // 4 -> ム
            let mutable state = 1
            while state < 5 do
                // 新しい "ういビーム" の座標セットのリスト
                let mutable nUiBeamList: ((int*int) list) list = []
                for uibeam in uibeamList do
                    let (ll, lc) = uibeam.[state-1]

                    // 次に来る文字の座標セット
                    let nextChars = [| 
                        (ll, lc + 1);  // 横並び
                        (ll + 1, lc);      // 縦読み
                        (ll + 1, lc + 1)   // 斜め読み
                        (ll + 1, lc - 1)   // 斜め読み
                        |]

                    for (nl, nc) in nextChars do
                        let next = this.getChar nl nc
                        match next with
                        | Some c -> 
                            if this.checkChar state c then 
                                let nUiBeam = uibeam @ [(nl, nc)]
                                nUiBeamList <- nUiBeamList @ [nUiBeam]
                            else
                                ()
                        | None -> ()

                uibeamList <- nUiBeamList
                state <- state + 1

        uibeamList