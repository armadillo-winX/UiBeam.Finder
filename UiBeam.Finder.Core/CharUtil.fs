namespace UiBeam.Finder.Core

module CharUtil =
    
    let u = [| 'う'; 'ウ'; 'ｳ'; '宇'; '雨'; '羽'; '卯'; '兎'; '右' |]
    let i = [| 'い'; 'イ'; 'ゐ'; 'ヰ'; 'ｲ'; '一'; '1'; '１' |]
    let bi = [| 'び'; 'ビ'; '微'; '美' |]
    let prlmark = [| 'ー'; 'ｰ'; '一'; '1'; '１'; '/'; '\\'; '／'; '＼' |]
    let mu = [| 'む'; 'ム'; 'ﾑ'; '無'; '娘'; '迎'; '夢'; '武'; '六' |]

    let internal isU (c: char) =
        u |> Array.contains c

    let internal isI (c: char) =
        i |> Array.contains c

    let internal isBi (c: char) =
        bi |> Array.contains c

    let internal isPrlMark (c: char) =
        prlmark |> Array.contains c

    let internal isMu (c: char) =
        mu |> Array.contains c