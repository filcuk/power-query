/*********************************************************\
| CONVERT TIME VALUE TO TIME TYPE
|
|=--notes----------------------------------------------=|
| Input: 133050, 33050
| Output: 13:30, 03:30
|
|=--credits----------------------------------------------=|
| Original 2023 by Filip Kraus, filipkraus.net
|
|=--changelog--------------------------------------------=|
| 	2023-06-13
| 		Initial
| 
\*********************************************************/

let ConvertTimeSimple = (SharpTime as number) => let
        Source = Number.ToText(SharpTime)
        ,Len = Text.Length(Source)
        ,Hour = Text.Start(Source, (if Len = 6 then 2 else 1))
        ,Minute = Text.Middle(Source, (if Len = 6 then 2 else 1), 2)
        ,Time = Time.FromText(Hour & ":" & Minute)
        ,Output = Time
    in Output
in
    ConvertTimeSimple