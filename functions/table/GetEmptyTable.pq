/*********************************************************\
| EMPTY TABLE GENERATOR
|
|=--purpose----------------------------------------------=|
| Creates an empty table. Completely unecessary fn.
| 
|=--usage------------------------------------------------=|
| Pass false, apply changes and create a placeholder
| measure: `placeholder3 = 0`. Change param to true.
| 
|=--credits----------------------------------------------=|
| Original 2023 by Filip Kraus, filipkraus.net
|
\*********************************************************/

let GetEmptyTable = (Final as logical) =>
    let
        Source = Table.FromRows(Json.Document(Binary.Decompress(Binary.FromText("i44FAA==", BinaryEncoding.Base64), Compression.Deflate)), let _t = ((type nullable text) meta [Serialized.Text = true]) in type table [RemoveMe = _t]),
        Table = Table.RemoveColumns(Source, "RemoveMe")
    in
        if Final then Table else Source
in
    GetEmptyTable