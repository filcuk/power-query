/*********************************************************\
| CLEAN HEADERS
|
|=--purpose----------------------------------------------=|
| Use Text.Clean() to clean table headers.
| 
|=--credits----------------------------------------------=|
| Original 2023 by Filip Kraus, contact@filipkraus.net
|
|=--changelog--------------------------------------------=|
| 	2023-07-26
| 		Initial
\*********************************************************/

let
    Source = (Table as table, Demote as logical) => 
    let
        Source = 
            if Demote then Table.DemoteHeaders(Table)
            else Table,
        Transpose = Table.Transpose(Source),
        Clean = Table.TransformColumns(Transpose, {{"Column1", Text.Clean}}),
        Trim = Table.TransformColumns(Clean, {{"Column1", Text.Trim}}),
        ReTranspose = Table.Transpose(Trim)
    in
        ReTranspose
in
    Source