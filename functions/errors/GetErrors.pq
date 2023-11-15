/*********************************************************\
| CLEAN HEADERS
|
|=--purpose----------------------------------------------=|
| Use Text.Clean() to clean table headers.
| 
|=--credits----------------------------------------------=|
| Original https://community.fabric.microsoft.com/t5/Power-Query/List-all-errors-in-a-table/td-p/1470730
| Modified 2023 by Filip Kraus, filipkraus.net
|
|=--changelog--------------------------------------------=|
| 	2023-08-01, 1.0.0
| 		Initial
|   2023-11-15, 1.1.0
|       Fix Excel formula errors showing as null
\*********************************************************/

(table as table) as table =>
    let
        errorExtract = (value as any) as any => 
            if value[HasError] = true
            then value[Error]
            else value[Value],

        errorToText = (errorRec as any) as any =>
            if Type.Is(Value.Type(errorRec), Record.Type)
            then "Reason: " & errorRec[Reason] & "#(lf)Message: " & errorRec[Message] & "#(lf)Value: " & (
                if errorRec[Detail] is null then "null" else
                try Text.From(errorRec[Detail])
                otherwise try Text.From(errorRec[Detail][Value])
                otherwise null
            )
            else errorRec,

        recsToErrors = (rec as record) as any =>
            List.Accumulate(
                Record.FieldNames(rec), [], (s, c) => 
                    Record.AddField(s, c, errorToText(errorExtract(try Record.Field(rec, c)))) 
            ),

        Output = 
            Table.FromList(
                List.Transform(
                    Table.ToRecords(
                        Table.SelectRowsWithErrors(table)
                    ), each recsToErrors(_)
                ), Record.FieldValues, Table.ColumnNames(table)
            )
    in
        Output