/*********************************************************\
| DYNAMIC COLUMN TYPES
|
|=--purpose----------------------------------------------=|
| Return ingested table with automatically detected
| column types.
| 
|=--notes------------------------------------------------=|
| Limited to hard-coded types, currently
| datetime, date, number and text.
| Note that the last line in this example passes
| table and the arguments.
| 
|=--credits----------------------------------------------=|
| Original: https://community.powerbi.com/t5/Community-Blog/Dynamically-Detect-Column-Types-with-Power-M/ba-p/1682639
| 2021, Steve de Peijper, linkedin.com/in/stevedep/
| 2023, Filip Kraus, filipkraus.net
|
|=--changelog--------------------------------------------=|
| 
\*********************************************************/

(tbl as table) as table  =>
let
    fn = (tbl as table, col as text, numberofrecords as number, marginforerror as number) as type =>
    let
        LijstmetValues = List.FirstN( Table.Column(tbl, col),numberofrecords), 
        Env = Record.Combine({[L=LijstmetValues],[DTF = #shared[DateTime.From]], [DF = #shared[Date.From]], [NF = #shared[Number.From]], [TF = #shared[Text.From]], [LT = #shared[List.Transform]], [LS = #shared[List.Select]], [LC = #shared[List.Count]]   }),
        NumberOfErrors = List.Transform({"DTF", "DF", "NF", "TF"}, each Expression.Evaluate("
                    LC(LS(
                        LT(L, each try  " & _ & "(_) otherwise ""Error""), each _ = ""Error""))", Env)),
        CheckWithinMargin = List.Transform(NumberOfErrors, each _ <= numberofrecords * marginforerror),
        typenr = List.PositionOf(CheckWithinMargin, true),  

        FirstTypeWithinMargin =  {"datetime", "date", "number", "text"}{typenr},
        CheckType = if List.Distinct(LijstmetValues){0} = null or FirstTypeWithinMargin = -1 then 4  else FirstTypeWithinMargin,
        result = Record.Field([number = type number,date = type date,datetime = type datetime,text = type text, any = type any],CheckType)
    in result,
    Columnsto2Type = Table.TransformColumnTypes(tbl, List.Transform(Table.ColumnNames(tbl), each {_,  fn(tbl,_, 7, 0.1)}))    
in
    Columnsto2Type