/*/ Switch \**********************************************\
|  PQ equivalent switch function to DAX.                  |
|-[ author ]----------------------------------------------|
|  2024, Filip Kraus, contact@filipkraus.net              |
|  2014, Daniil Maslyuk, xxlbi.com                        |
|-[ changelog ]-------------------------------------------|
|  2023-12-21  1.0.0  Initial
\*/

let Switch =
    let Function = (input as list) as any =>
        let
            Source = List.Buffer(input),
            Expression = List.First(Source),
            SkipExpression = List.Skip(Source),
            HasElse = Number.IsEven(List.Count(Source)),
            ElseValue = if HasElse then List.Last(Source) else null,
            ValuesResults = List.RemoveLastN(SkipExpression, Number.From(HasElse)),
            Values = List.Alternate(ValuesResults, 1, 1, 1),
            Results = List.Alternate(ValuesResults, 1, 1, 0),
            FirstResult = List.PositionOf(Values, Expression),
            FunctionResult = if FirstResult = -1 then ElseValue else Results{FirstResult}
        in
            FunctionResult,
        FunctionType = type function (input as list) as any
            meta [
                Documentation.Name = "Switch",
                Documentation.LongDescription = "Evaluates an expression against a list of values and returns one of multiple possible result expressions.",
                Documentation.Examples = {
                    [Description = "Simple Switch expression", Code = "Switch({2, 1, ""A"", 2, ""B""})", Result = """B"""],
                    [Description = "An equivalent of SWITCH(TRUE... in DAX", Code = "Switch({true, 1 > 2, ""A"", 1 < 2, ""B"", ""No result""})", Result = """B"""]
                }
            ],
        TypedFunction = Value.ReplaceType(Function, FunctionType)
    in
        TypedFunction
in
    Switch