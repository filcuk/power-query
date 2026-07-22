/*/ TrimAll \*********************************************\
| Trim whitespace around *and* in between text.           |
|-[ author ]----------------------------------------------|
| 2023, Rick De Groot,  gorilla.bi                        |
| 2024, Filip Kraus,    contact@filipkraus.net            |
|-[ changelog ]-------------------------------------------|
| 2026-07-22  1.1.0   Rename function
|                     Return text
|                     Split on space/tab/CR/LF/NBSP
| 2024-04-04  1.0.0   Initial
\*/

let
    TrimAll =
        let
            Function = (Input as text) as text =>
                let
                    Trim = Text.Trim(Input),
                    Separators = " " & Character.FromNumber(9) & Character.FromNumber(10) & Character.FromNumber(13) & Character.FromNumber(160),
                    Split = Text.SplitAny(Trim, Separators),
                    NonEmpty = List.RemoveItems(Split, {""}),
                    Combined = Text.Combine(NonEmpty, " ")
                in
                    Combined,
            FunctionType = type function (Input as (type text meta [
                    Documentation.FieldCaption = "Text input",
                    Documentation.FieldDescription = "Text you want trimmed (collapses space, tab, CR, LF, NBSP)",
                    Documentation.SampleValues = {"Example   text"},
                    Formatting.IsMultiLine = true,
                    Formatting.IsCode = false
                ])
                ) as text
                meta [
                    Documentation.Name = "TrimAll",
                    Documentation.LongDescription = "Trim whitespace around and in between text, collapsing runs of spaces/tabs/newlines to a single space.",
                    Documentation.Examples = {
                        [Description = "Collapse extra spaces", Code = "TrimAll("" This  has    extra spaces   "")", Result = """This has extra spaces"""]
                    }
                ],
            TypedFunction = Value.ReplaceType(Function, FunctionType)
        in
            TypedFunction
in
    TrimAll
