/*/ FULL TRIM \*******************************************\
| Trim spaces around *and* in between text efficiently.   |
/*/ TrimAll \*********************************************\
|-[ author ]----------------------------------------------|
| 2023, Rick De Groot,  gorilla.bi                        |
| 2024, Filip Kraus,    contact@filipkraus.net            |
|-[ changelog ]-------------------------------------------|
| 2026-07-22  1.1.0   Rename function
| 2024-04-04  1.0.0   Initial
\*/

let
    TrimFull =
    TrimAll =
        let
            Function = (Input as text) as any =>
                let
                    Trim = Text.Trim(Input),
					Split = Text.Split(Trim, " "),
					NonEmpty = List.RemoveItems(Split, {""}),
					Combined = Text.Combine(NonEmpty," ")
                in
                    Combined,
            FunctionType = type function (Input as (type text meta [
					Documentation.FieldCaption = "Text input",
					Documentation.FieldDescription = "Text you want trimmed",
					Documentation.SampleValues = {"Example   text"},
					Formatting.IsMultiLine = true,
					Formatting.IsCode = false
				])
				) as any
                meta [
                    Documentation.Name = "TrimFull",
                    Documentation.LongDescription = "Trim spaces around and in between text.",
                    Documentation.Examples = {
                        [Description = "", Code = "TrimFull("" This  has    extra spaces   "")", Result = """This has extra spaces"""]
                        [Description = "Collapse extra spaces", Code = "TrimAll("" This  has    extra spaces   "")", Result = """This has extra spaces"""]
                    }
                ],
            TypedFunction = Value.ReplaceType(Function, FunctionType)
        in
            TypedFunction
in
    TrimFull    TrimAll
