/*/ GetEmptyTable \***************************************\
|  Creates an empty table. Very useful.                   |
|-[ usage ]-----------------------------------------------|
|  Pass false, apply changes and create a placeholder     |
|  measure: `placeholder3 = 0`. Change param to true.     |
|-[ author ]----------------------------------------------|
|  2023, Filip Kraus, contact@filipkraus.net              |
|-[ changelog ]-------------------------------------------|
|  2026-07-22  1.0.1  Add documentation
|  2023-06-06  1.0.0  Initial
\*/

let GetEmptyTable =
	let Function = (Final as logical) as table =>
		let
			Source = Table.FromRows(Json.Document(Binary.Decompress(Binary.FromText("i44FAA==", BinaryEncoding.Base64), Compression.Deflate)), let _t = ((type nullable text) meta [Serialized.Text = true]) in type table [RemoveMe = _t]),
			Table = Table.RemoveColumns(Source, "RemoveMe")
		in
			if Final then Table else Source,

		FunctionType = type function (
			Final as (type logical meta [
				Documentation.FieldCaption = "Final empty table",
				Documentation.FieldDescription = "When false, returns a one-column placeholder table (RemoveMe). When true, returns a fully empty table with no columns.",
				Documentation.AllowedValues = {true, false},
				Documentation.SampleValues = {false}
			])
			) as table meta [
				Documentation.Name = "Get Empty Table",
				Documentation.LongDescription = "Creates an empty or placeholder table useful when scaffolding a model before real queries exist."
			],
		TypedFunction = Value.ReplaceType(Function, FunctionType)
	in
		TypedFunction
in
	GetEmptyTable
