/*/ GetUsedColumns \**************************************\
|  Remove automatically-named columns from a table.       |
|-[ usage ]-----------------------------------------------|
|  Useful when ingesting an Excel sheet that is not a     |
|  formal table and may have an incorrectly defined       |
|  range, leaving unused columns named Column1, etc.      |
|-[ author ]----------------------------------------------|
|  2026, Filip Kraus, contact@filipkraus.net              |
|-[ changelog ]-------------------------------------------|
|  2026-07-23  1.0.0  Initial
\*/

let GetUsedColumns =
	let Function = (
		Table as table
	) as table =>
		let
			AutoNamed = List.Select(
				Table.ColumnNames(Table),
				each Text.StartsWith(_, "Column")
			),
			Result = Table.RemoveColumns(Table, AutoNamed, MissingField.Ignore)
		in
			Result,

		FunctionType = type function (
			Table as (type table meta [
				Documentation.FieldCaption = "Table",
				Documentation.FieldDescription = "Table whose automatically-named columns (Column1, Column2, ...) should be removed"
			])
			) as table meta [
				Documentation.Name = "Get Used Columns",
				Documentation.LongDescription = "Removes columns whose names start with ""Column"", such as Column1 or Column999. Intended for Excel sheets without a table type where an oversized range left unused auto-named columns."
			],
		TypedFunction = Value.ReplaceType(Function, FunctionType)
	in
		TypedFunction
in
	GetUsedColumns
