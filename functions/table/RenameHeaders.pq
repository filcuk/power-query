/*/ RenameHeaders \***************************************\
|  Dynamically rename headers based on a mapping table.   |
|-[ author ]----------------------------------------------|
|  2025, Filip Kraus, contact@filipkraus.net              |
|-[ changelog ]-------------------------------------------|
|  2025-06-05  1.0.0  Initial
\*/

let RenameHeaders =
	let Function = (
		Target as table,
		Map as table
	) as table =>
		let
			Matches = Table.SelectRows(Map, each List.Contains(Table.ColumnNames(Target), [Name])),
			Replace = Table.RenameColumns(Target, List.Zip({Matches[Name], Matches[Alias]}))
		in
			Replace,
		FunctionType = type function (
			Target as (type table meta [
				Documentation.FieldCaption = "Target Table",
				Documentation.FieldDescription = "Include releases from selected date onwards"
			]),
			Map as (type table meta [
				Documentation.FieldCaption = "Mapping Table",
				Documentation.FieldDescription = "Include releases from selected release onwards"
			])
			) as any meta [
				Documentation.Name = "Rename Headers",
				Documentation.LongDescription = "Renames table headers using a column mapping"
			],
		TypedFunction = Value.ReplaceType(Function, FunctionType)
	in
		TypedFunction
in
	RenameHeaders