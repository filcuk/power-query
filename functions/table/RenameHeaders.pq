/*/ RenameHeaders \***************************************\
|  Dynamically rename headers based on a mapping table.   |
|-[ author ]----------------------------------------------|
|  2025, Filip Kraus, contact@filipkraus.net              |
|-[ changelog ]-------------------------------------------|
|  2026-07-22  1.0.1  Fix parameter documentation
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
				Documentation.FieldDescription = "Table whose column headers should be renamed"
			]),
			Map as (type table meta [
				Documentation.FieldCaption = "Mapping Table",
				Documentation.FieldDescription = "Must contain columns Name (current header) and Alias (new header). Only rows whose Name exists in Target are applied."
			])
			) as table meta [
				Documentation.Name = "Rename Headers",
				Documentation.LongDescription = "Renames table headers using a mapping table with Name and Alias columns.",
				Documentation.Examples = {
					[Description = "Basic usage", Code = "RenameHeaders(Source, #table({""Name"", ""Alias""}, {{""Col1"", ""Customer ID""}}))", Result = "Source with Col1 renamed to Customer ID"]
				}
			],
		TypedFunction = Value.ReplaceType(Function, FunctionType)
	in
		TypedFunction
in
	RenameHeaders