/*/ CleanHeaders \****************************************\
|  Use Text.Clean() to clean table headers.               |
|-[ author ]----------------------------------------------|
|  2023, Filip Kraus, contact@filipkraus.net              |
|-[ changelog ]-------------------------------------------|
|  2026-07-22  1.1.0  Promote cleaned header row back to headers
|                     Add documentation
|  2023-07-26  1.0.0  Initial
\*/

let CleanHeaders =
	let Function = (
		Table as table,
		Demote as logical
	) as table =>
		let
			Source =
				if Demote then Table.DemoteHeaders(Table)
				else Table,
			Transpose = Table.Transpose(Source),
			Clean = Table.TransformColumns(Transpose, {{"Column1", Text.Clean}}),
			Trim = Table.TransformColumns(Clean, {{"Column1", Text.Trim}}),
			ReTranspose = Table.Transpose(Trim),
			Result = Table.PromoteHeaders(ReTranspose, [PromoteAllScalars = true])
		in
			Result,

		FunctionType = type function (
			Table as (type table meta [
				Documentation.FieldCaption = "Table",
				Documentation.FieldDescription = "Table whose headers should be cleaned"
			]),
			Demote as (type logical meta [
				Documentation.FieldCaption = "Demote headers",
				Documentation.FieldDescription = "When true, demotes headers before cleaning then promotes them again. When false, expects the header row to already be demoted as data.",
				Documentation.AllowedValues = {true, false},
				Documentation.SampleValues = {true}
			])
			) as table meta [
				Documentation.Name = "Clean Headers",
				Documentation.LongDescription = "Cleans and trims table header names using Text.Clean and Text.Trim."
			],
		TypedFunction = Value.ReplaceType(Function, FunctionType)
	in
		TypedFunction
in
	CleanHeaders
