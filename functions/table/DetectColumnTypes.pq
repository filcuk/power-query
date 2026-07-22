/*/ DetectColumnTypes \***********************************\
|  Return ingested table with automatically detected      |
|  column types.                                          |
|-[ usage ]-----------------------------------------------|
|  Limited to hard-coded types: datetime, date, number    |
|  and text (fallback: any). Sample size and error margin |
|  are optional parameters.                               |
|-[ author ]----------------------------------------------|
|  2021, Steve de Peijper, linkedin.com/in/stevedep/      |
|  2023, Filip Kraus, contact@filipkraus.net              |
|-[ changelog ]-------------------------------------------|
|  2026-07-22  2.0.0  Replace Expression.Evaluate/#shared with native converters
|                     Expose optional SampleSize and MarginForError
|                     Prefer date when datetime values are all midnight
|                     Fix all-null / no-match columns crashing
\*/

let DetectColumnTypes =
	let Function = (
		Table as table,
		optional SampleSize as nullable number,
		optional MarginForError as nullable number
	) as table =>
		let
			SampleSize = if SampleSize = null then 7 else SampleSize,
			MarginForError = if MarginForError = null then 0.1 else MarginForError,

			DetectType = (tbl as table, col as text) as type =>
				let
					Sample = List.FirstN(Table.Column(tbl, col), SampleSize),
					CountErrors = (convert as function) as number =>
						List.Count(
							List.Select(
								List.Transform(Sample, each try convert(_) otherwise "Error"),
								each _ = "Error"
							)
						),
					NumberOfErrors = {
						CountErrors(DateTime.From),
						CountErrors(Date.From),
						CountErrors(Number.From),
						CountErrors(Text.From)
					},
					CheckWithinMargin = List.Transform(NumberOfErrors, each _ <= SampleSize * MarginForError),
					typenr = List.PositionOf(CheckWithinMargin, true),
					TypeNames = {"datetime", "date", "number", "text"},
					CheckType =
						if List.IsEmpty(Sample) or List.IsEmpty(List.RemoveNulls(Sample)) or typenr = -1 then
							"any"
						else if TypeNames{typenr} = "datetime" and CheckWithinMargin{1} then
							let
								Converted = List.RemoveNulls(
									List.Transform(List.RemoveNulls(Sample), each try DateTime.From(_) otherwise null)
								),
								AllMidnight = List.AllTrue(
									List.Transform(Converted, each Time.From(_) = #time(0, 0, 0))
								)
							in
								if AllMidnight then "date" else "datetime"
						else
							TypeNames{typenr},
					Result = Record.Field(
						[number = type number, date = type date, datetime = type datetime, text = type text, any = type any],
						CheckType
					)
				in
					Result,

			Output = Table.TransformColumnTypes(
				Table,
				List.Transform(Table.ColumnNames(Table), each {_, DetectType(Table, _)})
			)
		in
			Output,

		FunctionType = type function (
			Table as (type table meta [
				Documentation.FieldCaption = "Table",
				Documentation.FieldDescription = "Table whose column types should be detected"
			]),
			optional SampleSize as (type number meta [
				Documentation.FieldCaption = "Sample size",
				Documentation.FieldDescription = "Number of rows to sample per column. Defaults to 7",
				Documentation.SampleValues = {7}
			]),
			optional MarginForError as (type number meta [
				Documentation.FieldCaption = "Error margin",
				Documentation.FieldDescription = "Allowed conversion failure rate (0–1). Defaults to 0.1",
				Documentation.SampleValues = {0.1}
			])
			) as table meta [
				Documentation.Name = "Detect Column Types",
				Documentation.LongDescription = "Returns the table with column types detected from a sample of values (datetime, date, number, text, or any)."
			],
		TypedFunction = Value.ReplaceType(Function, FunctionType)
	in
		TypedFunction
in
	DetectColumnTypes
