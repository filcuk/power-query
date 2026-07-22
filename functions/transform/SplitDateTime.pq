/*/ SplitDateTime \***************************************\
|  Split a date/time value into individual date & time.   |
|-[ author ]----------------------------------------------|
|  2025, Filip Kraus, contact@filipkraus.net              |
|-[ changelog ]-------------------------------------------|
|  2026-07-22  1.1.0  Implement stub
|  2025-06-05  1.0.0  Initial
\*/

let SplitDateTime =
	let Function = (
		Target as datetime
	) as table =>
		let
			Output = #table(
				type table [Date = date, Time = time],
				{{Date.From(Target), Time.From(Target)}}
			)
		in
			Output,
		FunctionType = type function (
			Target as (type datetime meta [
				Documentation.FieldCaption = "Date & time value",
				Documentation.FieldDescription = "Value to split into date and time"
			])
			) as table meta [
				Documentation.Name = "Split Date & Time",
				Documentation.LongDescription = "Splits a date/time value into a one-row table with Date and Time columns.",
				Documentation.Examples = {
					[Description = "Basic usage", Code = "SplitDateTime(#datetime(2025, 6, 5, 14, 30, 0))", Result = "Table with Date = 2025-06-05, Time = 14:30:00"]
				}
			],
		TypedFunction = Value.ReplaceType(Function, FunctionType)
	in
		TypedFunction
in
	SplitDateTime
