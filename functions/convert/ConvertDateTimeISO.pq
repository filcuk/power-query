/*********************************************************\
| CONVERT TIME VALUE TO TIME TYPE
|
|=--notes----------------------------------------------=|
| Interprets a numeric HHmmss / Hmmss value as time.
| Input:  133050, 33050, 935
| Output: 13:30:50, 03:30:50, 00:09:35
| (Not an ISO datetime converter — name kept for compatibility.)
|
|=--credits----------------------------------------------=|
| Original 2023 by Filip Kraus, contact@filipkraus.net
|
|=--changelog--------------------------------------------=|
| 	2026-07-22
| 		Pad to 6 digits; parse HH:mm:ss; validate ranges
| 		Add documentation
| 	2023-06-13
| 		Initial
| 
\*********************************************************/

let ConvertDateTimeISO =
	let Function = (DateTime as number) as time =>
		let
			Digits = Text.From(Number.RoundDown(DateTime, 0)),
			Padded = Text.PadStart(Digits, 6, "0"),
			Hour = Number.From(Text.Start(Padded, 2)),
			Minute = Number.From(Text.Middle(Padded, 2, 2)),
			Second = Number.From(Text.End(Padded, 2)),
			Output =
				if Text.Length(Digits) > 6 then
					error Error.Record("Expression.Error", "Time value must have at most 6 digits (HHmmss).")
				else if Hour > 23 or Minute > 59 or Second > 59 then
					error Error.Record("Expression.Error", "Invalid time components in numeric value.")
				else
					#time(Hour, Minute, Second)
		in
			Output,

		FunctionType = type function (
			DateTime as (type number meta [
				Documentation.FieldCaption = "Numeric time",
				Documentation.FieldDescription = "HHmmss or Hmmss number to convert (e.g. 133050, 33050)",
				Documentation.SampleValues = {133050, 33050, 935}
			])
			) as time meta [
				Documentation.Name = "Convert DateTime ISO",
				Documentation.LongDescription = "Converts a numeric HHmmss / Hmmss value to a time type. Despite the name, this is not an ISO datetime parser.",
				Documentation.Examples = {
					[Description = "Six-digit HHmmss", Code = "ConvertDateTimeISO(133050)", Result = "#time(13, 30, 50)"],
					[Description = "Five-digit Hmmss", Code = "ConvertDateTimeISO(33050)", Result = "#time(3, 30, 50)"]
				}
			],
		TypedFunction = Value.ReplaceType(Function, FunctionType)
	in
		TypedFunction
in
	ConvertDateTimeISO
