/*/ SplitDateTime \***************************************\
|  Split date & time column to individual date & time.    |
|-[ author ]----------------------------------------------|
|  2025, Filip Kraus, contact@filipkraus.net              |
|-[ changelog ]-------------------------------------------|
|  2025-06-05  1.0.0  Initial
\*/

let SplitDateTime =
	let Function = (
		Target as datetime
	) as table =>
		let
			Replace = 0
		in
			Replace,
		FunctionType = type function (
			Map as (type datetime meta [
				Documentation.FieldCaption = "Date & time column",
				Documentation.FieldDescription = "Column to split"
			])
			) as any meta [
				Documentation.Name = "Split Date & Time",
				Documentation.LongDescription = "Splits column into individual date & time columns"
			],
		TypedFunction = Value.ReplaceType(Function, FunctionType)
	in
		TypedFunction
in
	SplitDateTime