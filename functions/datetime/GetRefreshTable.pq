/*/ REFRESH TABLE \**************************************\
|  Generate a table with refresh and client information.  |
|-[ author ]----------------------------------------------|
|  2023, Filip Kraus, contact@filipkraus.net              |
|-[ changelog ]-------------------------------------------|
|  2024-09-26  1.1.0  Improve column names
|                     Add summer time calculation
|                     Add custom time zone option
|                     Add output types
|                     Add documentation
|  2023-05-18  1.0.0  Initial
\*/

let GetRefreshTable = 
	let Function = (
		optional CustomOffset as nullable number
		,optional SummerTimeOffset as nullable logical
		,optional OutputType as nullable text
		) as any =>
		let
			// Defaults
			SummerTimeOffset = if CustomOffset <> null and SummerTimeOffset = null then false else SummerTimeOffset,
			OutputType = if OutputType = null then "Horizontal" else OutputType,

			// Generate base table
			Source = DateTimeZone.UtcNow(),
			Table = #table(type table [#"Refresh UTC" = datetimezone], {{Source}}),
			RefreshLocal = Table.AddColumn(Table, "Refresh Local", each DateTimeZone.ToLocal([Refresh UTC]), type datetimezone),
			TimeZoneLocalOffset = Table.AddColumn(RefreshLocal, "TZ Local Offset", each DateTimeZone.ZoneHours([Refresh Local]), type number),
			TimeZoneLocal = Table.AddColumn(TimeZoneLocalOffset, "TZ Local", each TimeZone.Current, type text),
			CultureLocal = Table.AddColumn(TimeZoneLocal, "Culture Local", each Culture.Current, type text),

			// Summer time (European standards)
			CurrentDate = DateTime.From(Source),
			SummerStart = Date.StartOfWeek(#date(Date.Year(Source), 3, 14), Day.Sunday) & #time(1, 0, 0),
			SummerEnd = Date.StartOfWeek(#date(Date.Year(Source), 11, 7), Day.Sunday) & #time(1, 0, 0),
			SummerTime = CurrentDate >= SummerStart and CurrentDate <= SummerEnd,
			AddSummerTime = Table.AddColumn(CultureLocal, "Summer Time", each SummerTime, type logical),
			StandardOutput = AddSummerTime,

			// Custom TZ offset
			CustomOutput = let
				CustomOffsetFinal = CustomOffset + (if SummerTime and SummerTimeOffset then 1 else 0),
				AddRefreshCustom = Table.AddColumn(AddSummerTime, "Refresh Custom", each DateTimeZone.SwitchZone([Refresh UTC], CustomOffsetFinal), type datetimezone),
				AddTZCustomOffset = Table.AddColumn(AddRefreshCustom, "TZ Custom Offset", each CustomOffsetFinal, Int64.Type),
				Output = AddTZCustomOffset
			in Output,

			// Output switching
			OutputTable = if CustomOffset = null then StandardOutput else CustomOutput,
			OutputTableVertical = Table.UnpivotOtherColumns(OutputTable, {}, "Name", "Value"),
			OutputRecord = Record.FromTable(OutputTableVertical),
			Output = 
				if OutputType = "Horizontal" then OutputTable
				else if OutputType = "Vertical" then OutputTableVertical
				else if OutputType = "Record" then OutputRecord
				else null
		in
			Output,
			FunctionType = type function (
				optional CustomOffset as (type number meta [
					Documentation.FieldCaption = "Custom TZ Offset",
					Documentation.FieldDescription = "Hours to offset from UTC time"
				])
				,optional SummerTimeOffset as (type logical meta [
					Documentation.FieldCaption = "Summer Time Offset",
					Documentation.FieldDescription = "Automatically adjust custom TZ for summer time",
					Documentation.AllowedValues = {true, false}
				])
				,optional OutputType as (type text meta [
					Documentation.FieldCaption = "Output Type",
					Documentation.FieldDescription = "Type of output to be generated",
					Documentation.AllowedValues = {"Horizontal", "Vertical", "Record"}
				])
				) as any meta [
					Documentation.Name = "Get Refresh Table"
					,Documentation.LongDescription = "Returns a table with current date and time."
					// ,Documentation.Examples = {
					// 	[Description = "", Code = "", Result = ""]
					// }
				],
			TypedFunction = Value.ReplaceType(Function, FunctionType)
		in
			TypedFunction
in
	GetRefreshTable