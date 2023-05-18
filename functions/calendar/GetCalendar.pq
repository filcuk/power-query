/*********************************************************\
| CALENDAR GENERATOR
|
|=--purpose----------------------------------------------=|
| Generate a date table based on start and end dates.
| 
|=--notes------------------------------------------------=|
| LangTag examples: en-US, en-GB
| Column naming:
|  	%	   date
|  	#, ##	number, padded number (text)
|  	Is ...	boolean
| 	@M/L	text shortenned/full (Jan/January)
| 
|=--credits----------------------------------------------=|
| Original 2014 by Matt Masson, mattmasson.com
| Modified 2020 by Filip Kraus, filipkraus.net
|
|=--changelog--------------------------------------------=|
| 	2023-04-06
| 		Add financial, ISO, calendar 4-4-5
| 		Apply standardised column naming
|	2023-05-11
|		Add 'Week @M'
| 	2023-05-18
| 		Fix last date (EndDate) not being generated
| 
\*********************************************************/

let GetCalendar = (
	StartDate as date,
	EndDate as date,
	optional LangTag as nullable text,
	optional FirstWeekDay as nullable number,
	optional FinYearStartMo as nullable number
) as table =>
	let
		/* Defaults */
		LangTag = if LangTag is null then "en-GB" else LangTag,
		FirstWeekDay = if FirstWeekDay is null then Day.Monday else FirstWeekDay,
		FinYearStartMo = if FinYearStartMo is null then 7 else FinYearStartMo,
		
		/* Variables */
		Today = Date.From(DateTime.LocalNow()),
		RollYear = Number.Round(365.25, 0),
		RollQuarter = Number.Round(RollYear / 4, 0),
		RollMonth = Number.Round(RollYear / 12, 0),

		/* Generate table spine based on a date column */
		DayCount = Duration.Days(Duration.From(EndDate - StartDate)) + 1,
		Source = List.Dates(StartDate, DayCount, #duration(1, 0, 0, 0)),
		TableFromList = Table.FromList(Source, Splitter.SplitByNothing()),
		ChangedType = Table.TransformColumnTypes(TableFromList, {{"Column1", type date}}),
		RenamedColumns = Table.RenameColumns(ChangedType,{{"Column1", "Date"}}),

		/* Date columns */
		D1 = Table.AddColumn(RenamedColumns, "Day of Month #", each Date.Day([Date]), Int64.Type),
		D2 = Table.AddColumn(D1, "Day of Month ##", each Date.ToText([Date], "dd", LangTag), type text),
		D3 = Table.AddColumn(D2, "Day of Week #", each Date.DayOfWeek([Date], Day.Monday) + 1, Int64.Type),
		D4 = Table.AddColumn(D3, "Day of Week ##", each Date.ToText([Date], "dd", LangTag), type text),
		D5 = Table.AddColumn(D4, "Day of Week @M", each Date.ToText([Date], "ddd", LangTag), type text),
		D6 = Table.AddColumn(D5, "Day of Week @L", each Date.DayOfWeekName([Date], LangTag), type text),
		D7 = Table.AddColumn(D6, "Day of Year #", each Date.DayOfYear([Date]), Int64.Type),
		D8 = Table.AddColumn(D7, "Day of Year ##", each Text.PadStart(Text.From(Date.DayOfYear([Date]), LangTag), 3, "0"), type text),

		W1 = Table.AddColumn(D8, "Week #", each Date.WeekOfYear([Date], FirstWeekDay), Int64.Type),
		W2 = Table.AddColumn(W1, "Week ##", each Text.PadStart(Text.From(Date.WeekOfYear([Date], FirstWeekDay), LangTag), 2, "0"), type text),
		W3 = Table.AddColumn(W2, "Week @M", each "W" & [#"Week ##"] , type text),
		W4 = Table.AddColumn(W3, "Week of Month #", each Date.WeekOfMonth([Date], FirstWeekDay), Int64.Type),
		W5 = Table.AddColumn(W4, "Week Start %", each Date.StartOfWeek([Date], FirstWeekDay), type date),
		W6 = Table.AddColumn(W5, "Week End %", each Date.EndOfWeek([Date], FirstWeekDay), type date),

		// Weeks are lengtend (>3 days) or shortenned (<4 days) to fit fully into calendar months
		W7 = Table.AddColumn(W6, "Week Constricted #", each
			if Date.StartOfWeek([Date], FirstWeekDay) < Date.StartOfMonth([Date])
				then if Duration.Days(Date.StartOfMonth([Date]) - Date.StartOfWeek([Date], FirstWeekDay)) < 4
					then Date.WeekOfYear([Date], FirstWeekDay) else Date.WeekOfYear(Date.AddWeeks([Date], 1), FirstWeekDay)
			else if Date.EndOfWeek([Date]) > Date.EndOfMonth([Date])
				then if Duration.Days(Date.EndOfWeek([Date], FirstWeekDay) - Date.EndOfMonth([Date])) < 4
					then Date.WeekOfYear([Date], FirstWeekDay) else Date.WeekOfYear(Date.AddWeeks([Date], -1), FirstWeekDay)
			else Date.WeekOfYear([Date], FirstWeekDay), Int64.Type),

		M1 = Table.AddColumn(W6, "Month #", each Date.Month([Date]), Int64.Type),
		M2 = Table.AddColumn(M1, "Month ##", each Date.ToText([Date], "MM", LangTag), type text),
		M3 = Table.AddColumn(M2, "Month @M", each Date.ToText([Date], "MMM", LangTag), type text),
		M4 = Table.AddColumn(M3, "Month @L", each Date.MonthName([Date], LangTag), type text),
		M5 = Table.AddColumn(M4, "Month Start %", each Date.StartOfMonth([Date]), type date),
		M6 = Table.AddColumn(M5, "Month End %", each Date.EndOfMonth([Date]), type date),

		Q1 = Table.AddColumn(M6, "Quarter #", each Date.QuarterOfYear([Date]), Int64.Type),
		Q2 = Table.AddColumn(Q1, "Quarter ##", each Text.PadStart(Text.From(Date.QuarterOfYear([Date]), LangTag), 2, "0"), type text),
		Q3 = Table.AddColumn(Q2, "Quarter Start %", each Date.StartOfQuarter([Date]), type date),
		Q4 = Table.AddColumn(Q3, "Quarter End %", each Date.EndOfQuarter([Date]), type date),

		Y1 = Table.AddColumn(Q4, "Year #", each Date.Year([Date]), Int64.Type),
		Y2 = Table.AddColumn(Y1, "Year @M", each Date.ToText([Date], "yy", LangTag), type text),

		/* Financial */
		F1 = Table.AddColumn(Y2, "Financial Year #", each if Date.Month([Date]) >= FinYearStartMo then Date.Year([Date]) else Date.Year([Date]) - 1, Int64.Type),
		F2 = Table.AddColumn(F1, "Financial Year @M", each "FY" & Text.End(Number.ToText([#"Financial Year #"], "D"), 2) & "/" & Text.End(Number.ToText([#"Financial Year #"] + 1, "D"), 2), type text),
		F3 = Table.AddColumn(F2, "Financial Quarter #", each Number.RoundUp((Date.Month([Date]) + ((FinYearStartMo - 1) * (if Date.Month([Date]) >= FinYearStartMo then -1 else 1))) / 3, 0), Int64.Type),
		F4 = Table.AddColumn(F3, "Financial Quarter @M", each "FQ" & Number.ToText([#"Financial Quarter #"], "D"), type text),
		F5 = Table.AddColumn(F4, "Financial Month #", each Date.Month([Date]) + ((FinYearStartMo - 1) * (if Date.Month([Date]) >= FinYearStartMo then -1 else 1)), Int64.Type),
		F6 = Table.AddColumn(F5, "Financial Month @M", each "FM" & Number.ToText([#"Financial Month #"], "D"), type text),

		/* ISO */
		// Get Thursday of each week, year of that Thursday and first Thursday of the year
			// As vars
		// CurrentThu = Date.AddDays([Date], 3 - Date.DayOfWeek([Date], Day.Monday)),
		// YearCurThu = Date.Year(CurrentThu),
		// FirstYrThu = Date.AddDays(#date(YearCurThu, 1, 7), - Date.DayOfWeek(#date(YearCurThu, 1, 1), Day.Friday)),
			// As columns
		I1 = Table.AddColumn(F6, "CurrentThu", each Date.AddDays([Date], 3 - Date.DayOfWeek([Date], Day.Monday)), type date),
		I2 = Table.AddColumn(I1, "YearCurThu", each Date.Year([CurrentThu]), Int64.Type), 
		I3 = Table.AddColumn(I2, "FirstYrThu", each Date.AddDays(#date([YearCurThu], 1, 7), - Date.DayOfWeek(#date([YearCurThu], 1, 1), Day.Friday)), type date),

		I4 = Table.AddColumn(I3, "ISO Date #", each Date.Year([Date]) * 10000 + Date.Month([Date]) * 100 + Date.Day([Date]), Int64.Type),
		// Count days between current week's Thursday and the first Thursday of the year
		I5 = Table.AddColumn(I4, "ISO Week #", each (Duration.Days([CurrentThu] - [FirstYrThu]) / 7) + 1, Int64.Type),
		I6 = Table.AddColumn(I5, "ISO Week Year #", each Date.Year([CurrentThu]), Int64.Type),
		I7 = Table.AddColumn(I6, "ISO Week Year #M", each [#"ISO Week Year #"] * 100 + [#"ISO Week #"], Int64.Type),
		I8 = Table.AddColumn(I7, "ISO Week Year @M", each Number.ToText([#"ISO Week Year #"]) & "-W" & Text.PadStart(Text.From([#"ISO Week #"]), 2, "0"), type text),	// 2022-W00
		I9 = Table.AddColumn(I8, "ISO Week Year @L", each [#"ISO Week Year @M"] & "-" & Text.PadStart(Text.From(Date.DayOfYear([Date])), 3, "0"), type text),	// 2022-W00-1

		I0 = Table.RemoveColumns(I9, {"CurrentThu", "YearCurThu", "FirstYrThu"}),

		/* Calendar 4-4-5 */
		Calendar445 = (WeekNo) => let 
			Periods = 
				{ 
					{(x)=>x< 5, [P=1, Q=1]}, 
					{(x)=>x< 9, [P=2, Q=1]}, 
					{(x)=>x<14, [P=3, Q=1]}, 
					{(x)=>x<18, [P=4, Q=2]}, 
					{(x)=>x<22, [P=5, Q=2]}, 
					{(x)=>x<27, [P=6, Q=2]}, 
					{(x)=>x<31, [P=7, Q=3]}, 
					{(x)=>x<35, [P=8, Q=3]}, 
					{(x)=>x<40, [P=9, Q=3]}, 
					{(x)=>x<44, [P=10,Q=4]}, 
					{(x)=>x<48, [P=11,Q=4]}, 
					{(x)=>true, [P=12,Q=4]} 
				}, 
			Result = List.First(List.Select(Periods, each _{0}(WeekNo))){1}
		in 
			Result,
		P1 = Table.ExpandRecordColumn(Table.AddColumn(I0, "Calendar445", each Calendar445([#"ISO Week #"]), type [P = Int64.Type, Q = Int64.Type]), "Calendar445", {"P", "Q"}, {"Period 445 #", "Period 445 Quarter #"}),

		/* Bool relative */
		L01 = Table.AddColumn(P1,  "Is in W-0", each Date.IsInCurrentWeek([Date]), type logical),
		L02 = Table.AddColumn(L01, "Is in W-1", each Date.IsInPreviousWeek([Date]), type logical),
		L03 = Table.AddColumn(L02, "Is in W-2", each Date.IsInPreviousNWeeks([Date], 2) and not Date.IsInPreviousNWeeks([Date], 1), type logical),

		L04 = Table.AddColumn(L03, "Is in M-0", each Date.IsInCurrentMonth([Date]), type logical),
		L05 = Table.AddColumn(L04, "Is in M-1", each Date.IsInPreviousMonth([Date]), type logical),
		L06 = Table.AddColumn(L05, "Is in M-2", each Date.IsInPreviousNMonths([Date], 2) and not Date.IsInPreviousNMonths([Date], 1), type logical),

		L07 = Table.AddColumn(L06, "Is in Q-0", each Date.IsInCurrentQuarter([Date]), type logical),
		L08 = Table.AddColumn(L07, "Is in Q-1", each Date.IsInPreviousQuarter([Date]), type logical),
		L09 = Table.AddColumn(L08, "Is in Q-2", each Date.IsInPreviousNQuarters([Date], 2) and not Date.IsInPreviousNQuarters([Date], 1), type logical),

		L10 = Table.AddColumn(L09, "Is in Y-0", each Date.IsInCurrentYear([Date]), type logical),
		L11 = Table.AddColumn(L10, "Is in Y-1", each Date.IsInPreviousYear([Date]), type logical),
		L12 = Table.AddColumn(L11, "Is in Y-2", each Date.IsInPreviousNYears([Date], 2) and not Date.IsInPreviousNYears([Date], 1), type logical),

		/* Bool rolling */
		L13 = Table.AddColumn(L12, "Is Rolling W-0", each Date.IsInPreviousNDays([Date], 7), type logical),
		L14 = Table.AddColumn(L13, "Is Rolling W-1", each Date.IsInPreviousNDays([Date], 7 * 2) and not Date.IsInPreviousNDays([Date], 7), type logical),

		L15 = Table.AddColumn(L14, "Is Rolling M-0", each Date.IsInPreviousNDays([Date], RollMonth), type logical),
		L16 = Table.AddColumn(L15, "Is Rolling M-1", each Date.IsInPreviousNDays([Date], RollMonth * 2) and not Date.IsInPreviousNDays([Date], RollMonth), type logical),

		L17 = Table.AddColumn(L16, "Is Rolling Q-0", each Date.IsInPreviousNDays([Date], RollQuarter), type logical),
		L18 = Table.AddColumn(L17, "Is Rolling Q-1", each Date.IsInPreviousNDays([Date], RollQuarter * 2) and not Date.IsInPreviousNDays([Date], RollQuarter), type logical),

		L19 = Table.AddColumn(L18, "Is Rolling Y-0", each Date.IsInPreviousNDays([Date], RollYear), type logical),
		L20 = Table.AddColumn(L19, "Is Rolling Y-1", each Date.IsInPreviousNDays([Date], RollYear * 2) and not Date.IsInPreviousNDays([Date], RollYear), type logical),

		/* Bool to date */
		L21 = Table.AddColumn(L20, "Is to Date Week", each Date.IsInCurrentWeek([Date]) and [Date] <= Today, type logical),
		L22 = Table.AddColumn(L21, "Is to Date Month", each Date.IsInCurrentMonth([Date]) and [Date] <= Today, type logical),
		L23 = Table.AddColumn(L22, "Is to Date Quarter", each Date.IsInCurrentQuarter([Date]) and [Date] <= Today, type logical),
		L24 = Table.AddColumn(L23, "Is to Date Year", each Date.IsInCurrentYear([Date]) and [Date] <= Today, type logical)
	in
		L24
in
	GetCalendar