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
| 	2023-04-06, 1.3.0
| 		Add financial, ISO, calendar 4-4-5
| 		Apply standardised column naming
|	2023-05-11, 1.3.1
|		Add 'Week @M'
| 	2023-05-18, 1.4.0
| 		Fix last date (EndDate) not being generated
| 		Add forward-looking bool columns
| 	2023-05-22, 1.4.1
| 		Add ISO week formats ##, @M
|	2023-05-25, 1.5.0
| 		Add Period 445 formats ##, @M
| 	2023-08-03, 1.5.1
|		Add Quarter @M, @L
| 	2023-08-23, 1.5.2
| 		Add [Day & Weekday @M]
| 		Fix [Day of Week ##] showing day of month
| 	2023-10-13, 1.5.3
|		Change [ISO Week Year #] to [ISO Year #]
| 		Add [ISO Week Year #] as YYYYWW, [Month Year #], [Month Year @M]
|	2023-10-26, 1.5.4
| 		Add [Date is Before Today], [Date is After Today]
| 	2023-10-30, 1.5.5
| 		Add [Week Year #, #M, @M, @L]
| 		Fix [Day of Week #] not using FirstWeekDay parameter
| 		Remove [Day of Week ##]
| 	2023-12-11, 1.5.6
|		Add [Is ≤ Today], [Is ≥ Today]
|		Change field names [Is < Today], [Is > Today]
|	2024-03-13, 1.6.0
| 		Add [Is Weekday/Weekend/Workday]
| 		Add GB holiday source (web)
\*********************************************************/

let GetCalendar = (
	StartDate as date,
	EndDate as date,
	optional LangTag as nullable text,
	optional FirstWeekDay as nullable number,
	optional FinYearStartMo as nullable number,
	optional GetWebHolidays as nullable logical
) as table =>
	let
		// Defaults //
		LangTag = if LangTag is null then "en-GB" else LangTag,
		FirstWeekDay = if FirstWeekDay is null then Day.Monday else FirstWeekDay,
		FinYearStartMo = if FinYearStartMo is null then 7 else FinYearStartMo,
		GetWebHolidays = if GetWebHolidays is null then false else GetWebHolidays,

		// Variables //
		Today = Date.From(DateTime.LocalNow()),
		RollYear = Number.Round(365.25, 0),
		RollQuarter = Number.Round(RollYear / 4, 0),
		RollMonth = Number.Round(RollYear / 12, 0),

		// Generate table spine based on a date column //
		DayCount = Duration.Days(Duration.From(EndDate - StartDate)) + 1,
		Source = List.Dates(StartDate, DayCount, #duration(1, 0, 0, 0)),
		TableFromList = Table.FromList(Source, Splitter.SplitByNothing()),
		ChangedType = Table.TransformColumnTypes(TableFromList, {{"Column1", type date}}),
		RenamedColumns = Table.RenameColumns(ChangedType,{{"Column1", "Date"}}),
		Cal = RenamedColumns,

		// GB holidays - England & Wales //
		Holidays =  if not GetWebHolidays then Cal else let
			HolidaysGB = let
				Source = Json.Document(Web.Contents("https://www.gov.uk/bank-holidays.json")),
				Table = Record.ToTable(Source),
				KeepCols = Table.SelectColumns(Table,{"Value"}),
				ExpandRecs = Table.ExpandRecordColumn(KeepCols, "Value", {"division", "events"}, {"Country", "Events"}),
				ReplaceList = [#"england-and-wales" = "GB-EAW", #"scotland" = "GB-SCT", #"northern-ireland" = "GB-NIR"],
				ReplaceCountry = Table.TransformColumns(ExpandRecs, {{"Country", each Record.FieldOrDefault(ReplaceList, _, _)}}),
				ExpandEvents = Table.ExpandRecordColumn(Table.ExpandListColumn(ReplaceCountry, "Events"), "Events", {"date", "title"}, {"Date", "Title"}),
				Group = Table.Group(ExpandEvents, {"Date"}, {{"Holiday", each [[Country], [Title]], type table [Country=text, Title=text]}}),
				Type = Table.TransformColumnTypes(Group,{{"Date", type date}})

			in
				Type,
			// Join = Table.NestedJoin(Cal, {"Date"}, Table.SelectRows(HolidaysGB, each [country] = "GB-EAW"), {"date"}, "holidays-gb", JoinKind.LeftOuter),
			Join = Table.NestedJoin(Cal, {"Date"}, HolidaysGB, {"Date"}, "HolidayTable", JoinKind.LeftOuter),
			Expand = Table.ExpandTableColumn(Join, "HolidayTable", {"Holiday"}, {"Holiday"})
		in
			Expand,
		CalCols = Holidays,

		// Date columns //
		D1 = Table.AddColumn(CalCols, "Day of Month #", each Date.Day([Date]), Int64.Type),
		D2 = Table.AddColumn(D1, "Day of Month ##", each Date.ToText([Date], "dd", LangTag), type text),
		D3 = Table.AddColumn(D2, "Day of Week #", each Date.DayOfWeek([Date], FirstWeekDay) + 1, Int64.Type),
		D4 = Table.AddColumn(D3, "Day of Week @M", each Date.ToText([Date], "ddd", LangTag), type text),
		D5 = Table.AddColumn(D4, "Day of Week @L", each Date.DayOfWeekName([Date], LangTag), type text),
		D6 = Table.AddColumn(D5, "Day of Year #", each Date.DayOfYear([Date]), Int64.Type),
		D7 = Table.AddColumn(D6, "Day of Year ##", each Text.PadStart(Text.From(Date.DayOfYear([Date]), LangTag), 3, "0"), type text),

		E1 = Table.AddColumn(D7, "Day & Weekday @M", each [#"Day of Month ##"] & ", " & [#"Day of Week @M"], type text),

		W1 = Table.AddColumn(E1, "Week #", each Date.WeekOfYear([Date], FirstWeekDay), Int64.Type),
		W2 = Table.AddColumn(W1, "Week ##", each Text.PadStart(Text.From([#"Week #"]), 2, "0"), type text),
		W3 = Table.AddColumn(W2, "Week @M", each "W" & [#"Week ##"], type text),
		W4 = Table.AddColumn(W3, "Week of Month #", each Date.WeekOfMonth([Date], FirstWeekDay), Int64.Type),
		W5 = Table.AddColumn(W4, "Week Start %", each Date.StartOfWeek([Date], FirstWeekDay), type date),
		W6 = Table.AddColumn(W5, "Week End %", each Date.EndOfWeek([Date], FirstWeekDay), type date),
		W7 = Table.AddColumn(W6, "Week Year #", each Text.From(Date.Year([Date])) & Text.PadStart(Text.From([#"Week #"]), 2, "0"), type text),
		W8 = Table.AddColumn(W7, "Week Year #M", each Date.Year([Date]) * 100 + [#"Week #"], Int64.Type),
		W9 = Table.AddColumn(W8, "Week Year @M", each Number.ToText(Date.Year([Date])) & "-W" & Text.PadStart(Text.From([#"Week #"]), 2, "0"), type text),	// 2022-W00
		W0 = Table.AddColumn(W9, "Week Year @L", each [#"Week Year @M"] & "-" & Text.PadStart(Text.From(Date.DayOfYear([Date])), 3, "0"), type text),	// 2022-W00-1
		
		// Weeks are lengtend (>3 days) or shortenned (<4 days) to fit fully into calendar months
		V1 = Table.AddColumn(W0, "Week Constricted #", each
			if Date.StartOfWeek([Date], FirstWeekDay) < Date.StartOfMonth([Date])
				then if Duration.Days(Date.StartOfMonth([Date]) - Date.StartOfWeek([Date], FirstWeekDay)) < 4
					then Date.WeekOfYear([Date], FirstWeekDay) else Date.WeekOfYear(Date.AddWeeks([Date], 1), FirstWeekDay)
			else if Date.EndOfWeek([Date]) > Date.EndOfMonth([Date])
				then if Duration.Days(Date.EndOfWeek([Date], FirstWeekDay) - Date.EndOfMonth([Date])) < 4
					then Date.WeekOfYear([Date], FirstWeekDay) else Date.WeekOfYear(Date.AddWeeks([Date], -1), FirstWeekDay)
			else Date.WeekOfYear([Date], FirstWeekDay), Int64.Type),

		M1 = Table.AddColumn(V1, "Month #", each Date.Month([Date]), Int64.Type),
		M2 = Table.AddColumn(M1, "Month ##", each Date.ToText([Date], "MM", LangTag), type text),
		M3 = Table.AddColumn(M2, "Month @M", each Date.ToText([Date], "MMM", LangTag), type text),
		M4 = Table.AddColumn(M3, "Month @L", each Date.MonthName([Date], LangTag), type text),
		M5 = Table.AddColumn(M4, "Month Year #", each Date.Year([Date]) * 100 + Date.Month([Date]), Int64.Type),
		M6 = Table.AddColumn(M5, "Month Year @M", each Text.From(Date.Year([Date])) & "-" & [#"Month ##"], type text),
		M7 = Table.AddColumn(M6, "Month Start %", each Date.StartOfMonth([Date]), type date),
		M8 = Table.AddColumn(M7, "Month End %", each Date.EndOfMonth([Date]), type date),

		Q1 = Table.AddColumn(M8, "Quarter #", each Date.QuarterOfYear([Date]), Int64.Type),
		Q2 = Table.AddColumn(Q1, "Quarter ##", each Text.PadStart(Text.From(Date.QuarterOfYear([Date]), LangTag), 2, "0"), type text),
		Q3 = Table.AddColumn(Q2, "Quarter @M", each "Q" & Text.From([#"Quarter #"]), type text),
		Q4 = Table.AddColumn(Q3, "Quarter @L", each "Q" & [#"Quarter ##"], type text),
		Q5 = Table.AddColumn(Q4, "Quarter Start %", each Date.StartOfQuarter([Date]), type date),
		Q6 = Table.AddColumn(Q5, "Quarter End %", each Date.EndOfQuarter([Date]), type date),

		Y1 = Table.AddColumn(Q6, "Year #", each Date.Year([Date]), Int64.Type),
		Y2 = Table.AddColumn(Y1, "Year @M", each Date.ToText([Date], "yy", LangTag), type text),

		// Financial //
		F1 = Table.AddColumn(Y2, "Financial Year #", each if Date.Month([Date]) >= FinYearStartMo then Date.Year([Date]) else Date.Year([Date]) - 1, Int64.Type),
		F2 = Table.AddColumn(F1, "Financial Year @M", each "FY" & Text.End(Number.ToText([#"Financial Year #"], "D"), 2) & "/" & Text.End(Number.ToText([#"Financial Year #"] + 1, "D"), 2), type text),
		F3 = Table.AddColumn(F2, "Financial Quarter #", each Number.RoundUp((Date.Month([Date]) + ((FinYearStartMo - 1) * (if Date.Month([Date]) >= FinYearStartMo then -1 else 1))) / 3, 0), Int64.Type),
		F4 = Table.AddColumn(F3, "Financial Quarter @M", each "FQ" & Number.ToText([#"Financial Quarter #"], "D"), type text),
		F5 = Table.AddColumn(F4, "Financial Month #", each Date.Month([Date]) + ((FinYearStartMo - 1) * (if Date.Month([Date]) >= FinYearStartMo then -1 else 1)), Int64.Type),
		F6 = Table.AddColumn(F5, "Financial Month @M", each "FM" & Number.ToText([#"Financial Month #"], "D"), type text),

		// ISO //
		// Get Thursday of each week, year of that Thursday and first Thursday of the year
			// As vars
		// CurrentThu = Date.AddDays([Date], 3 - Date.DayOfWeek([Date], Day.Monday)),
		// YearCurThu = Date.Year(CurrentThu),
		// FirstYrThu = Date.AddDays(#date(YearCurThu, 1, 7), - Date.DayOfWeek(#date(YearCurThu, 1, 1), Day.Friday)),
			// As columns
		IH1 = Table.AddColumn(F6, "CurrentThu", each Date.AddDays([Date], 3 - Date.DayOfWeek([Date], Day.Monday)), type date),
		IH2 = Table.AddColumn(IH1, "YearCurThu", each Date.Year([CurrentThu]), Int64.Type), 
		IH3 = Table.AddColumn(IH2, "FirstYrThu", each Date.AddDays(#date([YearCurThu], 1, 7), - Date.DayOfWeek(#date([YearCurThu], 1, 1), Day.Friday)), type date),

		I1 = Table.AddColumn(IH3, "ISO Date #", each Date.Year([Date]) * 10000 + Date.Month([Date]) * 100 + Date.Day([Date]), Int64.Type),
		// Count days between current week's Thursday and the first Thursday of the year
		I2 = Table.AddColumn(I1, "ISO Week #", each (Duration.Days([CurrentThu] - [FirstYrThu]) / 7) + 1, Int64.Type),
		I3 = Table.AddColumn(I2, "ISO Week ##", each Text.PadStart(Text.From([#"ISO Week #"]), 2, "0"), type text),
		I4 = Table.AddColumn(I3, "ISO Week @M", each "W" & [#"ISO Week ##"], type text),
		I5 = Table.AddColumn(I4, "ISO Year #", each Date.Year([CurrentThu]), Int64.Type),
		I6 = Table.AddColumn(I5, "ISO Week Year #", each Text.From([#"ISO Year #"]) & Text.PadStart(Text.From([#"ISO Week #"]), 2, "0"), type text),
		I7 = Table.AddColumn(I6, "ISO Week Year #M", each [#"ISO Year #"] * 100 + [#"ISO Week #"], Int64.Type),
		I8 = Table.AddColumn(I7, "ISO Week Year @M", each Number.ToText([#"ISO Year #"]) & "-W" & Text.PadStart(Text.From([#"ISO Week #"]), 2, "0"), type text),	// 2022-W00
		I9 = Table.AddColumn(I8, "ISO Week Year @L", each [#"ISO Week Year @M"] & "-" & Text.PadStart(Text.From(Date.DayOfYear([Date])), 3, "0"), type text),	// 2022-W00-1

		IH0 = Table.RemoveColumns(I9, {"CurrentThu", "YearCurThu", "FirstYrThu"}),

		// Calendar 4-4-5 //
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
		P1 = Table.ExpandRecordColumn(Table.AddColumn(IH0, "Calendar445", each Calendar445([#"ISO Week #"]), type [P = Int64.Type, Q = Int64.Type]), "Calendar445", {"P", "Q"}, {"P445 #", "P445 Quarter #"}),
		P2 = Table.AddColumn(P1, "P445 ##", each Text.PadStart(Text.From([#"P445 #"]), 2, "0"), type text),
		P3 = Table.AddColumn(P2, "P445 @M", each "P445 " & [#"P445 ##"], type text),
		P4 = Table.AddColumn(P3, "P445 Quarter @M", each "P445 Q" & Text.From([#"P445 Quarter #"]), type text),

		// Bool to date //
		LT1 = Table.AddColumn(P4, "Is to Date Week", each Date.IsInCurrentWeek([Date]) and [Date] <= Today, type logical),
		LT2 = Table.AddColumn(LT1, "Is to Date Month", each Date.IsInCurrentMonth([Date]) and [Date] <= Today, type logical),
		LT3 = Table.AddColumn(LT2, "Is to Date Quarter", each Date.IsInCurrentQuarter([Date]) and [Date] <= Today, type logical),
		LT4 = Table.AddColumn(LT3, "Is to Date Year", each Date.IsInCurrentYear([Date]) and [Date] <= Today, type logical),

		// Bool relative //
		LR1 = Table.AddColumn(LT4, "Is < Today", each [Date] < Today, type logical),
		LR2 = Table.AddColumn(LR1, "Is ≤ Today", each [Date] <= Today, type logical),
		LR3 = Table.AddColumn(LR2, "Is > Today", each [Date] > Today, type logical),
		LR4 = Table.AddColumn(LR3, "Is ≥ Today", each [Date] >= Today, type logical),

		LD1 = Table.AddColumn(LR4, "Is in D-2", each Date.IsInPreviousNDays([Date], 2) and not Date.IsInPreviousNDays([Date], 1), type logical),
		LD2 = Table.AddColumn(LD1, "Is in D-1", each Date.IsInPreviousDay([Date]), type logical),
		LD3 = Table.AddColumn(LD2, "Is in D-0", each Date.IsInCurrentDay([Date]), type logical),
		LD4 = Table.AddColumn(LD3, "Is in D+1", each Date.IsInNextDay([Date]), type logical),
		LD5 = Table.AddColumn(LD4, "Is in D+2", each Date.IsInNextNDays([Date], 2) and not Date.IsInNextNDays([Date], 1), type logical),

		LW1 = Table.AddColumn(LD5, "Is in W-2", each Date.IsInPreviousNWeeks([Date], 2) and not Date.IsInPreviousNWeeks([Date], 1), type logical),
		LW2 = Table.AddColumn(LW1, "Is in W-1", each Date.IsInPreviousWeek([Date]), type logical),
		LW3 = Table.AddColumn(LW2, "Is in W-0", each Date.IsInCurrentWeek([Date]), type logical),
		LW4 = Table.AddColumn(LW3, "Is in W+1", each Date.IsInNextWeek([Date]), type logical),
		LW5 = Table.AddColumn(LW4, "Is in W+2", each Date.IsInNextNWeeks([Date], 2) and not Date.IsInNextNWeeks([Date], 1), type logical),

		// LI1 = Table.AddColumn(LW5, "Is in I-2", each Date.IsInPreviousNWeeks([Date], 2) and not Date.IsInPreviousNWeeks([Date], 1), type logical),
		// LI2 = Table.AddColumn(LI1, "Is in I-1", each Date.IsInPreviousWeek([Date]), type logical),
		// LI3 = Table.AddColumn(LI2, "Is in I-0", each Date.IsInCurrentWeek([Date]), type logical),
		// LI4 = Table.AddColumn(LI3, "Is in I+1", each Date.IsInNextWeek([Date]), type logical),
		// LI5 = Table.AddColumn(LI4, "Is in I+2", each Date.IsInNextNWeeks([Date], 2) and not Date.IsInNextNWeeks([Date], 1), type logical),

		LM1 = Table.AddColumn(LW5, "Is in M-2", each Date.IsInPreviousNMonths([Date], 2) and not Date.IsInPreviousNMonths([Date], 1), type logical),
		LM2 = Table.AddColumn(LM1, "Is in M-1", each Date.IsInPreviousMonth([Date]), type logical),
		LM3 = Table.AddColumn(LM2, "Is in M-0", each Date.IsInCurrentMonth([Date]), type logical),
		LM4 = Table.AddColumn(LM3, "Is in M+1", each Date.IsInNextMonth([Date]), type logical),
		LM5 = Table.AddColumn(LM4, "Is in M+2", each Date.IsInNextNMonths([Date], 2) and not Date.IsInNextNMonths([Date], 1), type logical),

		LQ1 = Table.AddColumn(LM5, "Is in Q-2", each Date.IsInPreviousNQuarters([Date], 2) and not Date.IsInPreviousNQuarters([Date], 1), type logical),
		LQ2 = Table.AddColumn(LQ1, "Is in Q-1", each Date.IsInPreviousQuarter([Date]), type logical),
		LQ3 = Table.AddColumn(LQ2, "Is in Q-0", each Date.IsInCurrentQuarter([Date]), type logical),
		LQ4 = Table.AddColumn(LQ3, "Is in Q+1", each Date.IsInNextQuarter([Date]), type logical),
		LQ5 = Table.AddColumn(LQ4, "Is in Q+2", each Date.IsInNextNQuarters([Date], 2) and not Date.IsInNextNQuarters([Date], 1), type logical),

		LY1 = Table.AddColumn(LQ5, "Is in Y-2", each Date.IsInPreviousNYears([Date], 2) and not Date.IsInPreviousNYears([Date], 1), type logical),
		LY2 = Table.AddColumn(LY1, "Is in Y-1", each Date.IsInPreviousYear([Date]), type logical),
		LY3 = Table.AddColumn(LY2, "Is in Y-0", each Date.IsInCurrentYear([Date]), type logical),
		LY4 = Table.AddColumn(LY3, "Is in Y+1", each Date.IsInNextYear([Date]), type logical),
		LY5 = Table.AddColumn(LY4, "Is in Y+2", each Date.IsInNextNYears([Date], 2) and not Date.IsInNextNYears([Date], 1), type logical),

		LH1 = Table.AddColumn(LY5, "Is Weekday", each Date.DayOfWeek([Date], Day.Monday) <= 4, type logical),
		LH2 = Table.AddColumn(LH1, "Is Weekend", each Date.DayOfWeek([Date], Day.Monday) >= 5, type logical),
		// LH3 = Table.AddColumn(LH2, "Is Holiday", each if GetWebHolidays then [#"Holiday (GB-EAW)"] <> null else null, type logical),
		// LH3_1 = Table.AddColumn(LH3, "Is Holiday (GB-EAW)", each if GetWebHolidays then [#"Holiday (GB-EAW)"] <> null else null, type logical),
		// LH4 = Table.AddColumn(LH3_1, "Is Workday", each not [Is Holiday], type logical),
		// LH4_1 = Table.AddColumn(LH4, "Is Workday (GB-EAW)", each if GetWebHolidays then ([#"Holiday (GB-EAW)"] = null) and (Date.DayOfWeek([Date], Day.Monday) <= 4) else null, type logical),
		// TEST1 = Table.AddColumn(LH4_1, "TEST1", each ),

		// Binary rolling //
		RW1 = Table.AddColumn(LH2, "Is Rolling W-1", each Date.IsInPreviousNDays([Date], 7 * 2) and not Date.IsInPreviousNDays([Date], 7), type logical),
		RW2 = Table.AddColumn(RW1, "Is Rolling W-0", each Date.IsInPreviousNDays([Date], 7), type logical),
		RW3 = Table.AddColumn(RW2, "Is Rolling W+1", each Date.IsInNextNDays([Date], 7 * 2) and not Date.IsInNextNDays([Date], 7), type logical),

		RM1 = Table.AddColumn(RW3, "Is Rolling M-1", each Date.IsInPreviousNDays([Date], RollMonth * 2) and not Date.IsInPreviousNDays([Date], RollMonth), type logical),
		RM2 = Table.AddColumn(RM1, "Is Rolling M-0", each Date.IsInPreviousNDays([Date], RollMonth), type logical),
		RM3 = Table.AddColumn(RM2, "Is Rolling M+1", each Date.IsInNextNDays([Date], RollMonth * 2) and not Date.IsInNextNDays([Date], RollMonth), type logical),

		RQ1 = Table.AddColumn(RM3, "Is Rolling Q-1", each Date.IsInPreviousNDays([Date], RollQuarter * 2) and not Date.IsInPreviousNDays([Date], RollQuarter), type logical),
		RQ2 = Table.AddColumn(RQ1, "Is Rolling Q-0", each Date.IsInPreviousNDays([Date], RollQuarter), type logical),
		RQ3 = Table.AddColumn(RQ2, "Is Rolling Q+1", each Date.IsInNextNDays([Date], RollQuarter * 2) and not Date.IsInNextNDays([Date], RollQuarter), type logical),

		RY1 = Table.AddColumn(RQ3, "Is Rolling Y-1", each Date.IsInPreviousNDays([Date], RollYear * 2) and not Date.IsInPreviousNDays([Date], RollYear), type logical),
		RY2 = Table.AddColumn(RY1, "Is Rolling Y-0", each Date.IsInPreviousNDays([Date], RollYear), type logical),
		RY3 = Table.AddColumn(RY2, "Is Rolling Y+1", each Date.IsInNextNDays([Date], RollYear * 2) and not Date.IsInNextNDays([Date], RollYear), type logical)
	in
		RY3
in
	GetCalendar