/*/ GetCalendar \*****************************************\
|   _____     _    _      _____  _   _  ____     _    ____  
|  / ____|   / \  | |    | ____|| \ | ||  _ \   / \  |  _ \ 
|  | |      / _ \ | |    |  _|  |  \| || | | | / _ \ | |_) |
|  | |___  / ___ \| |___ | |___ | |\  || |_| |/ ___ \|  _ < 
|  \_____|/_/   \_|_____||_____||_| \_||____//_/   \_|_| \_\
| 
|-[ usage ]-----------------------------------------------|
|  Column reference tags                                  |
|    Abbr: Abbreviated form (Mon)                         |
|    Comp: Composite number (202501)                      |
|    Padd: Number padded with leading zeros (01)          |
|    Prfx: Number with a prefix (Q1)                      |
|-[ author ]----------------------------------------------|
|  2023, Filip Kraus, contact@filipkraus.net              |
|  2014, Matt Masson, mattmasson.com                      |
|-[ changelog ]-------------------------------------------|
|  2026-10-22  3.3.0  Add a configurable period calendar (PeriodPattern, PeriodWeek53)
|                     Add [Period Week], [Period Week of Month], [Period Year] and period start/end dates
|                     Change the period calendar to respect FirstWeekDay
|                     Remove [P445*] and [P445B*], replaced by [Period *]
|  2026-08-13  3.2.1  Fix [Week Side] on weeks that end on a month boundary
|  2026-03-25  3.2.0  Fix 445B using the wrong function
|                     Fix [Is Leap Day] missing day number
|                     Fix incorrect holiday regions
|                     Fix relative dates not respecting custom `Today`
|                     Fix [Week Constrained] missing `FirstWeekDay`
|                     Fix [Week Constrained *] using wrong type
|                     Fix [Is Weekday/end] shifting with `FirstWeekDay`
|                     Fix holidays to be null-safe
|                     Fix [Relative Calendar Weeks] missing `FirstWeekDay`
|                     Fix [Relative *] rounding to prevent +1 shift
|                     Change [Week Side] to lower case
|  2026-02-02  3.1.2  Fix [Is WTD] returning values based on local device culture
|  2025-11-10  3.1.1  Fix [Is in Week*], [Relative Weeks], [Relative Day of Week] returning incorrect values
|  2025-05-07  3.1.0  Add [ISO Week & Side*]
|                     Change names to include tags at the end
|                     Improve compatibility of [Day & Ordinal] (thanks Asma)
|                     Fix issue with [Week Side] for first/last week of year
|  2025-03-28  3.0.0  Change column names to be more user-friendly
|                     Add option to explicitly define current date
|                     Add sort by date
|                     Add [Day & Ordinal], [Year & Quarter]
|                     Add holiday fields & text for all GB regions
|                     Add [Relative Day,Week,Quarter of Week,Year]
|  2025-03-07  2.2.1  Add [Relative Month of Year] field
|  2025-02-28  2.2.2  Add previous relative calcs as rolling fields
|  2025-02-24  2.2.1  Fix relative fields not sticking to their period
|  2024-08-16  2.2.0  Change [P445 @M] from 'P445 ##' to 'Period ##'
|  2024-08-09  2.1.0  Add a variation (B) of the 4-4-5 calendar
|  2024-05-23  2.0.1  Fix relative years skipped in steps
|  2024-04-03  2.0.0  Add built-in documentation
|                     Change default date to UTC
|                     Change date params to optional with a week running default
|                     Change CultureCode default to Culture.Current
|  2024-03-18  1.7.0  Add [Relative Days/Weeks/Months/Quarters/Years]
|                     Add [Is Leap Year/Day]
|  2024-03-13  1.6.0  Add [Is Weekday/Weekend/Workday]
|                     Add GB holiday web source (optional  by parameter)
|  2023-12-11  1.5.6  Add [Is ≤ Today], [Is ≥ Today]
|                     Change field names [Is < Today], [Is > Today]
|  2023-10-30  1.5.5  Add [Week Year #, #M, @M, @L]
|                     Fix [Day of Week #] not using FirstWeekDay parameter
|                     Remove [Day of Week ##]
|  2023-10-26  1.5.4  Add [Date is Before Today], [Date is After Today]
|  2023-10-13  1.5.3  Change [ISO Week Year #] to [ISO Year #]
|                     Add [ISO Week Year #] as YYYYWW, [Month Year #], [Month Year @M]
|  2023-08-23  1.5.2  Add [Day & Weekday @M]
|                     Fix [Day of Week ##] showing day of month
|  2023-08-03  1.5.1  Add Quarter @M, @L
|  2023-05-25  1.5.0  Add Period 445 formats ##, @M
|  2023-05-22  1.4.1  Add ISO week formats ##, @M
|  2023-05-18  1.4.0  Fix last date (EndDate) not being generated
|                     Add forward-looking bool columns
|  2023-05-11  1.3.1  Add 'Week @M'
|  2023-04-06  1.3.0  Add financial, ISO, calendar 4-4-5
|                     Apply standardised column naming
\*/

let Calendar = 
	let Function = (
		optional StartDate as date,
		optional EndDate as date,
		optional CultureCode as nullable text,
		optional FirstWeekDay as nullable number,
		optional FinYearStartMo as nullable number,
		optional UseWebSources as nullable logical,
		optional CurrentDate as nullable date
	) as table => 
		let
			// Variables //
			Today = if CurrentDate <> null then CurrentDate else Date.From(DateTimeZone.UtcNow()),
			RollYear = Number.Round(365.25, 0),
			RollQuarter = Number.Round(RollYear / 4, 0),
			RollMonth = Number.Round(RollYear / 12, 0),

			// Defaults //
			StartDate = if StartDate <> null 			then StartDate 			else Date.AddDays(Today, -7),
			EndDate = if EndDate <> null 				then EndDate 			else Date.AddDays(Today, -0),
			CultureCode = if CultureCode <> null 		then CultureCode 		else Culture.Current,
			FirstWeekDay = if FirstWeekDay <> null 		then FirstWeekDay 		else Day.Monday,
			FinYearStartMo = if FinYearStartMo <> null 	then FinYearStartMo 	else 7,
			UseWebSources = if UseWebSources <> null 	then UseWebSources 		else false,

			// Generate table spine based on a date column //
			DayCount = Duration.TotalDays(Duration.From(EndDate - StartDate)) + 1,
			Source = List.Dates(StartDate, DayCount, #duration(1, 0, 0, 0)),
			TableFromList = Table.FromList(Source, Splitter.SplitByNothing()),
			ChangedType = Table.TransformColumnTypes(TableFromList, {{"Column1", type date}}),
			RenamedColumns = Table.RenameColumns(ChangedType,{{"Column1", "Date"}}),
			Cal = RenamedColumns,

			// GB Holidays - Uses online govt source //
			Holidays =  if not UseWebSources then Cal else let
				HolidaysGB = let
					Source = Json.Document(Web.Contents("https://www.gov.uk/bank-holidays.json")),
					Table = Record.ToTable(Source),
					KeepCols = Table.SelectColumns(Table,{"Value"}),
					ExpandRecs = Table.ExpandRecordColumn(KeepCols, "Value", {"division", "events"}, {"Country", "Events"}),
					ReplaceList = [#"england-and-wales" = "GB_EAW", #"scotland" = "GB_SCT", #"northern-ireland" = "GB_NIR"],
					ReplaceCountry = Table.TransformColumns(ExpandRecs, {{"Country", each Record.FieldOrDefault(ReplaceList, _, _)}}),
					ExpandEvents = Table.ExpandRecordColumn(Table.ExpandListColumn(ReplaceCountry, "Events"), "Events", {"date", "title"}, {"Date", "Title"}),
					Group = Table.Group(ExpandEvents, {"Date"}, {{"Holidays", each Table.ToRecords(Table.PromoteHeaders(Table.Transpose([[Country], [Title]]))){0}, type record}}),
					Type = Table.TransformColumnTypes(Group,{{"Date", type date}})

				in
					Type,
				Join = Table.NestedJoin(Cal, {"Date"}, HolidaysGB, {"Date"}, "HolidayTable", JoinKind.LeftOuter),
				Expand = Table.ExpandTableColumn(Join, "HolidayTable", {"Holidays"}, {"Holidays"})
			in
				Expand,
			Cx = Holidays,

			// Date columns //
			D1 = Table.AddColumn(Cx, "Day", each Date.Day([Date]), Int64.Type),
			D2 = Table.AddColumn(D1, "Day (Padd)", each Date.ToText([Date], "dd"), type text),
			// Ordinal = {"th", "st", "nd", "rd"},
			// D3 = Table.AddColumn(D2, "Day & Ordinal", each Text.From(Date.Day([Date])) & ((
			// 		if Text.End(Text.From(Number.IntegerDivide(Date.Day([Date]), 10)), 1) = "1"
			// 		then Ordinal{0}
			// 		else Ordinal{Number.Mod(Date.Day([Date]), 10)}?
			// 	)??Ordinal{0}), type text),
			Ordinal = {"th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th"},
			D3 = Table.AddColumn(D2, "Day & Ordinal", each let o = (x) => Text.From(x) & (if List.Contains({11,12,13}, x) then "th" else Ordinal{Number.Mod(x, 10)}) in o(Date.Day([Date])), type text),
			D4 = Table.AddColumn(D3, "Day of Week Number", each Date.DayOfWeek([Date], FirstWeekDay) + 1, Int64.Type),
			D5 = Table.AddColumn(D4, "Day of Week (Abbr)", each Date.ToText([Date], "ddd", CultureCode), type text),
			D6 = Table.AddColumn(D5, "Day of Week", each Date.DayOfWeekName([Date], CultureCode), type text),
			D7 = Table.AddColumn(D6, "Day of Year", each Date.DayOfYear([Date]), Int64.Type),
			D8 = Table.AddColumn(D7, "Day of Year (Padd)", each Text.PadStart(Text.From(Date.DayOfYear([Date]), CultureCode), 3, "0"), type text),
			Dx = D8,

			E1 = Table.AddColumn(Dx, "Day & Weekday (Abbr)", each Date.ToText([Date], "dd") & ", " & Date.ToText([Date], "ddd", CultureCode), type text),
			Ex = E1,

			W1 = Table.AddColumn(Ex, "Week", each Date.WeekOfYear([Date], FirstWeekDay), Int64.Type),
			W2 = Table.AddColumn(W1, "Week (Padd)", each Text.PadStart(Text.From(Date.WeekOfYear([Date], FirstWeekDay)), 2, "0"), type text),
			W3 = Table.AddColumn(W2, "Week (Prfx)", each "W" & Text.PadStart(Text.From(Date.WeekOfYear([Date], FirstWeekDay)), 2, "0"), type text),
			W4 = Table.AddColumn(W3, "Week of Month", each Date.WeekOfMonth([Date], FirstWeekDay), Int64.Type),
			W5 = Table.AddColumn(W4, "Week Start Date", each Date.StartOfWeek([Date], FirstWeekDay), type date),
			W6 = Table.AddColumn(W5, "Week End Date", each Date.EndOfWeek([Date], FirstWeekDay), type date),
			W7 = Table.AddColumn(W6, "Year & Week (Comp)", each Date.Year([Date]) * 100 + Date.WeekOfYear([Date], FirstWeekDay), Int64.Type),
			W8 = Table.AddColumn(W7, "Year & Week", each Text.From(Date.Year([Date])) & "-W" & Text.PadStart(Text.From(Date.WeekOfYear([Date], FirstWeekDay)), 2, "0"), type text),	// 2022-W01
			W9 = Table.AddColumn(W8, "Year & Week & Day of Year", each Text.From(Date.Year([Date])) & "-W" & Text.PadStart(Text.From(Date.WeekOfYear([Date], FirstWeekDay)), 2, "0") & "-" & Text.PadStart(Text.From(Date.DayOfYear([Date])), 3, "0"), type text),	// 2022-W01-001
			Wx = W9,

			// Weeks are lengthened (>3 days) or shortened (<4 days) to fit fully into calendar months
			V1 = Table.AddColumn(Wx, "Week Constrained", each
				if Date.StartOfWeek([Date], FirstWeekDay) < Date.StartOfMonth([Date])
					then if Duration.Days(Date.StartOfMonth([Date]) - Date.StartOfWeek([Date], FirstWeekDay)) < 4
						then Date.WeekOfYear([Date], FirstWeekDay) else Date.WeekOfYear(Date.AddWeeks([Date], 1), FirstWeekDay)
				else if Date.EndOfWeek([Date], FirstWeekDay) > Date.EndOfMonth([Date])
					then if Duration.Days(Date.EndOfWeek([Date], FirstWeekDay) - Date.EndOfMonth([Date])) < 4
						then Date.WeekOfYear([Date], FirstWeekDay) else Date.WeekOfYear(Date.AddWeeks([Date], -1), FirstWeekDay)
				else Date.WeekOfYear([Date], FirstWeekDay), Int64.Type),
			V2 = Table.AddColumn(V1, "Week Constrained (Padd)", each Text.PadStart(Text.From([Week Constrained]), 2, "0"), type text),
			V3 = Table.AddColumn(V2, "Week Constrained (Prfx)", each "W" & [#"Week Constrained (Padd)"], type text),
			V4 = Table.AddColumn(V3, "Week Side", each (
				if Date.Month([Week Start Date]) <> Date.Month([Week End Date])
					and Date.Year([Week Start Date]) = Date.Year([Week End Date])
				then if Date.Month([Date]) = Date.Month([Week Start Date]) then "a" else "b"
				else null), type text),
			V5 = Table.AddColumn(V4, "Week & Side (Prfx)", each [#"Week (Prfx)"] & (if [Week Side] = null then "" else [Week Side]), type text),
			Vx = V5,

			M1 = Table.AddColumn(Vx, "Month Number", each Date.Month([Date]), Int64.Type),
			M2 = Table.AddColumn(M1, "Month Number (Padd)", each Date.ToText([Date], "MM"), type text),
			M3 = Table.AddColumn(M2, "Month Name (Abbr)", each Date.ToText([Date], "MMM", CultureCode), type text),
			M4 = Table.AddColumn(M3, "Month Name", each Date.MonthName([Date], CultureCode), type text),
			M5 = Table.AddColumn(M4, "Year & Month (Comp)", each Date.Year([Date]) * 100 + Date.Month([Date]), Int64.Type),
			M6 = Table.AddColumn(M5, "Year & Month", each Text.From(Date.Year([Date])) & "-" & Date.ToText([Date], "MM"), type text),
			M7 = Table.AddColumn(M6, "Month Start Date", each Date.StartOfMonth([Date]), type date),
			M8 = Table.AddColumn(M7, "Month End Date", each Date.EndOfMonth([Date]), type date),

			Q1 = Table.AddColumn(M8, "Quarter", each Date.QuarterOfYear([Date]), Int64.Type),
			Q2 = Table.AddColumn(Q1, "Quarter (Prfx)", each "Q" & Text.From(Date.QuarterOfYear([Date])), type text),
			Q3 = Table.AddColumn(Q2, "Quarter Start Date", each Date.StartOfQuarter([Date]), type date),
			Q4 = Table.AddColumn(Q3, "Quarter End Date", each Date.EndOfQuarter([Date]), type date),
			Q5 = Table.AddColumn(Q4, "Year & Quarter (Comp)", each Date.Year([Date]) * 100 + Date.QuarterOfYear([Date]), Int64.Type),
			Q6 = Table.AddColumn(Q5, "Year & Quarter", each Text.From(Date.Year([Date])) & "-Q" & Text.From(Date.QuarterOfYear([Date])), type text),
			Qx = Q6,

			Y1 = Table.AddColumn(Qx, "Year", each Date.Year([Date]), Int64.Type),
			Y2 = Table.AddColumn(Y1, "Year (Abbr)", each "'" & Date.ToText([Date], "yy", CultureCode), type text),
			Yx = Y2,

			// Financial //
			F1 = Table.AddColumn(Yx, "Financial Year", each if Date.Month([Date]) >= FinYearStartMo then Date.Year([Date]) else Date.Year([Date]) - 1, Int64.Type),
			F2 = Table.AddColumn(F1, "Financial Year (Prfx)", each "FY" & Text.End(Text.From([Financial Year]), 2) & "/" & Text.End(Text.From([Financial Year] + 1), 2), type text),
			F3 = Table.AddColumn(F2, "Financial Quarter", each Number.RoundUp((Date.Month([Date]) + ((FinYearStartMo - 1) * (if Date.Month([Date]) >= FinYearStartMo then -1 else 1))) / 3, 0), Int64.Type),
			F4 = Table.AddColumn(F3, "Financial Quarter (Prfx)", each "FQ" & Text.From([Financial Quarter]), type text),
			F5 = Table.AddColumn(F4, "Financial Month", each Date.Month([Date]) + ((FinYearStartMo - 1) * (if Date.Month([Date]) >= FinYearStartMo then -1 else 1)), Int64.Type),
			F6 = Table.AddColumn(F5, "Financial Month (Prfx)", each "FM" & Text.PadStart(Text.From([Financial Month]), 2, "0"), type text),
			Fx = F6,

			// ISO 8601 //
			// Get Thursday of each week, year of that Thursday and first Thursday of the year
				// As vars
			// CurrentThu = Date.AddDays([Date], 3 - Date.DayOfWeek([Date], Day.Monday)),
			// YearCurThu = Date.Year(CurrentThu),
			// FirstYrThu = Date.AddDays(#date(YearCurThu, 1, 7), - Date.DayOfWeek(#date(YearCurThu, 1, 1), Day.Friday)),
				// As columns
			IH1 = Table.AddColumn(Fx, "CurrentThu", each Date.AddDays([Date], 3 - Date.DayOfWeek([Date], Day.Monday)), type date),
			IH2 = Table.AddColumn(IH1, "YearCurThu", each Date.Year([CurrentThu]), Int64.Type), 
			IH3 = Table.AddColumn(IH2, "FirstYrThu", each Date.AddDays(#date([YearCurThu], 1, 7), - Date.DayOfWeek(#date([YearCurThu], 1, 1), Day.Friday)), type date),

			I1 = Table.AddColumn(IH3, "Year & Month & Day (Comp)", each Date.Year([Date]) * 10000 + Date.Month([Date]) * 100 + Date.Day([Date]), Int64.Type),
			I2 = Table.AddColumn(I1, "ISO Week", each (Duration.Days([CurrentThu] - [FirstYrThu]) / 7) + 1, Int64.Type), // Days between current week's Thursday and the first Thursday of the year
			I3 = Table.AddColumn(I2, "ISO Week (Padd)", each Text.PadStart(Text.From([ISO Week]), 2, "0"), type text),
			I4 = Table.AddColumn(I3, "ISO Week (Prfx)", each "W" & [#"ISO Week (Padd)"], type text),
			I5 = Table.AddColumn(I4, "ISO Week & Side (Prfx)", each [#"ISO Week (Prfx)"] & (if [Week Side] = null then "" else [Week Side]), type text),
			I6 = Table.AddColumn(I5, "ISO Year", each Date.Year([CurrentThu]), Int64.Type),
			I7 = Table.AddColumn(I6, "ISO Year & Week (Comp)", each [ISO Year] * 100 + [ISO Week], Int64.Type),
			I8 = Table.AddColumn(I7, "ISO Year & Week", each Text.From([ISO Year]) & "-W" & Text.PadStart(Text.From([ISO Week]), 2, "0"), type text), // 2022-W00
			IHx = Table.RemoveColumns(I8, {"CurrentThu", "YearCurThu", "FirstYrThu"}),

			// Period calendar //
			// Weeks per period, grouped by quarter. The outer list repeats until it fills 52 weeks.
			//   {{4,4,5}}                               4-4-5
			//   {{4,5,4}}                               4-5-4
			//   {{4,4,5},{4,4,5},{4,5,4},{4,5,4}}       mixed / full-year definition
			PeriodPattern = {{4,4,5}},
			PeriodWeek53 = null,   // period number that absorbs week 53; null = last period

			PeriodPatternIsLists = Value.Is(PeriodPattern, type list) and List.Count(PeriodPattern) > 0 and List.MatchesAll(PeriodPattern, each Value.Is(_, type list) and List.Count(_) > 0),
			PeriodPatternFlat = if PeriodPatternIsLists then List.Combine(PeriodPattern) else {},
			PeriodPatternValuesOk = PeriodPatternIsLists and List.MatchesAll(PeriodPatternFlat, each Value.Is(_, type number) and _ > 0 and _ = Number.RoundDown(_, 0)),
			PeriodPatternTotal = if PeriodPatternValuesOk then List.Sum(PeriodPatternFlat) else 0,
			PeriodPatternSpansYear = PeriodPatternValuesOk and PeriodPatternTotal > 0 and Number.Mod(52, PeriodPatternTotal) = 0,
			PeriodCount = if PeriodPatternSpansYear then Number.IntegerDivide(52, PeriodPatternTotal) * List.Count(PeriodPatternFlat) else 0,
			PeriodWeek53Ok = PeriodWeek53 = null or (PeriodPatternSpansYear and Value.Is(PeriodWeek53, type number) and PeriodWeek53 >= 1 and PeriodWeek53 <= PeriodCount and PeriodWeek53 = Number.RoundDown(PeriodWeek53, 0)),
			PeriodCheck =
				if not PeriodPatternIsLists then error Error.Record("Expression.Error", "Period pattern must be a list of non-empty quarter lists", [Pattern = PeriodPattern])
				else if not PeriodPatternValuesOk then error Error.Record("Expression.Error", "Period pattern values must be positive whole numbers of weeks", [Pattern = PeriodPattern])
				else if not PeriodPatternSpansYear then error Error.Record("Expression.Error", "Period pattern must total 52 weeks (or a divisor of 52)", [Pattern = PeriodPattern, Total = PeriodPatternTotal])
				else if not PeriodWeek53Ok then error Error.Record("Expression.Error", "PeriodWeek53 must be null or a period number from 1 to " & Text.From(PeriodCount), [PeriodWeek53 = PeriodWeek53, PeriodCount = PeriodCount])
				else true,

			PeriodRepeated = List.Repeat(PeriodPattern, Number.IntegerDivide(52, PeriodPatternTotal)),
			PeriodWeeks52 = List.Combine(PeriodRepeated),
			PeriodQuarters52 = List.Combine(List.Transform({0..(List.Count(PeriodRepeated) - 1)}, (i) => List.Repeat({i + 1}, List.Count(PeriodRepeated{i})))),
			PeriodWeek53At = if PeriodWeek53 = null then List.Count(PeriodWeeks52) else PeriodWeek53,
			PeriodWeeks53 = List.Transform({0..(List.Count(PeriodWeeks52) - 1)}, (i) => PeriodWeeks52{i} + (if i + 1 = PeriodWeek53At then 1 else 0)),

			PeriodBuildWeekMap = (weekCounts as list, quarters as list) as list =>
				let
					n = List.Count(weekCounts),
					PeriodStarts = List.FirstN(List.Accumulate(weekCounts, {1}, (state, w) => state & {List.Last(state) + w}), n),
					Periods = List.Accumulate(
						{0..(n - 1)},
						{},
						(state, i) =>
							let
								q = quarters{i},
								prev = if List.IsEmpty(state) then null else List.Last(state),
								qStart = if prev = null or prev[Quarter] <> q then PeriodStarts{i} else prev[QuarterStartWk]
							in
								state & {[
									Month = i + 1,
									Quarter = q,
									MonthStartWk = PeriodStarts{i},
									MonthWks = weekCounts{i},
									QuarterStartWk = qStart
								]}
					),
					WithQuarterWks = List.Transform(
						Periods,
						(p) => Record.AddField(
							p,
							"QuarterWks",
							List.Sum(List.Transform(List.Select(Periods, each [Quarter] = p[Quarter] and [QuarterStartWk] = p[QuarterStartWk]), each [MonthWks]))
						)
					),
					Map = List.Combine(List.Transform(WithQuarterWks, (p) =>
						List.Transform({1..p[MonthWks]}, (w) => [
							Month = p[Month],
							Quarter = p[Quarter],
							WeekOfMonth = w,
							MonthStartWk = p[MonthStartWk],
							MonthWks = p[MonthWks],
							QuarterStartWk = p[QuarterStartWk],
							QuarterWks = p[QuarterWks]
						])
					))
				in
					Map,

			// Built once, before any calendar rows. An invalid pattern errors here.
			PeriodWeekMap52 = if PeriodCheck then PeriodBuildWeekMap(PeriodWeeks52, PeriodQuarters52) else {},
			PeriodWeekMap53 = if PeriodCheck then PeriodBuildWeekMap(PeriodWeeks53, PeriodQuarters52) else {},
			P0 = if PeriodCheck and List.Count(PeriodWeekMap52) = 52 and List.Count(PeriodWeekMap53) = 53 then IHx else error Error.Record("Expression.Error", "Period week map did not cover the year", [Weeks52 = List.Count(PeriodWeekMap52), Weeks53 = List.Count(PeriodWeekMap53)]),

			PeriodYearStart = (y as number) as date => Date.StartOfWeek(#date(y, 1, 4), FirstWeekDay),
			PeriodInfo = (d as date) as record =>
				let
					WkStart = Date.StartOfWeek(d, FirstWeekDay),
					PY = Date.Year(Date.AddDays(WkStart, 3)),
					YS = PeriodYearStart(PY),
					YE = Date.AddDays(PeriodYearStart(PY + 1), -1),
					Wk = Number.IntegerDivide(Duration.Days(WkStart - YS), 7) + 1,
					YearDays = Duration.Days(YE - YS) + 1,
					Map = if YearDays = 371 then PeriodWeekMap53 else if YearDays = 364 then PeriodWeekMap52 else error Error.Record("Expression.Error", "Period year is not 52 or 53 weeks", [Year = PY, Days = YearDays]),
					R = Map{Wk - 1},
					MonthPadd = Text.PadStart(Text.From(R[Month]), 2, "0"),
					WeekPadd = Text.PadStart(Text.From(Wk), 2, "0")
				in [
					Year = Int64.From(PY),
					YearStart = YS,
					YearEnd = YE,
					Quarter = Int64.From(R[Quarter]),
					QuarterPrfx = "PQ" & Text.From(R[Quarter]),
					QuarterStart = Date.AddDays(YS, (R[QuarterStartWk] - 1) * 7),
					QuarterEnd = Date.AddDays(YS, (R[QuarterStartWk] - 1 + R[QuarterWks]) * 7 - 1),
					Month = Int64.From(R[Month]),
					MonthPadd = MonthPadd,
					MonthPrfx = "P" & MonthPadd,
					MonthStart = Date.AddDays(YS, (R[MonthStartWk] - 1) * 7),
					MonthEnd = Date.AddDays(YS, (R[MonthStartWk] - 1 + R[MonthWks]) * 7 - 1),
					Week = Int64.From(Wk),
					WeekPadd = WeekPadd,
					WeekPrfx = "PW" & WeekPadd,
					WeekOfMonth = Int64.From(R[WeekOfMonth]),
					YearQuarterComp = Int64.From(PY * 100 + R[Quarter]),
					YearQuarter = Text.From(PY) & "-PQ" & Text.From(R[Quarter]),
					YearMonthComp = Int64.From(PY * 100 + R[Month]),
					YearMonth = Text.From(PY) & "-P" & MonthPadd,
					YearWeekComp = Int64.From(PY * 100 + Wk),
					YearWeek = Text.From(PY) & "-PW" & WeekPadd
				],
			PeriodType = type [
				Year = Int64.Type,
				YearStart = date,
				YearEnd = date,
				Quarter = Int64.Type,
				QuarterPrfx = text,
				QuarterStart = date,
				QuarterEnd = date,
				Month = Int64.Type,
				MonthPadd = text,
				MonthPrfx = text,
				MonthStart = date,
				MonthEnd = date,
				Week = Int64.Type,
				WeekPadd = text,
				WeekPrfx = text,
				WeekOfMonth = Int64.Type,
				YearQuarterComp = Int64.Type,
				YearQuarter = text,
				YearMonthComp = Int64.Type,
				YearMonth = text,
				YearWeekComp = Int64.Type,
				YearWeek = text
			],
			P1 = Table.ExpandRecordColumn(
				Table.AddColumn(P0, "Period", each PeriodInfo([Date]), PeriodType),
				"Period",
				{"Year", "YearStart", "YearEnd", "Quarter", "QuarterPrfx", "QuarterStart", "QuarterEnd", "Month", "MonthPadd", "MonthPrfx", "MonthStart", "MonthEnd", "Week", "WeekPadd", "WeekPrfx", "WeekOfMonth", "YearQuarterComp", "YearQuarter", "YearMonthComp", "YearMonth", "YearWeekComp", "YearWeek"},
				{"Period Year", "Period Year Start Date", "Period Year End Date", "Period Quarter", "Period Quarter (Prfx)", "Period Quarter Start Date", "Period Quarter End Date", "Period Month", "Period Month (Padd)", "Period Month (Prfx)", "Period Month Start Date", "Period Month End Date", "Period Week", "Period Week (Padd)", "Period Week (Prfx)", "Period Week of Month", "Period Year & Quarter (Comp)", "Period Year & Quarter", "Period Year & Month (Comp)", "Period Year & Month", "Period Year & Week (Comp)", "Period Year & Week"}
			),
			Px = P1,

			// Relative calendar counts //
			T1 = Table.AddColumn(Px, "Relative Days", each Duration.TotalDays([Date] - Today), Int64.Type),
			T2 = Table.AddColumn(T1, "Relative Calendar Weeks", each (Date.Year([Date]) * 52 + Date.WeekOfYear([Date], FirstWeekDay)) - (Date.Year(Today) * 52 + Date.WeekOfYear(Today, FirstWeekDay)), Int64.Type),
			T3 = Table.AddColumn(T2, "Relative Calendar Months", each (Date.Year([Date]) * 12 + Date.Month([Date])) - (Date.Year(Today) * 12 + Date.Month(Today)), Int64.Type),
			T4 = Table.AddColumn(T3, "Relative Calendar Quarters", each (Date.Year([Date]) * 4 + Date.QuarterOfYear([Date])) - (Date.Year(Today) * 4 + Date.QuarterOfYear(Today)), Int64.Type),
			T5 = Table.AddColumn(T4, "Relative Calendar Years", each Date.Year([Date]) - Date.Year(Today), Int64.Type),
			// Relative rolling counts //
			T6 = Table.AddColumn(T5, "Relative Weeks", each Number.IntegerDivide(Number.From(Date.StartOfWeek([Date], FirstWeekDay)) - Number.From(Date.StartOfWeek(Today, FirstWeekDay)), 7), Int64.Type),
			T7 = Table.AddColumn(T6, "Relative Months", each Number.Round(Value.Divide(Duration.TotalDays([Date] - Today), (31+28.25+31+30+31+30+31+31+30+31+30+31)/12), 0), Int64.Type),
			T8 = Table.AddColumn(T7, "Relative Quarters", each Number.Round(Value.Divide(Duration.TotalDays([Date] - Today), (31+28.25+31+30+31+30+31+31+30+31+30+31)/4), 0), Int64.Type),
			T9 = Table.AddColumn(T8, "Relative Years", each Number.Round(Value.Divide(Duration.TotalDays([Date] - Today), 365.25), 0), Int64.Type),
			Tx = T9,

			// Relative comparative counts //
			U1 = Table.AddColumn(Tx, "Relative Day of Week", each let d = Date.DayOfWeek([Date], FirstWeekDay) - Date.DayOfWeek(Today, FirstWeekDay) in d + (if d < 0 then 7 else 0), Int64.Type),
			U2 = Table.AddColumn(U1, "Relative Day of Year", each Date.DayOfYear([Date]) - Date.DayOfYear(Today), Int64.Type),
			U3 = Table.AddColumn(U2, "Relative Week of Year", each Date.WeekOfYear([Date], FirstWeekDay) - Date.WeekOfYear(Today, FirstWeekDay), Int64.Type),
			U4 = Table.AddColumn(U3, "Relative Month of Year", each Date.Month([Date]) - Date.Month(Today), Int64.Type),
			U5 = Table.AddColumn(U4, "Relative Quarter of Year", each Date.QuarterOfYear([Date]) - Date.QuarterOfYear(Today), Int64.Type),
			Ux = U5,

			// Helper functions (relative to Today / CurrentDate — not host Date.IsIn*) //
			IsInRelativeWeek = (RelWeek, Offset) => RelWeek = Offset,
			IsInRelativeMonth = (d, Offset) => Date.StartOfMonth(d) = Date.StartOfMonth(Date.AddMonths(Today, Offset)),
			IsInRelativeQuarter = (d, Offset) => Date.StartOfQuarter(d) = Date.StartOfQuarter(Date.AddQuarters(Today, Offset)),
			IsInRelativeYear = (d, Offset) => Date.Year(d) = Date.Year(Today) + Offset,
			IsInPreviousNDays = (d, N) => d >= Date.AddDays(Today, -N) and d < Today,
			IsInNextNDays = (d, N) => d > Today and d <= Date.AddDays(Today, N),

			// Bool to date //
			LT1 = Table.AddColumn(Ux, "Is WTD", each IsInRelativeWeek([Relative Weeks], 0) and [Date] <= Today, type logical),
			LT2 = Table.AddColumn(LT1, "Is MTD", each IsInRelativeMonth([Date], 0) and [Date] <= Today, type logical),
			LT3 = Table.AddColumn(LT2, "Is QTD", each IsInRelativeQuarter([Date], 0) and [Date] <= Today, type logical),
			LT4 = Table.AddColumn(LT3, "Is YTD", each IsInRelativeYear([Date], 0) and [Date] <= Today, type logical),

			// Bool relative //
			LR1 = Table.AddColumn(LT4, "Is Before Today", each [Date] < Today, type logical),
			LR2 = Table.AddColumn(LR1, "Is On or Before Today", each [Date] <= Today, type logical),
			LR3 = Table.AddColumn(LR2, "Is After Today", each [Date] > Today, type logical),
			LR4 = Table.AddColumn(LR3, "Is On or After Today", each [Date] >= Today, type logical),

			LD1 = Table.AddColumn(LR4, "Is in Day -2", each [Date] = Date.AddDays(Today, -2), type logical),
			LD2 = Table.AddColumn(LD1, "Is in Day -1", each [Date] = Date.AddDays(Today, -1), type logical),
			LD3 = Table.AddColumn(LD2, "Is in Today", each [Date] = Today, type logical),
			LD4 = Table.AddColumn(LD3, "Is in Day +1", each [Date] = Date.AddDays(Today, 1), type logical),
			LD5 = Table.AddColumn(LD4, "Is in Day +2", each [Date] = Date.AddDays(Today, 2), type logical),
			LDx = LD5,

			LW1 = Table.AddColumn(LDx, "Is in Week -2", each IsInRelativeWeek([Relative Weeks], -2), type logical),
			LW2 = Table.AddColumn(LW1, "Is in Week -1", each IsInRelativeWeek([Relative Weeks], -1), type logical),
			LW3 = Table.AddColumn(LW2, "Is in This Week", each IsInRelativeWeek([Relative Weeks], 0), type logical),
			LW4 = Table.AddColumn(LW3, "Is in Week +1", each IsInRelativeWeek([Relative Weeks], 1), type logical),
			LW5 = Table.AddColumn(LW4, "Is in Week +2", each IsInRelativeWeek([Relative Weeks], 2), type logical),

			// DO NOT USE - Doesn't account for week start day //
			// LW1 = Table.AddColumn(LD5, "Is in Week -2", each Date.IsInPreviousNWeeks([Date], 2) and not Date.IsInPreviousNWeeks([Date], 1), type logical),
			// LW2 = Table.AddColumn(LW1, "Is in Week -1", each Date.IsInPreviousWeek([Date]), type logical),
			// LW3 = Table.AddColumn(LW2, "Is in This Week", each Date.IsInCurrentWeek([Date]), type logical),
			// LW4 = Table.AddColumn(LW3, "Is in Week +1", each Date.IsInNextWeek([Date]), type logical),
			// LW5 = Table.AddColumn(LW4, "Is in Week +2", each Date.IsInNextNWeeks([Date], 2) and not Date.IsInNextNWeeks([Date], 1), type logical),

			LM1 = Table.AddColumn(LW5, "Is in Month -2", each IsInRelativeMonth([Date], -2), type logical),
			LM2 = Table.AddColumn(LM1, "Is in Month -1", each IsInRelativeMonth([Date], -1), type logical),
			LM3 = Table.AddColumn(LM2, "Is in This Month", each IsInRelativeMonth([Date], 0), type logical),
			LM4 = Table.AddColumn(LM3, "Is in Month +1", each IsInRelativeMonth([Date], 1), type logical),
			LM5 = Table.AddColumn(LM4, "Is in Month +2", each IsInRelativeMonth([Date], 2), type logical),

			LQ1 = Table.AddColumn(LM5, "Is in Quarter -2", each IsInRelativeQuarter([Date], -2), type logical),
			LQ2 = Table.AddColumn(LQ1, "Is in Quarter -1", each IsInRelativeQuarter([Date], -1), type logical),
			LQ3 = Table.AddColumn(LQ2, "Is in This Quarter", each IsInRelativeQuarter([Date], 0), type logical),
			LQ4 = Table.AddColumn(LQ3, "Is in Quarter +1", each IsInRelativeQuarter([Date], 1), type logical),
			LQ5 = Table.AddColumn(LQ4, "Is in Quarter +2", each IsInRelativeQuarter([Date], 2), type logical),

			LY1 = Table.AddColumn(LQ5, "Is in Year -2", each IsInRelativeYear([Date], -2), type logical),
			LY2 = Table.AddColumn(LY1, "Is in Year -1", each IsInRelativeYear([Date], -1), type logical),
			LY3 = Table.AddColumn(LY2, "Is in This Year", each IsInRelativeYear([Date], 0), type logical),
			LY4 = Table.AddColumn(LY3, "Is in Year +1", each IsInRelativeYear([Date], 1), type logical),
			LY5 = Table.AddColumn(LY4, "Is in Year +2", each IsInRelativeYear([Date], 2), type logical),
			LYx = LY5,

			LH1 = Table.AddColumn(LYx, "Is Leap Year", each Date.IsLeapYear([Date]), type logical),
			LH2 = Table.AddColumn(LH1, "Is Leap Day", each Date.IsLeapYear([Date]) and (Date.Month([Date]) = 2) and (Date.Day([Date]) = 29), type logical),
			LH3 = Table.AddColumn(LH2, "Is Weekday", each Date.DayOfWeek([Date], Day.Monday) <= 4, type logical),
			LH4 = Table.AddColumn(LH3, "Is Weekend", each Date.DayOfWeek([Date], Day.Monday) >= 5, type logical),
			LHx = LH4,

			LK0 = LHx,
			GB_EAW1 = Table.AddColumn(LK0, "Is Holiday (GB-EAW)", each [Holidays]?[GB_EAW]? <> null, type logical),
			GB_EAW2 = Table.AddColumn(GB_EAW1, "Holiday (GB-EAW)", each [Holidays]?[GB_EAW]?, type text),
			GB_EAW3 = Table.AddColumn(GB_EAW2, "Is Workday (GB-EAW)", each ([Holidays]?[GB_EAW]? = null) and (Date.DayOfWeek([Date], Day.Monday) <= 4), type logical),
			GB_SCT1 = Table.AddColumn(GB_EAW3, "Is Holiday (GB-SCT)", each [Holidays]?[GB_SCT]? <> null, type logical),
			GB_SCT2 = Table.AddColumn(GB_SCT1, "Holiday (GB-SCT)", each [Holidays]?[GB_SCT]?, type text),
			GB_SCT3 = Table.AddColumn(GB_SCT2, "Is Workday (GB-SCT)", each ([Holidays]?[GB_SCT]? = null) and (Date.DayOfWeek([Date], Day.Monday) <= 4), type logical),
			GB_NIR1 = Table.AddColumn(GB_SCT3, "Is Holiday (GB-NIR)", each [Holidays]?[GB_NIR]? <> null, type logical),
			GB_NIR2 = Table.AddColumn(GB_NIR1, "Holiday (GB-NIR)", each [Holidays]?[GB_NIR]?, type text),
			GB_NIR3 = Table.AddColumn(GB_NIR2, "Is Workday (GB-NIR)", each ([Holidays]?[GB_NIR]? = null) and (Date.DayOfWeek([Date], Day.Monday) <= 4), type logical),
			LKx = if UseWebSources then GB_NIR3 else LK0,

			// Binary rolling //
			RW1 = Table.AddColumn(LKx, "Is in Rolling Week -1", each IsInPreviousNDays([Date], 7 * 2) and not IsInPreviousNDays([Date], 7), type logical),
			RW2 = Table.AddColumn(RW1, "Is in Rolling Week", each IsInPreviousNDays([Date], 7), type logical),
			RW3 = Table.AddColumn(RW2, "Is in Rolling Week +1", each IsInNextNDays([Date], 7 * 2) and not IsInNextNDays([Date], 7), type logical),

			RM1 = Table.AddColumn(RW3, "Is in Rolling Month -1", each IsInPreviousNDays([Date], RollMonth * 2) and not IsInPreviousNDays([Date], RollMonth), type logical),
			RM2 = Table.AddColumn(RM1, "Is in Rolling Month", each IsInPreviousNDays([Date], RollMonth), type logical),
			RM3 = Table.AddColumn(RM2, "Is in Rolling Month +1", each IsInNextNDays([Date], RollMonth * 2) and not IsInNextNDays([Date], RollMonth), type logical),

			RQ1 = Table.AddColumn(RM3, "Is in Rolling Quarter -1", each IsInPreviousNDays([Date], RollQuarter * 2) and not IsInPreviousNDays([Date], RollQuarter), type logical),
			RQ2 = Table.AddColumn(RQ1, "Is in Rolling Quarter", each IsInPreviousNDays([Date], RollQuarter), type logical),
			RQ3 = Table.AddColumn(RQ2, "Is in Rolling Quarter +1", each IsInNextNDays([Date], RollQuarter * 2) and not IsInNextNDays([Date], RollQuarter), type logical),

			RY1 = Table.AddColumn(RQ3, "Is in Rolling Year -1", each IsInPreviousNDays([Date], RollYear * 2) and not IsInPreviousNDays([Date], RollYear), type logical),
			RY2 = Table.AddColumn(RY1, "Is in Rolling Year", each IsInPreviousNDays([Date], RollYear), type logical),
			RY3 = Table.AddColumn(RY2, "Is in Rolling Year+1", each IsInNextNDays([Date], RollYear * 2) and not IsInNextNDays([Date], RollYear), type logical),
			RYx = RY3,

			Out = Table.Sort(RYx, {{"Date", Order.Descending}})
		in
			Out,
		FunctionType = type function (
			optional StartDate as (type date meta [
				Documentation.FieldCaption = "Start date",
				Documentation.FieldDescription = "Date to start calendar from (inclusive)",
				Documentation.SampleValues = {#date(Date.Year(DateTimeZone.UtcNow()), 1, 1)}
			]),
			optional EndDate as (type date meta [
				Documentation.FieldCaption = "End date",
				Documentation.FieldDescription = "Date to end calendar at (inclusive)",
				Documentation.SampleValues = {#date(Date.Year(DateTimeZone.UtcNow()), 12, 31)}
			]),
			optional CultureCode as (type text meta [
				Documentation.FieldCaption = "Culture code",
				Documentation.FieldDescription = "May affect number or date/time formats",
				Documentation.AllowedValues = {"en-GB", "en-US", "af", "am", "ar-sa", "as", "az-Latn", "be", "bg", "bn-BD", "bn-IN", "bs", "ca", "ca-ES-valencia", "cs", "cy", "da", "de", "de-de", "el", "es", "es-ES", "es-US", "es-MX", "et", "eu", "fa", "fi", "fil-Latn", "fr", "fr-FR", "fr-CA", "ga", "gd-Latn", "gl", "gu", "ha-Latn", "he", "hi", "hr", "hu", "hy", "id", "ig-Latn", "is", "it", "it-it", "ja", "ka", "kk", "km", "kn", "ko", "kok", "ku-Arab", "ky-Cyrl", "lb", "lt", "lv", "mi-Latn", "mk", "ml", "mn-Cyrl", "mr", "ms", "mt", "nb", "ne", "nl", "nl-BE", "nn", "nso", "or", "pa", "pa-Arab", "pl", "prs-Arab", "pt-BR", "pt-PT", "qut-Latn", "quz", "ro", "ru", "rw", "sd-Arab", "si", "sk", "sl", "sq", "sr-Cyrl-BA", "sr-Cyrl-RS", "sr-Latn-RS", "sv", "sw", "ta", "te", "tg-Cyrl", "th", "ti", "tk-Latn", "tn", "tr", "tt-Cyrl", "ug-Arab", "uk", "ur", "uz-Latn", "vi", "wo", "xh", "yo-Latn", "zh-Hans", "zh-Hant", "zu"},
				Documentation.SampleValues = {"en-GB", "en-US"}
			]),
			optional FirstWeekDay as (type number meta [
				Documentation.FieldCaption = "Week start",
				Documentation.FieldDescription = "First day of week",
				Documentation.AllowedValues = {Day.Monday, Day.Tuesday, Day.Wednesday, Day.Thursday, Day.Friday, Day.Saturday, Day.Sunday},
				Documentation.SampleValues = {Day.Monday, Day.Sunday},
				Formatting.IsCode = false
			]),
			optional FinYearStartMo as (type number meta [
				Documentation.FieldCaption = "Financial year start",
				Documentation.FieldDescription = "Month of year which starts new financial year",
				Documentation.AllowedValues = {7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6},
				Documentation.SampleValues = {7}
			]),
			optional UseWebSources as (type logical meta [
				Documentation.FieldCaption = "Web sources",
				Documentation.FieldDescription = "Enable web sources, such as for holidays",
				Documentation.AllowedValues = {true, false},
				Documentation.SampleValues = {true}
			]),
			optional CurrentDate as (type date meta [
				Documentation.FieldCaption = "Current Date",
				Documentation.FieldDescription = "Allows explicit definition of current date",
				Documentation.SampleValues = {Date.From(DateTimeZone.UtcNow())}
			])
			) as any meta [
				Documentation.Name = "Calendar",
				Documentation.LongDescription = "Returns a Calendar table.",
				Documentation.Examples = {
					[Description = "Basic usage", Code = "Calendar(null, null, null, null, null, null, null)", Result = "Calendar using all default values."],
					[Description = "Typical usage", Code = "Calendar(RangeFrom, RangeTo, ""en-GB"", null, null, true, null)", Result = "Calendar using date range params and web holiday source."],
					[Description = "Explicit default values", Code = "Calendar(Date.AddDays(Date.From(DateTime.LocalNow()), -8), Date.AddDays(Date.From(DateTime.LocalNow()), -1), Culture.Current, Day.Monday, 7, false, null)", Result = "Calendar using all default parameter values explicitly."]
				}
			],
		TypedFunction = Value.ReplaceType(Function, FunctionType)
	in
		TypedFunction
in
	Calendar