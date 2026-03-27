/*************************************************************\
|=[ SPLIT TABLE DATE/TIME
|  Applied to a table, splits select date/time columns
|  to individual date & time columns.
|  
|-[ Version: 1.0.0 (2026-03-27)
|-[ Usage:
|  Refer to the built-in function documentation.
|  
\*/

let SplitTableDateTime =
	let Function = (
		Table as table,
		Columns as list,
		optional Replace as logical,
		optional DateSuffix as text,
		optional TimeSuffix as text
	) as table =>
		
		let
			// Parameter defaults
			Replace = if Replace = null then true else Replace,
			DateSuffix = if DateSuffix = null then "_Date" else DateSuffix,
			TimeSuffix = if TimeSuffix = null then "_Time" else TimeSuffix,

			SplitColumns =
				List.Accumulate(
					Columns,
					Table,
					(state, colName) =>
						let
							DateCol = Table.AddColumn(
								state,
								colName & DateSuffix,
								each Date.From(Record.Field(_, colName)),
								type date
							),

							TimeCol = Table.AddColumn(
								DateCol,
								colName & TimeSuffix,
								each Time.From(Record.Field(_, colName)),
								type time
							),

							Cleaned =
								if Replace then
									Table.RemoveColumns(TimeCol, {colName})
								else
									TimeCol
						in
							Cleaned
				),
			
			Output = SplitColumns
		in
			Output,

		FunctionType = type function (
			Table as (type table meta [
				Documentation.FieldCaption = "Table",
				Documentation.FieldDescription = "Table to process"
			]),
			Columns as (type list meta [
				Documentation.FieldCaption = "Column list",
				Documentation.FieldDescription = "List of date/time columns to split"
			]),
			optional Replace as (type logical meta [
				Documentation.FieldCaption = "Replace columns",
				Documentation.FieldDescription = "Defaults to true; whether original columns should be removed"
			]),
			optional DateSuffix as (type text meta [
				Documentation.FieldCaption = "Date column suffix",
				Documentation.FieldDescription = "Defaults to '_Date'; appended to original column name"
			]),
			optional TimeSuffix as (type text meta [
				Documentation.FieldCaption = "Time column suffix",
				Documentation.FieldDescription = "Defaults to '_Time'; appended to original column name"
			])
			) as any meta [
				Documentation.Name = "Split Date/Time",
				Documentation.LongDescription = "Returns a table with date/time columns split into individual date & time columns"
			],
		TypedFunction = Value.ReplaceType(Function, FunctionType)
	in
		TypedFunction
in
	SplitTableDateTime