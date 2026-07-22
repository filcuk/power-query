/*/ GetErrors \*******************************************\
|  Return error values with messages, where possible.     |
|-[ author ]----------------------------------------------|
|  Original: community.fabric.microsoft.com (td-p/1470730)|
|  Modified: 2023, Filip Kraus, contact@filipkraus.net    |
|-[ changelog ]-------------------------------------------|
|  2026-07-22  1.3.0  Add documentation type
|                     Make ReturnAll optional
|  2024-01-25  1.2.0  Add parameter to allow returning full table
|  2023-11-15  1.1.0  Fix Excel formula errors showing as null
|  2023-08-01  1.0.0  Initial
\*/

let GetErrors =
	let Function = (
		Table as table,
		optional ReturnAll as nullable logical
	) as table =>
		let
			ReturnAll = if ReturnAll = null then false else ReturnAll,

			errorExtract = (value as any) as any => 
				if value[HasError] = true
				then value[Error]
				else value[Value],

			errorToText = (errorRec as any) as any =>
				if Type.Is(Value.Type(errorRec), Record.Type)
				then "Reason: " & errorRec[Reason]
				& "#(lf)Message: " & errorRec[Message]
				& "#(lf)Value: " & (
					if errorRec[Detail] is null then "null"
					else
						try Text.From(errorRec[Detail])
						otherwise try Text.From(errorRec[Detail][Value])
						otherwise null
				)
				// Return other values
				else errorRec,

			recsToErrors = (rec as record) as any =>
				List.Accumulate(
					Record.FieldNames(rec), [], (s, c) => 
						Record.AddField(s, c, 
							errorToText(
								errorExtract(try Record.Field(rec, c))
							)
						) 
				),

			OutputErrors = 
				Table.FromList(
					List.Transform(
						Table.ToRecords(Table.SelectRowsWithErrors(Table)), each recsToErrors(_)
					), Record.FieldValues, Table.ColumnNames(Table)
				),
			
			OutputAll = 
				Table.FromList(
					List.Transform(
						Table.ToRecords(Table), each recsToErrors(_)
					), Record.FieldValues, Table.ColumnNames(Table)
				),
			
			Output = if ReturnAll then OutputAll else OutputErrors
		in
			Output,

		FunctionType = type function (
			Table as (type table meta [
				Documentation.FieldCaption = "Table",
				Documentation.FieldDescription = "Table to scan for cell errors"
			]),
			optional ReturnAll as (type logical meta [
				Documentation.FieldCaption = "Return all rows",
				Documentation.FieldDescription = "Defaults to false; when true, returns every row with errors expanded to text where present",
				Documentation.AllowedValues = {true, false},
				Documentation.SampleValues = {false}
			])
			) as table meta [
				Documentation.Name = "Get Errors",
				Documentation.LongDescription = "Returns rows with error values expanded to reason/message text where possible."
			],
		TypedFunction = Value.ReplaceType(Function, FunctionType)
	in
		TypedFunction
in
	GetErrors
