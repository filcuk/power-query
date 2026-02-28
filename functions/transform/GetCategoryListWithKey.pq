/*/ Get Category List with Key \**************************\
|  Converts a concatenated category column                |
|  to a distinct list with link to the original record.   |
|-[ author ]----------------------------------------------|
|  2026, Filip Kraus, contact@filipkraus.net              |
|-[ changelog ]-------------------------------------------|
|  2026-02-28  1.0.0  Initial
\*/

let GetCategoryListWithKey =
	let Function = (
		Table as table,
		KeyColumn as text,
		ItemColumn as text,
		Delimiter as text
	) as table =>

        let
            Source = Table,
            Columns = Table.SelectColumns(Source,{KeyColumn, ItemColumn}),
            RenameTemp = Table.RenameColumns(Columns,{{ItemColumn, "Temp"}}),
            FilterNull = Table.SelectRows(RenameTemp, each ([Temp] <> null)),
            AddList = Table.AddColumn(FilterNull, ItemColumn, each Text.Split([Temp], Delimiter)),
            RemoveTemp = Table.RemoveColumns(AddList,{"Temp"}),
            ExpandCategory = Table.ExpandListColumn(RemoveTemp, ItemColumn),
            Type = Table.TransformColumnTypes(ExpandCategory,{{ItemColumn, type text}}),
            Distinct = Table.Distinct(Type),
            Sort = Table.Sort(Distinct,{{KeyColumn, Order.Descending}})
        in
            Sort,

		FunctionType = type function (
			Table as (type table meta [
				Documentation.FieldCaption = "Source table",
				Documentation.FieldDescription = "Table to extract data from"
			]),
			KeyColumn as (type text meta [
				Documentation.FieldCaption = "Key column name",
				Documentation.FieldDescription = "Name of the record key column",
				Documentation.SampleValues = {"Key1"}
			]),
			ItemColumn as (type text meta [
				Documentation.FieldCaption = "Item column name",
				Documentation.FieldDescription = "Name of the category column with concatenated values to split",
				Documentation.SampleValues = {"Category1"}
			]),
			Delimiter as (type text meta [
				Documentation.FieldCaption = "List delimiter",
				Documentation.FieldDescription = "Delimiter to split the list by",
				Documentation.SampleValues = {","}
			])
			) as any meta [
				Documentation.Name = "Get Category List with Key",
				Documentation.LongDescription = "Converts a concatenated category column to a distinct list with link to the original record key.",
				Documentation.Examples = {
					[Description = "Basic usage", Code = "GetCategoryListWithKey(Table1, ""Key1"", ""Category1"", "","")", Result = "A table with distinct key and category pair."]
				}
			],
		TypedFunction = Value.ReplaceType(Function, FunctionType)
	in
		TypedFunction
in
	GetCategoryListWithKey