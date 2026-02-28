/*/ Get Category List \***********************************\
|  Converts a concatenated category column                |
|  to a distinct list.                                    |
|-[ author ]----------------------------------------------|
|  2026, Filip Kraus, contact@filipkraus.net              |
|-[ changelog ]-------------------------------------------|
|  2026-02-28  1.1.0  Add option to include blanks for bi-direcational relationships
|  2026-02-28  1.0.0  Initial
\*/

let GetCategoryList =
	let Function = (
		Column as list,
		Name as text,
		Delimiter as text,
		optional IncludeBlanks as logical
	) as table =>

		let
			Source = Column,
			IncludeBlanks = if IncludeBlanks <> null then IncludeBlanks else false,

			Distinct1 = List.Distinct(Source),
			Table = Table.FromList(Distinct1, Splitter.SplitByNothing(), null, null, ExtraValues.Error),
			Filter = Table.SelectRows(Table, each ([Column1] <> null) or IncludeBlanks),
			AddCategory = Table.AddColumn(Filter, Name, each try Text.Split([Column1], Delimiter) otherwise {null}),
			RemoveList = Table.RemoveColumns(AddCategory,{"Column1"}),
			ExpandCategory = Table.ExpandListColumn(RemoveList, Name),
			Type = Table.TransformColumnTypes(ExpandCategory,{{Name, type text}}),
			Distinct2 = Table.Distinct(Type),
			Sort = Table.Sort(Distinct2,{{Name, Order.Ascending}})
		in
			Sort,

		FunctionType = type function (
			Column as (type list meta [
				Documentation.FieldCaption = "Category column",
				Documentation.FieldDescription = "Column with concatenated values to split"
			]),
			Name as (type text meta [
				Documentation.FieldCaption = "Column name",
				Documentation.FieldDescription = "Name of the resulting column",
				Documentation.SampleValues = {"Category2"}
			]),
			Delimiter as (type text meta [
				Documentation.FieldCaption = "List delimiter",
				Documentation.FieldDescription = "Delimiter to split the list by",
				Documentation.SampleValues = {","}
			]),
			optional IncludeBlanks as (type logical meta [
				Documentation.FieldCaption = "Include blanks",
				Documentation.FieldDescription = "Should the output include empty categories",
				Documentation.AllowedValues = {true, false},
				Documentation.SampleValues = {false}
			])
			) as any meta [
				Documentation.Name = "Get Category List",
				Documentation.LongDescription = "Converts a concatenated category column to a distinct list.",
				Documentation.Examples = {
					[Description = "Basic usage", Code = "GetCategoryList(Table1[Category1], ""Category2"", "","")", Result = "A table with distinct categories."]
				}
			],
		TypedFunction = Value.ReplaceType(Function, FunctionType)
	in
		TypedFunction
in
	GetCategoryList