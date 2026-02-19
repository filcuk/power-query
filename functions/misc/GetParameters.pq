/*/ GetParameters \***************************************\
|  Dynamically retrieves parameters with metadata.        |
|  May depend on custom metadata.                         |
|-[ author ]----------------------------------------------|
|  2026, Filip Kraus, contact@filipkraus.net              |
|-[ changelog ]-------------------------------------------|
|  2026-02-19  1.0.0  Initial
\*/

let GetParameters =
	let Function = (
		DisplayLevel as text
	) as table =>

        let
            Source = #sections[Section1],
            Table = Record.ToTable(Source),
            AddMeta = Table.AddColumn(Table, "Meta", each try Value.Metadata([Value]) otherwise null),
            FilterParameters = Table.SelectRows(AddMeta, each try [Meta][IsParameterQuery] otherwise false),
            AddDescription = Table.AddColumn(FilterParameters, "Description", each try [Meta][Documentation.Description] otherwise null, type text),
            AddRequired = Table.AddColumn(AddDescription, "Required", each if [Meta][IsParameterQueryRequired] then "Yes" else "No", type text),
            AddOptions = Table.AddColumn(AddRequired, "Options", each try Text.Combine([Meta][List], ", ") otherwise null, type text),
            ExpandMeta = Table.ExpandRecordColumn(AddOptions, "Meta", {"Type", "DisplayLevel", "DefaultValue"}, {"Type", "Display Level", "Default"}),
            Type = Table.TransformColumnTypes(ExpandMeta,{{"Type", type text}, {"Display Level", type text}, {"Value", type text}, {"Default", type text}}),
            // Display level is a custom metadata that must be added manually to each parameter
            DisplayList =
                if DisplayLevel = "Report" then {"Report"}                                          // For use in DAX
                else if DisplayLevel = "User" then {"Report", "User"}                               // User visibility (e.g. date range)
                else if DisplayLevel = "Support" then {"Report", "User", "Support"}                 // Support visibility (e.g. object GUID)
                else if DisplayLevel = "Developer" then {"Report", "User", "Support", "Developer"}  // Developer visibility (e.g. changelog config)
                else {},
            FilterDisplayLevel = Table.SelectRows(Type, each List.Contains(DisplayList, [Display Level]))
        in
            FilterDisplayLevel,

		FunctionType = type function (
			DisplayLevel as (type text meta [
				Documentation.FieldCaption = "Display Level",
				Documentation.FieldDescription = "Acting as a filter, it defines which parameters are output",
                Documentation.AllowedValues = {"Report", "User", "Support", "Developer"},
				Documentation.SampleValues = {"Support"}
			])
			) as any meta [
				Documentation.Name = "Get Parameters",
				Documentation.LongDescription = "Dynamically retrieves parameters and their metadata from current model"
			],

		TypedFunction = Value.ReplaceType(Function, FunctionType)
	in
		TypedFunction
in
	GetParameters