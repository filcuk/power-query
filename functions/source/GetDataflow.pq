/*********************************************************\
| GET DATAFLOW
|
|=--purpose----------------------------------------------=|
| PQ equivalent switch function to DAX
| 
|=--notes------------------------------------------------=|
| Does not support environments
|  
|=--credits----------------------------------------------=|
| Original	2024, Filip Kraus, filipkraus.net
|
|=--changelog--------------------------------------------=|
| 	2024-09-17  1.0.0   Initial
|   2024-09-27  1.1.0   Add table parameter
|
\*********************************************************/

let
    GetDataflow =
        let
            Function = (
                WorkspaceGUID as text
                ,DataflowGUID as text
                ,Entity as nullable text
                ,Version as nullable text
            ) as any =>
                let
                    Platform = PowerPlatform.Dataflows(null),
                    Workspaces = Platform{[Id="Workspaces"]}[Data],
                    Workspace = Workspaces{[workspaceId=WorkspaceGUID]}[Data],
                    Dataflow = Workspace{[dataflowId=DataflowGUID]}[Data],
                    OutputDataflow = Dataflow,

                    OutputEntity = let
                        Version = if Version = null then "" else Version,
                        Table = Dataflow{[entity=Entity,version=Version]}[Data]
                    in Table,

                    Output = if Entity = null then OutputDataflow else OutputEntity
                in
                    Output,

            FunctionType = type function (
                WorkspaceGUID as (type text meta [
					Documentation.FieldCaption = "Workspace GUID"
					,Formatting.IsCode = true
				])
                ,DataflowGUID as (type text meta [
					Documentation.FieldCaption = "Dataflow GUID"
					,Formatting.IsCode = true
				])
                ,Entity as (type text meta [
					Documentation.FieldCaption = "Entity Name"
					,Formatting.IsCode = true
				])
                ,Version as (type text meta [
					Documentation.FieldCaption = "Entity Version"
                    ,Documentation.Description = "Default is """""
					,Formatting.IsCode = true
				])
                ) as any meta [
                    Documentation.Name = "GetDataflow"
                    ,Documentation.LongDescription = "Parametric access to Dataflows and their entities."
                    // ,Documentation.Examples = {
                    //     [Description = "", Code = "", Result = ""]
                    // }
                ],
            TypedFunction = Value.ReplaceType(Function, FunctionType)
        in
            TypedFunction
in
    GetDataflow