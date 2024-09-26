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
|
\*********************************************************/

let
    GetDataflow =
        let
            Function = (WorkspaceGUID as text, DataflowGUID as text) as any =>
                let
                    Platform = PowerPlatform.Dataflows(null),
                    Workspaces = Platform{[Id="Workspaces"]}[Data],
                    Workspace = Workspaces{[workspaceId=WorkspaceGUID]}[Data],
                    Dataflow = Workspace{[dataflowId=DataflowGUID]}[Data]
                in
                    Dataflow,

            FunctionType = type function (WorkspaceGUID as text, DataflowGUID as text) as any
                meta [
                    Documentation.Name = "GetDataflow"
                    ,Documentation.LongDescription = "Parametric access to Dataflows."
                    // ,Documentation.Examples = {
                    //     [Description = "Simple Switch expression", Code = "Switch({2, 1, ""A"", 2, ""B""})", Result = """B"""],
                    //     [Description = "An equivalent of SWITCH(TRUE... in DAX", Code = "Switch({true, 1 > 2, ""A"", 1 < 2, ""B"", ""No result""})", Result = """B"""]
                    // }
                ],
            TypedFunction = Value.ReplaceType(Function, FunctionType)
        in
            TypedFunction
in
    GetDataflow