/*/ TZ CONVERTER \****************************************\
| Time zone converter with DST support                     |
|-[ author ]----------------------------------------------|
| 2022, Damian (comments), blog.crossjoin.co.uk           |
| 2023, Filip Kraus,       contact@filipkraus.net         |
|-[ changelog ]-------------------------------------------|
|  2023-05-31   1.0.0   Initial
|  2023-06-29   1.1.0   Add CET/CEST
|                       Add static DST TZ conversions 
\*/

let
    ConvertDateTimeZone = (
        InputDateTime as datetime,
        InputTimeZone as text,
        TargetTimeZone as text,
        AutoDST as logical
    ) as record => let
        
    // BST - British Summer Time
    BSTStart = Date.StartOfWeek(#datetime(Date.Year(InputDateTime),03,31,1,0,0),Day.Sunday) + #duration(0,1,0,0),
    BSTClose = Date.StartOfWeek(#datetime(Date.Year(InputDateTime),10,31,1,0,0),Day.Sunday) + #duration(0,1,0,0),
    IsBST = AutoDST and InputDateTime >= BSTStart and InputDateTime < BSTClose,

    // CEST - Central European Summer Time
    CESTStart = Date.StartOfWeek(#datetime(Date.Year(InputDateTime),03,31,1,0,0),Day.Sunday) + #duration(0,1,0,0),
    CESTClose = Date.StartOfWeek(#datetime(Date.Year(InputDateTime),10,31,1,0,0),Day.Sunday) + #duration(0,1,0,0),
    IsCEST = AutoDST and InputDateTime >= CESTStart and InputDateTime < CESTClose,

    UTCtoGMT = InputDateTime + (if IsBST then #duration(0,1,0,0) else #duration(0,0,0,0)),
    GMTtoUTC = InputDateTime - (if IsBST then #duration(0,1,0,0) else #duration(0,0,0,0)),
    UTCtoBST = InputDateTime + #duration(0,1,0,0),
    BSTtoUTC = InputDateTime - #duration(0,1,0,0),

    UTCtoCET = InputDateTime + (if IsBST then #duration(0,2,0,0) else #duration(0,1,0,0)),
    CETtoUTC = InputDateTime - (if IsBST then #duration(0,2,0,0) else #duration(0,1,0,0)),
    UTCtoCEST = InputDateTime + #duration(0,2,0,0),
    CESTtoUTC = InputDateTime - #duration(0,2,0,0),

    Output =
        if InputTimeZone = TargetTimeZone then Error.Record("Same input and target time zone.")
        // else if (List.Contains({ "BST", "CEST" }, InputTimeZone) or List.Contains({ "BST", "CEST" }, TargetTimeZone)) and AutoDST = false
        //     then Error.Record("DST time zones require 'Auto DST' to be enabled.")
        
        // UTC - GMT/BST
        else if InputTimeZone = "UTC" and TargetTimeZone = "GMT" then [DateTime=UTCtoGMT, DST=IsBST]
        else if InputTimeZone = "GMT" and TargetTimeZone = "UTC" then [DateTime=GMTtoUTC, DST=IsBST]
        else if InputTimeZone = "UTC" and TargetTimeZone = "BST" then [DateTime=UTCtoBST, DST=true]
        else if InputTimeZone = "BST" and TargetTimeZone = "UTC" then [DateTime=BSTtoUTC, DST=true]
        
        // UTC - CET/CEST
        else if InputTimeZone = "UTC" and TargetTimeZone = "CET" then [DateTime=UTCtoCET, DST=IsCEST]
        else if InputTimeZone = "CET" and TargetTimeZone = "UTC" then [DateTime=CETtoUTC, DST=IsBST]
        else if InputTimeZone = "UTC" and TargetTimeZone = "CEST" then [DateTime=UTCtoCEST, DST=true]
        else if InputTimeZone = "CEST" and TargetTimeZone = "UTC" then [DateTime=CESTtoUTC, DST=true]
        
        else Error.Record("Unsupported time zone.")

in
    Output,
  
    FunctionType = type function (
        InputDateTime as (type datetime meta [
            Documentation.FieldCaption = "Date time value",
            Documentation.FieldDescription = "Value to be converted.",
            Documentation.SampleValues = { "01/01/2023 13:00:00" }
        ]),
        InputTimeZone as (type text meta [
            Documentation.FieldCaption = "Input time zone",
            Documentation.FieldDescription = "Time zone of the input.",
            Documentation.AllowedValues = { "UTC", "GMT", "BST", "CET", "CEST" }
        ]),
        TargetTimeZone as (type text meta [
            Documentation.FieldCaption = "Target time zone",
            Documentation.FieldDescription = "Desired output time zone.",
            Documentation.AllowedValues = { "UTC", "GMT", "BST", "CET", "CEST"}
        ]),
        AutoDST as (type logical meta [
            Documentation.FieldCaption = "Auto DST",
            Documentation.FieldDescription = "Adjust for daylight saving time.",
            Documentation.AllowedValues = { true, false }
        ])
    ) as record meta [
        Documentation.Name = "Convert date time zone",
        Documentation.LongDescription = "Convert between time zones with the option to use DST.",
        Documentation.Examples = {[
            Description = "Convert UTC to GMT",
            Code = "ConvertDateTimeZone(#datetime(2023, 1, 1, 12, 0, 0), ""UTC"", ""GMT"", true)",
            Result = "#table({""DateTime"", ""DST""}, {{""01/01/2023 12:00:00"", ""false""}})"
        ],[
            Description = "Convert UTC to GMT/BST",
            Code = "ConvertDateTimeZone(#datetime(2023, 5, 1, 12, 0, 0), ""UTC"", ""GMT"", true)",
            Result = "#table({""DateTime"", ""DST""}, {{""01/05/2023 13:00:00"", ""true""}})"
        ]}
    ]
in
    Value.ReplaceType(ConvertDateTimeZone, FunctionType)