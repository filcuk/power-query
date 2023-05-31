/*********************************************************\
| TIME ZONE CONVERTER WITH DST SUPPORT
|
|=--credits----------------------------------------------=|
| Original 2022 by Damian (comments), blog.crossjoin.co.uk
| Modified 2023 by Filip Kraus, filipkraus.net
|
|=--changelog--------------------------------------------=|
| 	2023-05-31
| 		Initial
| 
\*********************************************************/

let
    ConvertDateTimeZone = (
        InputDateTime as datetime,
        InputTimeZone as text,
        TargetTimeZone as text,
        UseDST as logical
    ) as record => let
        
    // BST - British Summer Time
    BSTStart = Date.StartOfWeek(#datetime(Date.Year(InputDateTime),03,31,0,0,0),Day.Sunday) + #duration(0,1,0,0),
    BSTClose = Date.StartOfWeek(#datetime(Date.Year(InputDateTime),10,31,0,0,0),Day.Sunday) + #duration(0,1,0,0),
    IsBST = UseDST and InputDateTime >= BSTStart and InputDateTime < BSTClose,

    UTCtoGMT = InputDateTime + (if IsBST then #duration(0,1,0,0) else #duration(0,0,0,0)),
    GMTtoUTC = InputDateTime - (if IsBST then #duration(0,1,0,0) else #duration(0,0,0,0)),

    Output =
        if InputTimeZone = TargetTimeZone then Error.Record("Same input and target time zone.")
        else if InputTimeZone = "UTC" and TargetTimeZone = "GMT" then [DateTime=UTCtoGMT, DST=IsBST]
        else if InputTimeZone = "GMT" and TargetTimeZone = "UTC" then [DateTime=GMTtoUTC, DST=false]
        // else if InputTimeZone = "UTC" and TargetTimeZone = "CEST" then CEST
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
            Documentation.AllowedValues = { "UTC", "GMT" }
        ]),
        TargetTimeZone as (type text meta [
            Documentation.FieldCaption = "Target time zone",
            Documentation.FieldDescription = "Desired output time zone.",
            Documentation.AllowedValues = { "UTC", "GMT" }
        ]),
        UseDST as (type logical meta [
            Documentation.FieldCaption = "Use DST",
            Documentation.FieldDescription = "Account for daylight saving time.",
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