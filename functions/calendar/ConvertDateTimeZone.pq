/*********************************************************\
| TIME ZONE CONVERTER
|
|=--purpose----------------------------------------------=|
| Convert time with support for summer time.
| 
|=--notes------------------------------------------------=|
| Supported time zones:
| 	UTC, GMT
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
        Output
in
    ConvertDateTimeZone