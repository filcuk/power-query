/*********************************************************\
| REFRESH GENERATOR
|
|=--purpose----------------------------------------------=|
| Generate a table with refresh and client information.
| 
|=--credits----------------------------------------------=|
| Original 2023 by Filip Kraus, filipkraus.net
|
\*********************************************************/

let GetRefreshTable = () =>
    let
        Source = DateTimeZone.UtcNow(),
        Table = #table(type table [RefreshUTC = datetimezone], {{Source}}),
        RefreshLocal = Table.AddColumn(Table, "RefreshLocal", each DateTimeZone.ToLocal([RefreshUTC]), type datetimezone),
        TimeZoneLocalOffset = Table.AddColumn(RefreshLocal, "TimeZoneLocalOffset", each DateTimeZone.ZoneHours([RefreshLocal]), type number),
        TimeZoneLocal = Table.AddColumn(TimeZoneLocalOffset, "TimeZoneLocal", each TimeZone.Current, type text),
        CultureLocal = Table.AddColumn(TimeZoneLocal, "CultureLocal", each Culture.Current, type text)
    in
        CultureLocal
in
    GetRefreshTable