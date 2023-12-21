# power-query

Use description headings with changelogs:
```powerquery
/*********************************************************\
| CALENDAR GENERATOR
|
|=--purpose----------------------------------------------=|
| Generate a date table based on start and end dates.
| 
|=--notes------------------------------------------------=|
| LangTag examples: en-US, en-GB
| Column naming:
|  	%	   date
|  	#, ##	number, padded number (text)
|  	Is ...	boolean
| 	@M/L	text shortenned/full (Jan/January)
| 
|=--credits----------------------------------------------=|
| Original 1999 by X, x.com
| Modified 2000 by Filip Kraus, filipkraus.net
|
|=--changelog--------------------------------------------=|
| 	2023-04-06, 1.3.0
| 		Add financial, ISO, calendar 4-4-5
| 		Apply standardised column naming
|	2023-05-11, 1.3.1
|		Add 'Week @M'
| 	2023-05-18, 1.4.0
| 		Fix last date (EndDate) not being generated
| 		Add forward-looking bool columns
| 	2023-05-22, 1.4.1
| 		Add ISO week formats ##, @M
|	2023-05-25, 1.5.0
| 		Add Period 445 formats ##, @M
| 	2023-08-03, 1.5.1
|		Add Quarter @M, @L
| 	2023-08-23, 1.5.2
| 		Add [Day & Weekday @M]
| 		Fix [Day of Week ##] showing day of month
| 	2023-10-13, 1.5.3
|		Change [ISO Week Year #] to [ISO Year #]
| 		Add [ISO Week Year #] as YYYYWW, [Month Year #], [Month Year @M]
|	2023-10-26, 1.5.4
| 		Add [Date is Before Today], [Date is After Today]
| 	2023-10-30, 1.5.5
| 		Add [Week Year #, #M, @M, @L]
| 		Fix [Day of Week #] not using FirstWeekDay parameter
| 		Remove [Day of Week ##]
| 
\*********************************************************/
```