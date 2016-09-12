<img align="right" src="https://ii8.github.io/guira/guira.svg">
# Guira

Guira is a schedule querying language.  
It allows querying sets of and checking individual dates against a schedule
defined in the guira language.

[Try in the browser](https://ii8.github.com/guira)

IRC: [#guira on freenode](http://webchat.freenode.net/?channels=guira&uio=OT10cnVlJjExPTI5Nwd8)

## Installing
You need ocaml(version 4.02.0 or later) and findlib
```
make
make install
```

To generate javascript run `make js`
(requires [js_of_ocaml](http://ocsigen.org/js_of_ocaml))

For the mysql plugin, run `make mysql` (requires mysql)

To run tests use `make test`
(requires [chibi-scheme](https://github.com/ashinn/chibi-scheme))

`make install` will install whatever has been built. So if you only want
the mysql plugin, just don't run make without a target.

## Basic usage

**To get the 3rd day from today**
```
λ echo '(day 3)' | guira
2016-07-14
```

**To check if the 14th of July in 2016 is 2 days from now**
```
λ echo '(day 3)' | guira 2016-07-14
λ echo $?
0
```
Note that today is day 1, this can be changed with the '-s' flag, see below.

**Using subselectors**  
All Mondays next month
```
λ echo '(month 2 (day mon))' | guira
2016-08-01
2016-08-08
2016-08-15
2016-08-22
2016-08-29
```

**Combining conditions**
```
λ echo '(year (or ad2017 ad2030) (day 3)))' | guira
2017-01-03
2030-01-03
```

## Command line options

If _DATE_ is given, the program is in check-mode.
Otherwise it is in query-mode.

`guira [OPTION...] [DATE]`

 * `-s`,  `--start-date` `<date>`  
   This is the date from which dates will be queried if in query-mode  
   and the date from which the `n` variable is calculated if used in the
   outermost selector.  
   Defaults to today.

 * `-e`, `--end-date` `<date>`  
   Last date to query, in check-mode this option is ignored.

 * `-i`, `--interval` `(minute|hour|day|week|month|year)`  
   Interval at which dates should be checked in query-mode.  
   All selectors in the query with greater precision than this interval
   will be ignored.  
   Defaults to the precision of the query.

 * `-f`, `--format` `<format>`  
   Format of output dates, accepts a subset of strftime format strings;  
   currently implements a, b, d, F, H, m, M, S and Y.

 * `-h`, `--help`  
   Prints a very helpful message.

## Return codes
the program will exit with one of the following codes:

 * `0` - Signifies that the date is valid for the given query in check-mode.
         In query-mode it just means 'success'.
 * `1` - Given date does not match query string.
 * `2` - An error occurred, will be accompanied by a message on stderr.

## Selectors
Available selectors are `year`, `month`, `week`, `day`, `hour` and `minute`.  
A selector can be applied to a condition and a list of subselectors:
```
(month                ; selector
  (or jul oct)        ; condition
  (day 4)             ; subselector
  (day (or mon fri))) ; another subselector
```

If there are multiple subselectors, a date needs to match one or more of
them to be valid.

A subselectors interval must always be smaller than the parent interval.

Everything after a ';' is ignored until a newline.

The following grammar specifications are extended BNF with
an 'integer' terminal.

**Grammar**
```
selector ::= '(' interval [condition] {selector} ')'
interval ::= 'year' | 'month' | 'week' | 'day' | 'hour' | 'minute'
```

## Conditions
Conditions can be combined with the `or` and `and` functions, these
can be arbitrarily nested.  
`not` will simply reverse a conditions effect.  

**Grammar**
```
condition ::= expression
            | '(' 'or' condition {condition} ')'
            | '(' 'and' condition {condition} ')'
            | '(' 'not' condition ')'
            | '(' 'eq' expression expression ')'
            | '(' 'gt' expression expression ')'
            | weekday
            | month
            | 'ad' integer
            | 'leap'

weekday ::= 'mon' | 'tue' | 'wed' | 'thu' | 'fri' | 'sat' | 'sun'

month ::= 'jan' | 'feb' | 'mar' | 'apr' | 'may' | 'jun'
        | 'jul' | 'aug' | 'sep' | 'oct' | 'nov' | 'dec'
```

## Expressions
When the condition is just an expression, it is implicitly compared to `n`
for equality.
`n` is the difference in terms of the current selectors interval
between the start of the parent selector
(for example Monday if the parent selector is `week`)
and the current time.  
So `(week (day 2))` will pick the 2nd day since Monday, which is
Tuesday.  
If there is no parent selector, the start date is used.

**Grammar**
```
expression ::= 'n'
             | 'week-of-month'
             | 'day-of-month'
             | integer
             | '(' '+' expression {expression} ')'
             | '(' 'mod' expression expression ')'
```

## The Javascript library
`make js` will produce a file named guira.js, this defines a function `guira`.

To use it call it with the reference date as the first parameter, the interval
as second parameter, the query third, and lastly the date to check.
```js
js> guira("2014-01-01", "day", "(day thu)", "2015-01-01");
true
```

## The mysql plugin
`make mysql` will create a shared object called mysql-guira.so, this can be
dynamically linked by mysql to expose a GUIRA() function.  
`make install` will copy the file to [your mysql plugin directory]/guira.so  
You need to run a CREATE FUNCTION query:
```SQL
CREATE FUNCTION guira RETURNS INTEGER SONAME "guira.so";
```
And then the function can be used:
```SQL
mysql> SELECT GUIRA("2010-01-01", "day", "(month (day (eq (mod n 3) 0)))", "2015-04-15") AS berd;
+------+
| berd |
+------+
|    1 |
+------+
1 row in set (0.00 sec)
```
it returns 1 if the query matches, 0 otherwise.
