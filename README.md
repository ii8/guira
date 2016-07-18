
# Guira

Guira is a schedule querying language.  
It allows querying sets of and checking individual dates against a schedule
defined in the guira language.

[Try in the browser](https://sir-murray.github.com/guira)

## Installing
You need ocaml and findlib(often comes with ocaml)
```
make
make install
```

To generate javascript run `make js`
(requires [js_of_ocaml](http://ocsigen.org/js_of_ocaml))

To run tests use `make test`
(requires [chibi-scheme](https://github.com/ashinn/chibi-scheme))

## Basic usage

**To get the 3rd day from today**
```
λ echo '(day (nth 3))' | guira
2016-07-14
```

**To check if the 14th of July in 2016 is 2 days from now**
```
λ echo '(day (nth 3))' | guira 2016-07-14
λ echo $?
0
```
Note that today is day 1, this can be changed with the '-s' flag, see below.

**Using subselectors**  
All Mondays next month
```
λ echo '(month (nth 2) (day mon))' | guira
2016-08-01
2016-08-08
2016-08-15
2016-08-22
2016-08-29
```

**Combining conditions**
```
λ echo '(year (or 2017 2030) (day (nth 3)))' | guira
2017-01-03
2030-01-03
```

## Command line options

If _DATE_ is given, the program is in check-mode.
Otherwise it is in query-mode.

`guira [OPTION...] [DATE]`

 * `-s`,  `--start-date` `<date>`  
   This is the date from which dates will be queried if in query-mode  
   and the date from which `nth` conditions are calculated if they are not
   in a subselector.  
   Defaults to today.

 * `-e`, `--end-date` `<date>`  
   Last date to query, in check-mode this option is ignored.

 * `-f`, `--format` `<format>`  
   Format of output dates, accepts a subset of strftime format strings;  
   currently implements a, b, d, F, m and Y.

 * `-i`, `--interval` `(day|week|month|year)`  
   Interval at which dates should be checked in query-mode.  
   All selectors in the query with greater precision than this interval
   will be ignored.

 * `-h`, `--help`  
   Prints a very helpful message.

## Return codes
the program will exit with one of the following codes:

 * `0` - Signifies that the date is valid for the given query in check-mode.
         In query-mode it just means 'success'.
 * `1` - Given date does not match query string.
 * `2` - An error occurred, will be accompanied by a message on stderr.

## Selectors
Available selectors are `year`, `month`, `week` and `day`.  
A selector can be applied to a condition and a list of subselectors:
```
(month                ; selector
  (or jul oct)        ; condition
  (day (nth 4))       ; subselector
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
interval ::= 'year' | 'month' | 'week' | 'day'
```

## Conditions
Conditions can be combined with the `or` and `and` functions, these
can be arbitrarily nested.  
`not` will simply reverse a conditions effect.  
Some conditions can only be used with their specific selectors,
for example the `jan` condition can only be used with the `month` selector.  
The nth condition can be applied to any selector.

**Grammar**
```
condition ::= '(' 'or' condition {condition} ')'
            | '(' 'and' condition {condition} ')'
            | '(' 'not' condition ')'
            | selector-condition
            | nth

nth ::= '(' 'nth' boolean-expression [condition] ')'
```

## Nth calculations
This uses the difference in terms of the current selectors interval
between the start of the parent selector
(for example Monday if the parent selector is `week`)
and the current time.  
So `(week (day (nth 2)))` will pick the 2nd day since Monday, which is
Tuesday.  
If there is no parent selector, the start date is used.

The nth value can be compared using arbitrary mathematical equations.  
The special variable "n" is substituted for the nth value.  
In the case where only one side of the equation is given, that expression
will be implicitly compared to "n" for equality.

`eq` checks for equality.  
`gt` checks if its first argument is greater than the second.  
`sum` adds all of its arguments.  
`mod` finds the remainder of dividing its first argument by the second.  

**Grammar**
```
boolean-expression ::= expression
                     | '(' 'eq' expression expression ')'
                     | '(' 'gt' expression expression ')'

expression ::= 'n'
             | integer
             | '(' 'sum' expression {expression} ')'
             | '(' 'mod' expression expression ')'
```

## year
The year selector can take a number as a condition which specifies
an exact year or `leap` which will select all leap years.

**Grammar**
```
selector-condition ::= integer
                     | 'leap'
```

## month
Takes a three letter month name as a condition.

**Grammar**
```
selector-condition ::= 'jan' | 'feb' | 'mar' | 'apr' | 'may' | 'jun'
                     | 'jul' | 'aug' | 'sep' | 'oct' | 'nov' | 'dec'
```

## week
Week has no special conditions, its only use is nth selection.

## day
Days can be selected by a three letter weekday.

**Grammar**
```
selector-condition ::= 'mon' | 'tue' | 'wed' | 'thu' | 'fri' | 'sat' | 'sun'
```

## The Javascript library
`make js` will produce a file named guira.js, this defines a function `guira`.

To use it call it with a guira query as the first parameter,
the date to check as second parameter, the reference date
as the third parameter, and lastly the interval.
```js
js> guira("(day thu)", "2015-01-01", "2014-01-01", "day");
true
```
