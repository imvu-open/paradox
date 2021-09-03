# Paradox

## Build:

### If using docker and stack-static.yaml
0. (cabal update)
1. (install stack and docker)
2. stack --stack-yaml=stack-static.yaml docker pull
3. stack --stack-yaml=stack-static.yaml build

### If using normal
0. (cabal update)
1. (install stack)
2. stack build

## Test Run:
### These steps will get you to a running state given that you configure them to use your istatd isntance (local or remote)
0. ## configs from dev may need to be edited to point at your istatd instance/have your users
1. (sudo) mkdir -p /etc/paradox/
2. (sudo) mkdir -p /var/log/paradox
3. (sudo) cp dev/config/\* /etc/paradox/
4. ## ensure /var/log/paradox is writable to your dev user
5. $(stack {OPTIONAL YAML ARGS} path --local-install-root)/bin/paradox &

## Configuration
### Main Config
/etc/paradox/config  
```js
{
  target : // string - A string specifying the url target where paradox can make istatd requests
, host : // string - host to include in host header of target
, user : // string - if istatd requires http auth this is the user
, password : // string - if istatd requires http auth this is the password
, use_auth : // bool - detemines if istatd target requires auth or not
, query_timer : // int - microsecond interval for querying counter list
, port : // int - port to listen on
, istatd_agent_host : // string - host for istatd agent to report metrics to
, istatd_agent_port : // int - port istatd agent listens on
, istatd_agent_categories : // path - path to a file that gives istad categories
, istatd_agent_prefix : // string - prefix for reported istatd counters
, serve_base : // path - path to root of files folder for serving static files
, sandboxed_commands = {
    "cmdname" : {
        "command" : // string - command to run,
        "delim" : // string - results delimiter,
        "args" : // list[string] - base args to command
        }...
  },
, logger_config = {
    "name" : {
        "type" : // logger_type - type of logger to configure,
        "config" : {
            "path" : // string - filepath,
            "bufferSize" : // int - buffer before flushing,
            ...etc
        },
        "level": // logger_level - what levels to log to this logger
    }...
  }
, events_config = {
    "name" : {
        "proto" : // "http"|"https" - what protocol to contact,
        "addr" : // string - address,
        "port" : // int - port,
        "endpoint" : // string - path fragment for endpoint,
        "filter": // event_filter - special string to filter results
    }...
  }
, smtp_server : // string - where is our smtp server
, fonts_path : // string - path to the folder containing our svg fonts folder
, source_addr : // string - what email name should be used when sending emails
, base_url : // string - the base url this will be reached at http(s)://asdasd/
, permalink_dir : // string - directory that will store saved images / svgs for permalinking
, permalink_wait_timeout : // string - how long a permalink request will await for a png/svg to be rendered
, repl_port : // int - the port to run the tcp repl on
, template_dir :  // string - where templates (parameterized dashboards should be found)
}

```

_This is the type of log we will be configuring_
```haskell
logger_type = "file"
             | "access"
             | "stdout"
             | "stderr"
```

_Configuration is specific to the type_

###### file|access
```json
config = {
    "path" : "filepath",
    "bufferSize" : #sizeOfFlushBuffer
}
```
###### stdout|stderr
```json
config = {
    "bufferSize" : #sizeOfFlushBuffer
}
```

_Level configures what level of logs go to that file (>=)_

```haskell
logger_level = "trace"
              | "debug"
              | "verbose"
              | "info"
              | "warning"
              | "error"
              | "critical"
```
Note: "access" type logs ignore level but currently must be included

###### example
```json
{
    "logging" : {
        "type" : "file",
        "config" : {
            "path" : "/var/log/paradox/paradox.log",
            "bufferSize" : 4096
        },
        "level": "verbose"
    },
    "stdout" : {
        "type" : "stdout",
        "config" : {
            "buffersSize" : 4096
        },
        "level": "info"
    },
    "access" : {
        "type" : "access",
        "config" : {
            "path" : "/var/log/paradox/access.log",
            "bufferSize" : 4096
        },
        "level": "trace"
    }
}
```


#### Sandboxed commands

###### example:
```json
{
   "space_echo" : {
        "command" : "echo",
        "delim" : " ",
        "args" : ["-n"]
   },
   "newline_cat" : {
        "command" : "cat",
        "delim" : "\n",
        "args" : []
   }
}
```

#### Events config
```haskell
event_filter = string ; ':'
```

###### example:
```json
{
   "pushes" : {
        "proto" : "http",
        "addr" : "localhost",
        "port" : 900,
        "endpoint" : "/words/,
        "filter" : "words:",
   },
   "commits" : {
        "proto" : "https",
        "addr" : "remote.addr",
        "port" : 9000,
        "endpoint" : "/commits/,
        "filter" : "commits:",
   }
}
```


## Query:
```
PREDEFINED

lower = [a-z];
upper = [A-Z];
digit = [0-9];
stringwithescapes = ? any string with visibile escaped characters and normal chars ?;

GRAMMAR

alphanum = lower | upper | digit;

whitespace = { " " };

firstcounterchar = lower | "_" | "!";

counterchar = alphanum | "_" | "-" | "!" | "<" | ">" | "+";

shellchar = alphanum | "!" | "@" | "#" | "$" | "%" | "^" | "*"
          | "(" | ")" | "-" | "_" | "=" | "+" | "|" | "[" | "]" | "{" | "}"
          | ":" | "," | "." | "<" | ">" | "/" | "?" | " ";

shell = "{'", { shellchar }+ , "'}";

array = "[" , ( { counterchar }+ , [ "," , { counterchar }+ ] ) , "]";

counterpart = ( { counterchar } | "*" | "?") | (array | shell);

counter = firstcounterchar, { counterchar }*, { ".", { counterpart }+ }+;

sign = "+" | "-";

double = [ sign ], { digit }+, ".", { digit }+;
integer = [ sign ], { digit }+;

bool = 'false' | 'true' | 'False' | 'True';

literalstring = '"', stringwithescapes '"';

literalarray = "[",
             ( ( double, { ",", double }* )
             | ( integer, { ",", integer }* )
             | ( bool, { ",", bool }* )
             | ( literalstring, { ",", literalstring }* )
             ),
             "]";

literalident = double | integer | bool | literalarray | literalstring;

param = whitespace*, lower, { alphanum }*, whitespace*, "=", whitespace*, literalident, whitespace*;

params = [ param, { ",", param } ];

paramsection = "{", whitespace*, params, whitespace*, "}";

counters = counter, { ",", counter }*, [ paramsection ];



infixApply = whitespace+, "|", whitespace+;

apply = whitespace+;

compose = whitespace+, ".", whitespace+;

expr = (expr, infixApply, expr) | ( expr apply expr ) | ("(", whitespace*, expr, ")");

topexpr = counters | ("constantLine", whitespace+, double) | (counters, infixApply, expr) | (expr, apply, counters)
```

### Example:
* redis.memory.host.ahost
  * retrns a graph for redis.memory for 'ahost'
* mysql.connections.host.\*
  * returns a graph for mysql.connections for each host
* sum mysql.queries.queue.host.\*
  * returns a graph for the summed total of mysql.queures.queue per host
* mysql.queries.queue.host.\* | sum
  * same as above.
  * returns a graph for the summed total of mysql.queures.queue per host
* mysql.queries.queue.host.ahost,mysql.queries.queue.host.ahost2 | sum
  * returns a graph for to summed total of mysql.queures.queue for each of ahost and ahost2


### Note:
Whitelisted shell commands allow you to expand a counter based on a third part shell command.

As an example, using the above example Sandboxed Commands File configuration

* redis.memory.host.{'space_echo host1 host2 host3'}
* redis.memory.host.{'newline_cat /file/with/hosts/in/it'}

### Offsets

A counter may be followed by a construct that looks like:
```
timestring = ([0-9]+, [smhDWMY])+
{ offset = \d+|timestring }
```
example:
  * counter.thing { offset = -1234 }
    * offset into past 1234 seconds
  * counter.thing { offset = "-1W" }
    * offset into past 1 week

### Language
Paradox's language has a few built in constructs
 * '(expression)'
   * nesting expressions
 * '|'
   * Infix function application (ala the unix pipe)
   * ctr.value | function | anotherfunction
   * ctr.value | function | (more.ctrs | anotherfunction)
 * ' '
   * Normal fix function application
   * anotherfunction (function ctr.value)
 * '.'
   * Haskell style function composition
   * ctr.value | (function "arg" . anotherfunction 1234)

### Functions:

##### sum|add                      :: SeriesList -> SeriesList
Sum together a list of time series. This is a reducing function
  * sum mysql.connections.host.\*
  * mysql.connections.host.\* | sum
  * mysql.connections.host.\* | add

##### alias                        :: String -> SeriesList -> SeriesList
Renames a series list. Use only on reduced series lists
  * sum my.number.of.users.\* | alias "Total Online Users"

##### smartAlias                   :: String -> Int -> SeriesList -> SeriesList
Renames a series list. Will attempt to select the counter part of a processed series list and split it on .
and then replace all instances of {} in the string parameter with the counter part at the int index (0 indexed)
  * sum my.number.of.users.\* | smartAlias "Number of users {}" 4
  * counter.\* | keepSeriesAboveDeviation 20 | smartAlias "newname.{}.above-20-deviations" 4

##### smartAliasA                  :: String -> [Int] -> SeriesList -> SeriesList
Renames a series list. Will attempt to select the counter parts of a processed series list and split it on .
and then replace all instances of {#n} in the string parameter with the counter part at the int index (0 indexed)
  * sum my.number.of.users.\* | smartAlias "Number of {1} users {3}" [1,3]
  * counter.\* | keepSeriesAboveDeviation 20 | smartAliasA "newname.{4}.above-20-deviations" [4]

##### exclude                      :: String -> SeriesList -> SeriesList
Remove counters from the series list whose name matches the regex string
  * tail.logs.\* | exclude "warning"

##### include                      :: String -> SeriesList -> SeriesList
Only include counters from the series list whose name matches the regex string
  * tail.logs.\* | include "warning"

##### scale                        :: Double -> SeriesList -> SeriesList
Scale each of the series in the serieslist by the double value
  * cpu.idle.ahost | scale -1.0

##### scaleByInterval              :: SeriesList -> SeriesList
Scale each value in the serieslist by its interval value. Useful in conjunction with integrate and derive
  * num.messages.sent.per.second | scaleByInterval

##### divideByInterval             :: SeriesList -> SeriesList
Similar to scale by inteval. Modify each value in the serieslist by its interval value. Useful in conjunction with integrate and derive
  * num.messages.sent.per.second | divideByInterval

##### integrate                    :: SeriesList -> SeriesList
Produce a running sum over time over each time series in the series list
  * num.messages.sent.per.second | integrate

##### derive                       :: SeriesList -> SeriesList
Take a running total and produce the change between data points for each time series in the series list
  * total.revenue | derive

##### sub|subtract                 :: SeriesList -> SeriesList -> SeriesList
Subtract the sum of the second series list from the (reduced) first series list
  * sub mysql.total.connections mysql.connections.ahost
  * mysql.connections.ahost | sub mysql.total.connections
  * mysql.connections.the\* | sub mysql.total.connections

##### divide                       :: SeriesList -> SeriesList -> SeriesList
Divide the first (reduced) series list by the sum of the second series list
  * divide total.messages.sent num.online.users
  * num.online.users | divide total.messages.sent

##### avg|average                  :: SeriesList -> SeriesList
Average a series list by summing them together and dividing by the number of series
  * response.time.host.\* | avg

##### movingAverage                :: Integer -> SeriesList -> SeriesList
Runs a window filter over timeseries data for *n* past points, graphing the average of those points at each time
  * cpu.idle.host.ahost | movingAverage 30

##### medianFilter                 :: Integer -> SeriesList -> SeriesList
Runs a window filter over timeseries data for *n* past points, graphing the median value of those points at each time
  * cpu.idle.host.ahost | medianFilter 30

##### offset                       :: Double -> SeriesList -> SeriesList
Adds the offset value to each point in the series lists timeseries
  * errors | offset -10.0

##### cons                         :: SeriesList -> SeriesList -> SeriesList
List cons operator, combines 2 series lists
  * errors.warning | cons warnings.\*

##### constantLine                 :: Double -> SeriesList
Draws a generated constant line with value *n*
  * constantLine 1.0
  * constantLine 100.0

##### log                 :: Integer -> Double -> Integer -> SeriesList
Mutates the data into logarithmic form. 
Runs the following computation on every data point. Arguments are {base}, {offset}, {bias}.  <log_{base}({data_point} + {offset}) + {bias}>
  * ctr.error | log 10 0 0
  * ctr.\* | log 10 0.1 1

##### summarizeAvg                 :: Integer -> SeriesList -> SeriesList
Reduces the data points in a time series by averaging them over a new interval, specified in seconds.
  * num.connected.users | summarizeAvg 600

##### summarizeSum                 :: Integer -> SeriesList -> SeriesList
Reduces the data points in a time series by summing  them over a new interval, specified in seconds.
  * num.errors | summarizeSum 600

##### graphAvg                     :: SeriesList -> SeriesList
Has the effect of setting min,max, and avg to value in avg, and zeroing the stddev
  * mysql.query.time.\* | graphAvg

##### graphMax                     :: SeriesList -> SeriesList
Has the effect of setting min,max, and avg to value in max, and zeroing the stddev
  * mysql.query.time.\* | graphMax

##### graphMin                     :: SeriesList -> SeriesList
Has the effect of setting min,max, and avg to value in min, and zeroing the stddev
  * mysql.query.time.\* | graphMin

##### graphField                     :: GetAvg|GetMin|GetMax -> SeriesList -> SeriesList
Has the effect of setting min,max, and avg to value from the appropriate get function, and zeroing the stddev
  * mysql.query.time.\* | graphField getMin

##### maxMaxAtEachSample           :: SeriesList -> SeriesList
Keeps the timeseries value from a series list that has the maximum max data point. Basically reduces a list of series into a single series of all the maximum max values at each point.
  * mysql.query.time.\* | maxMaxAtEachSample

##### maxAvgAtEachSample           :: SeriesList -> SeriesList
Keeps the timeseries value from a series list that has the maximum avg data point. Basically reduces a list of series into a single series of all the maximum avg values at each point.
  * mysql.query.time.\* | maxAvgAtEachSample

##### maxMinAtEachSample           :: SeriesList -> SeriesList
Keeps the timeseries value from a series list that has the maximum min data point. Basically reduces a list of series into a single series of all the maximum min values at each point.
  * mysql.query.time.\* | maxMinAtEachSample

##### maxValueAtEachSample           :: GetAvg|GetMin|GetMax -> SeriesList -> SeriesList
Keeps the timeseries value from a series list that has the maximum data point (for the field from the appropriate getter). Basically reduces a list of series into a single series of all the maximum min values at each point.
  * mysql.query.time.\* | maxValuetEachSample getMax

##### keepSeriesAboveDeviation     :: Integer -> SeriesList -> SeriesList
##### keepSeriesAboveDeviationInc  :: Integer -> SeriesList -> SeriesList
These functions will take a list of series and only return the ones who have at least one point that is outside the number of standard deviations specified in the first argument.

##### keepSeriesWithinDeviation    :: Integer -> SeriesList -> SeriesList
##### keepSeriesWithinDeviationInc :: Integer -> SeriesList -> SeriesList
These functions will take a list of series and only return the ones who have all points within the number of standard deviations specified in the first argument.

##### removeAbovePercentile        :: Integer -> SeriesList -> SeriesList
##### removeAbovePercentileInc     :: Integer -> SeriesList -> SeriesList
##### removeBelowPercentile        :: Integer -> SeriesList -> SeriesList
##### removeBelowPercentileInc     :: Integer -> SeriesList -> SeriesList
##### removeAboveValue             :: Double -> SeriesList -> SeriesList
##### removeAboveValueInc          :: Double -> SeriesList -> SeriesList
##### removeBelowValue             :: Double -> SeriesList -> SeriesList
##### removeBelowValueInc          :: Double -> SeriesList -> SeriesList
These functions will remove values that are above/below the value/percentile passed in the first arguement.

##### keepSeriesAbove              :: Double -> SeriesList -> SeriesList
##### keepSeriesAboveInc           :: Double -> SeriesList -> SeriesList
##### keepSeriesBelow              :: Double -> SeriesList -> SeriesList
##### keepSeriesBelowInc           :: Double -> SeriesList -> SeriesList
These functions will take a list of series and only return the ones who have at least one point above or below the value passed in the first argument.

##### keepSeriesAllAbove           :: Double -> SeriesList -> SeriesList
##### keepSeriesAllAboveInc        :: Double -> SeriesList -> SeriesList
##### keepSeriesAllBelow           :: Double -> SeriesList -> SeriesList
##### keepSeriesAllBelowInc        :: Double -> SeriesList -> SeriesList
These functions will take a list of series and only return the ones who have all points above or below the value passed in the first argument.

##### graphOptions                 :: String -> SeriesList -> SeriesList
Acts as a passtrough for data that should be associated with the results of a query,
but which has been chosen by the client. Used to add styling parameters to graphs. This
variant takes an escaped json string.
  * mobile.users | graphOptions "{\"strokeWidth\":4.0}"
  * mobile.users | graphOptions "{\"strokeWidth\":2.0,\"strokePattern\",[2,3]}"

##### graphOption[I|D|S|B]         :: String -> Int|Double|String|Bool -> SeriesList -> SeriesList
Acts as a passtrough for data that should be associated with the results of a query,
but which has been chosen by the client. Used to add styling parameters to graphs. This
variant adds typed key value pairs to the map to be returned.
  * mobile.users | graphOptionD "strokeWidth" 4.0
  * mobile.users | graphOptionD "strokeWidth" 2.0 | graphOptionB "showAnnotations" False

##### graphOption[I|D|S|B]A         :: String -> [Int|Double|String|Bool] -> SeriesList -> SeriesList
Acts as a passtrough for data that should be associated with the results of a query,
but which has been chosen by the client. Used to add styling parameters to graphs. This
variant adds typed key value pairs to the map to be returned, where the values are arrays
  * mobile.users | graphOptionIA "strokePattern" [4,0,3]
  * mobile.users | graphOptionD "strokeWidth" 2.0 | graphOptionB "showAnnotations" False | graphOptionIA "strokePattern" [1,2]

##### addAxis                      :: String -> SeriesList -> SeriesList
Acts as a passtrough for data that should be associated with the results of a query but affects a graph,
specifically the addition of a different y axis. Used in conjunction with graphOptionS "axis" {name} to attach
a query to that axis.
  * mobile.users | addAxis "y2" | graphOptionS "axis" "y2"

##### axisOptions                  :: String -> SeriesList -> SeriesList
Acts as a passtrough for data that should be associated with the results of a query,
but which has been chosen by the client. Used to add styling parameters to axes. This
variant takes an escaped json string.
  * mobile.users | addAxis "y2" | axisOptions "{\"y2\":{\"drawAxis\":true}}"

##### axisOption[I|D|S|B]          :: String -> String -> Int|Double|String|Bool -> SeriesList -> SeriesList
First argument is axis name to act on. Acts as a passtrough for data that should be associated with the results of a query,
but which has been chosen by the client. Used to add styling parameters to axes. This
variant adds typed key value pairs to the map to be returned.
  * mobile.users | addAxis "y2" | axisOptionB "y2" "drawAxis" true

##### axisOption[I|D|S|B]A         :: String -> String -> [Int|Double|String|Bool] -> SeriesList -> SeriesList
First argument is axis name to act on. Acts as a passtrough for data that should be associated with the results of a query,
but which has been chosen by the client. Used to add styling parameters to axes. This
variant adds typed key value pairs to the map to be returned, where the values are arrays
  * mobile.users | addAxis "y2" | axisOptionIA "y2" "someArrayOption" [4,0,3]


##### surfaceOptions                  :: String -> SeriesList -> SeriesList
Acts as a passtrough for data that should be associated with the results of a query,
but which has been chosen by the client. Used to add styling parameters to axes. This
variant takes an escaped json string.
  * mobile.users | addAxis "y2" | axisOptions "{\"y2\":{\"drawAxis\":true}}"

##### surfaceOption                   :: String -> SeriesList -> SeriesList
This variant adds a key key with no values to the map to be returned.
  * mobile.users | surfaceOption "thing\_we\_can\_check"

##### surfaceOption[I|D|S|B]          :: String -> Int|Double|String|Bool -> SeriesList -> SeriesList
Acts as a passtrough for data that should be associated with the results of a query,
but which has been chosen by the client. Used to add styling to a whole graph. This
variant adds typed key value pairs to the map to be returned.
  * mobile.users | surfaceOptionB "constrainThings" True

##### surfaceOption[I|D|S|B]A         :: String -> [Int|Double|String|Bool] -> SeriesList -> SeriesList
Acts as a passtrough for data that should be associated with the results of a query,
but which has been chosen by the client. Used to add styling to a whole graph. This
variant adds typed key value pairs to the map to be returned, where the values are arrays
  * mobile.users | surfaceOptionIA "someArrayOption" [4,0,3]

##### Map                             :: (SeriesList -> SeriesList) -> SeriesList -> SeriesList
Higher order function. Takes a fully reduced expression and runs it on each element of the series list,
returning a result for each series processed. Can be used to take a reducing function and run it for 
each result from a counter glob query.
  * map (subtract (mobile.users.\* | sum)) mobile.users.\*
    * NOTE: this will sum all mobile users, and prepare a subtract function that will then take EACH result from mobile.users.\* in turn and subtract it from the total.

##### When                             :: (SeriesList -> Bool) -> (SeriesList -> SeriesList) -> SeriesList -> SeriesList
##### WhenElse                         :: (SeriesList -> Bool) -> (SeriesList -> SeriesList) -> (SeriesList -> SeriesList) -> SeriesList -> SeriesList
##### WhenTransformed                  :: (SeriesList -> Bool) -> (SeriesList -> SeriesList) -> (SeriesList -> SeriesList) -> SeriesList -> SeriesList
##### WhenElseTransformed              :: (SeriesList -> Bool) -> (SeriesList -> SeriesList) -> (SeriesList -> SeriesList) -> (SeriesList -> SeriesList) -> SeriesList -> SeriesList
Decision making functions. Take a predicate as the first argument. Each one adds a simple variation.
  * when - second argument is action to perform on true result from predicate
  * whenElse - second argument is action to perform on true result from predicate, third is action to perform otherwise
  * whenTransformed - second argument is transformation to apply to counter data in order to perform predicate, third argument is action to perform on true result from predicate
  * whenElseTransformed - second argument is transformation to apply to counter data in order to perform predicate, third argument is action to perform on true result from predicate, fourth is action to perform otherwise
    * some.ctrs.\* | whenElseTransformed (hasAnyValue greaterThan getAvg 10.0) (scale 10.0) (dyColor "red") (dyColor "blue")
      * colors the lines red when ANY avg value in any of the counters is greater than 10 when the scale 10 function has been applied. Does not scale the resulting graph

##### HasAnyValue                      :: (Double -> Double -> Bool) -> GetAvg|GetMin|GetMax -> Double -> SeriesList -> Bool
##### HasAllValue                      :: (Double -> Double -> Bool) -> GetAvg|GetMin|GetMax -> Double -> SeriesList -> Bool
##### HasNoValue                       :: (Double -> Double -> Bool) -> GetAvg|GetMin|GetMax -> Double -> SeriesList -> Bool
Predicate function. Will check a condition on all the values in the series list. Will use appropriate getter function to access fields.
  * hasAnyValue - checks if any element in any series in the list meets the predicate
  * hasAllValue - checks if all element in all series in the list meets the predicate
  * hasNoValue - checks if no element in no series in the list meets the predicate

##### GreaterThan                      :: Double -> Double -> Bool
##### GreaterThan                      :: Double -> Double -> Bool
##### LessThan                         :: Double -> Double -> Bool
##### LessThanOrEqual                  :: Double -> Double -> Bool
Conditional predicate. Passed to higher order predicate functions.

##### GetAvg                           :: Bucket -> Double
##### GetMin                           :: Bucket -> Double
##### GetMax                           :: Bucket -> Double
##### GetCount                         :: Bucket -> Double
Getter functions. Let higher order functions access fields in the series list -> series -> bucket data structure

##### Not                              :: Bool -> Bool
Used to invert boolean predicates.

##### ApplyWithValue                   :: (Double -> ReturnData -> ReturnData) -> (ReturnData -> Double) -> ReturnData -> ReturnData
Applies the function in the first argument, using a value collected by the function in the second argumemnt.

##### GetLargest                       :: GetAvg|GetMin|GetMax -> ReturnData -> Double
##### GetSmallest                      :: GetAvg|GetMin|GetMax -> ReturnData -> Double
Gets the largest/smallest value, using the first argument getter function as a field selector.
##### GetLargestInRange                :: GetAvg|GetMin|GetMax -> Double -> ReturnData -> Double
##### GetSmallestInRange               :: GetAvg|GetMin|GetMax -> Double -> ReturnData -> Double
Gets the largest/smallest value, using the first argument getter function as a field selector, and the second argument as a right biased percentage range.

##### SmallerD                         :: Double -> Double -> Double
##### LargerD                          :: Double -> Double -> Double
##### MulD                             :: Double -> Double -> Double
##### AddD                             :: Double -> Double -> Double
##### DivD                             :: Double -> Double -> Double
##### SubD                             :: Double -> Double -> Double
Equivalent to min, max, \*, +, /, - For doubles in the expression language, respectively.


### Alias Functions for dyGraph

##### dyColor                       :: String -> SeriesList -> SeriesList
Sets the color of the resulting query graphs. Instructs dygraph to use `color` on these graph lines.

##### dyStrokeBorderColor           :: String -> SeriesList -> SeriesList
Sets the border color of the resulting query graphs. Instructs dygraph to use `strokeBorderColor` on these graph lines.

##### dyStrokeWidth                 :: Double -> SeriesList -> SeriesList
Sets the line width of the resulting query graphs. Instructs dygraph to use `strokeWidth` on these graph lines.

##### dyStrokeBorderWidth           :: Double -> SeriesList -> SeriesList
Sets the stroke width of the resulting query graphs. Instructs dygraph to use `strokeBorderWidth` on these graph lines.

##### dyStrokePattern               :: [Int] -> SeriesList -> SeriesList
Sets the stroke pattern of the resulting query graphs. Instructs dygraph to use `strokePattern` on these graph lines.

##### dyPointSize                   :: Double -> SeriesList -> SeriesList
Sets the size of points of the resulting query graphs. Instructs dygraph to use `pointSize` on these graph lines.

##### dyFillGraph                   :: Bool -> SeriesList -> SeriesList
Sets whether or not this graph will fill under the resulting query graphs. Instructs dygraph to use `fillGraph` on these graph lines.

##### dyFillAlpha                   :: Double -> SeriesList -> SeriesList
Sets the alpha of the fill of the resulting query graphs. Instructs dygraph to use `fillAlpha` on these graph lines.

##### dyAxis                        :: String -> SeriesList -> SeriesList
Sets the what graph to associate this with. Instructs dygraph to use `axis` on these graph lines.

##### dyDrawPoints                  :: Bool -> SeriesList -> SeriesList
Sets whether to draw the points of the resulting query graphs. Instructs dygraph to use `drawPoints` on these graph lines.

##### dyConnectSeparatedPoints      :: Bool -> SeriesList -> SeriesList
Sets the connection strategy of seperated points of the resulting query graphs. Use with `dyGraphNulls`. Instructs dygraph to use `connectSeparatedPoints` on these graph lines.

##### dyShowAnnotations             :: Bool -> SeriesList -> SeriesList
Sets the draw status of the annotation boxes of the resulting query graphs. Instructs dygraph to use `showAnnotations` on these graph lines.

##### dyAxisLogScaleY               :: Bool -> SeriesList -> SeriesList
Sets whether to try log scale for the "y" axis. Instructs dygraph to use `logScale` on "y" axis.

##### dyAxisLogScaleY2              :: Bool -> SeriesList -> SeriesList
Sets whether to try log scale for the "y2" axis. Instructs dygraph to use `logScale` on "y2" axis.

##### dyAxisIndependentTicksY       :: Bool -> SeriesList -> SeriesList
Sets wheter the "y" axis should use independant ticks from other axes. Instructs dygraph to use `independantTicks` on the "y" axis.

##### dyAxisIndependentTicksY2      :: Bool -> SeriesList -> SeriesList
Sets wheter the "y2" axis should use independant ticks from other axes. Instructs dygraph to use `independantTicks` on the "y2" axis.

##### dyAxisDrawAxisY               :: Bool -> SeriesList -> SeriesList
Sets whether to draw the "y" axis or not. Instructs dygraph to use `drawAxis` on the "y" axis.

##### dyAxisDrawAxisY2              :: Bool -> SeriesList -> SeriesList
Sets whether to draw the "y2" axis or not. Instructs dygraph to use `drawAxis` on the "y2" axis.

##### dyAxisIncludeZeroY            :: Bool -> SeriesList -> SeriesList
Sets whether zero should be included in the "y" axis. Instructs dygraph to use `includeZero` on the "y" axis.

##### dyAxisIncludeZeroY2           :: Bool -> SeriesList -> SeriesList
Sets whether zero should be included in the "y2" axis. Instructs dygraph to use `includeZero` on the "y" axis.

##### dyGraphNulls                  :: Bool -> SeriesList -> SeriesList
Sets the way dygraph will draw missing data points `NaN` (False [default] or `null` (True). `null` points can be connected by `dyConnectSeparatedPoints`. Instructs paradox frontend on how to draw missing data points.

##### dyStepPlot                    :: Bool -> SeriesList -> SeriesList
Sets whether or not dygraph should draw the graph as a step plot rather than line graph

##### dyForceStep                   :: SeriesList -> SeriesList
Sets dygraph to draw the graph as a step plot rather than line graph. Additionally enables connectSeperatedPoints, and graphNulls. Has good performance on graphs with large gaps in them

##### dyAddAxis                     :: String -> SeriesList -> SeriesList
Acts as a passtrough for data that should be associated with the results of a query but affects a graph,
specifically the addition of a different y axis. Used in conjunction with graphOptionS "axis" {name} to attach
a query to that axis.

##### dyStepNulls                   :: Bool -> SeriesList -> SeriesList
Sets whether dygraph should draw the graph as a step plot rather than line graph. Additionally enables connectSeperatedPoints, and graphNulls. Has good performance on graphs with large gaps in them

##### dyConstrainMin                :: Double -> SeriesList -> SeriesList
##### dyConstrainMax                :: Double -> SeriesList -> SeriesList
##### dyConstrainMinY2              :: Double -> SeriesList -> SeriesList
##### dyConstrainMaxY2              :: Double -> SeriesList -> SeriesList
Sets min or max constraints on a graph Y or Y2 axis.

##### dyConstrain                   :: Double -> Double -> SeriesList -> SeriesList
##### dyConstrainY2                 :: Double -> Double -> SeriesList -> SeriesList
Sets min (arg 1) and max (arg2) constraints on a graph Y or Y2 axis.
  * a.counter | dyConstrain 0.0 10.0

# API

### /eval

```
POST
```

```js
{ events=[
    { name -- string -- local identifier
    , ident -- string -- Ends with ':', server side dispatch identifier (to select what event backend to query)
    , keys=["key1", "key2"] -- keys to query event backend with (dependant on event backend)
    },...
  ]
, keys=["query", "other query",...]
, maxSamples == integer
, start -- timestamp
, stop -- timestamp
}
```

#### REPLY
```js
200

{ events={
    <event_key>=[
        { created_at -- timestamp -- when the event occurred
        , message -- strnig - simplified data
        ,... <event specific keys>
        }
    ],...
  }
, results={
    <query>={
          counters={
            <counter_name>={
                  data=[
                    { avg -- double -- average value at this bucket
                    , count -- integer -- number of collected data in this bucket
                    , min -- double -- minimum value at this bucket
                    , max -- double - maximum value at this bucket
                    , sdev -- double -- standard deviation at this bucket
                    , sum -- double -- sum of values in this bucket
                    , sumsq -- double - sum of squares at this time/bucket
                    , time -- timestamp -- time of this bucket
                  ]
                , end -- timestamp -- time of last bucket
                , interval -- integer -- interval in seconds between data
                , name -- string -- counter name
                , start -- timestamp -- start time of first bucket
                , graph_opt -- json -- json encdoded dictionary of graph options for rendering
            },...
        }
        , options={}
    }
  }
, interval -- integer -- interval in seconds between data
, start -- timestamp -- returned data start
, stop -- timestamp --returned data stop
}

|

422

{ status="error"
, message -- string - detailing error
}

|

500

Istatd Error message
```

### /render

#### PAYLOAD

```
POST
```

```js
{ start=Integer -- timestamp
, stop=Integer -- timestamp
, maxSamples=Integer -- Number of points to gather
, width=Integer -- [OPTIONAL=800] width of graphs
, height=Integer -- [OPTIONAL=600] height of each graph
, email=Text -- [OPTIONAL=None]email address to send svg to
, inline_html=Bool -- [OPTIONAL=False] should the svg be a document or inline_html element
, graphs={
    <key>={ -- key will be the title of the graph
          keys=["query","other query",...]
        , drawOptions={} -- currently unused, may control expression of svg
    }
}
```

#### REPLY
```
200

svg document | inline_html img with svg data  

|

400

{"json": "Malformed json"}  

```

### /render

#### PAYLOAD

```
GET

start=Integer -- timestamp  
stop=Integer -- timestamp  
maxSamples=Integer -- Number of points to gather  
width=Integer -- [OPTIONAL=800] width of graphs  
height=Integer -- [OPTIONAL=600] height of each graph  
email=Text -- [OPTIONAL=None] email address to send svg to  
inline_html=Bool -- [OPTIONAL=False] should the svg be a document or inline_html element

Graphs are specified by  

<key>=["query","other query",...] -- key will be the title of the graph  
```

#### REPLY
```
200

svg document | inline_html img with svg data  

|

400

{"json": "Malformed json"}  

```

### /email

#### PAYLOAD
```
POST
```

```js
{ start=Integer -- timestamp
, stop=Integer -- timestamp
, maxSamples=Integer -- Number of points to gather
, width=Integer -- [OPTIONAL=800] width of graphs
, height=Integer -- [OPTIONAL=600] height of each graph
, email=Text -- email address to send svg to
, graphs={
    <key>={ -- key will be the title of the graph
          keys=["query","other query",...]
        , drawOptions={} -- currently unused, may control expression of svg
    }
}
```

#### REPLY
```
200

{"success": true} | {"json": "Malformed json"}  

|

400

{"json": "Malformed json"}  
```



### /email

#### PAYLOAD

```
GET  

start=Integer -- timestamp  
stop=Integer -- timestamp  
maxSamples=Integer -- Number of points to gather  
width=Integer -- [OPTIONAL=800] width of graphs  
height=Integer -- [OPTIONAL=600] height of each graph  
email=Text -- email address to send svg to  

Graphs are specified by  

<key>=["query","other query",...] -- key will be the title of the graph  
```

#### REPLY
```
200 {"success": true}

|

400

{"json": "Malformed json"}  
```



### Query Examples

```
foo.web.response.5xx.role.bar-web-api | whenElse (hasAnyValue greaterThan getAvg 2.5) (dyFillAlpha 0.9 . dyFillGraph True . dyColor "darkred" . applyWithValue dyConstrainMax (getLargestInRange getAvg 10.0 . mulD 1.1 . largerD 2.5)) (dyConstrain 0.0 2.5)

foo.web.response.5xx.role.bar-web-api |-- Counter Query -> returns a list of counter data
    | |-- Infix function application, takes result of counter query to left and applies it to the function to the right
        whenElse |-- conditional function. Checks a predicate on the counter data passed in, then chooses to do the first action or the second based on True or False, respectively
            ( |-- start nested expression #1 . in this case the condition for whenElse
                hasAnyValue |-- predicate function. Checks if its predicate (1st arg) holds for the field (2nd arg) against the value (3rd arg). Will be given the counter data passed to the higher order function it was supplied to
                    greaterThan |-- predicate function. per bucket predicate.
                    getAvg |-- field accessor for buckets
                    2.5 |-- value. in this case the value supplied to the greaterThan predicated by hasAnyValue
            ) |-- end nested expression #1
            ( |-- start nested expression #2. in this case the True action to perform, when the condition holds
                dyFillAlpha |-- styling function. sets graph fill alpha
                    0.9 |-- value. argument for dyFillAlpha
                . |-- function composition
                dyFillGraph |-- styling function. sets fill graph status
                    True |-- value. argument for dyFillGraph
                . |-- function composition
                dyColor |-- styling function. sets color of line
                    "darkred" |-- value. argument for dyColor
                . |-- function composition
                applyWithValue |-- higher order application function. will apply its first argument with the results of the expression in its 2nd argument
                    dyConstrainMax |-- function to be applied. styling function. contrains a graphs max value
                    ( |-- start nested expression #3. Supplies a Double value to dyConstrainMax via applyWithValue
                        getLargestInRange |-- double provider function. will use arg1 to access a field of the counter data passed in, getting the largest value over the right biased percent range (2nd argument 10% from right of graph)
                            getAvg |-- field accessor for getLargestInRange.
                            10.0 |-- value. right biased, percent range for getLargestInRange
                        . |-- function composition
                        mulD |-- double multiplication
                            1.1 |-- value. arguemnt for mulD
                        . |-- function composition
                        largerD |-- double chooser. chooses larger value
                            2.5 |-- value. argument for largerD
                    ) |-- end nested expression #3
            ) |-- end nested expression #2
            ( |-- start nested expression #4. Else condition for whenElse.
                dyConstrain |-- styline function. constrains min and max
                    0.0 |-- value. min arg for dyConstrain
                    2.5 |-- value. max arg for dyConstrain
            ) |-- end nested expression #4
```

