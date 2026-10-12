[![CRAN version](http://www.r-pkg.org/badges/version/reportr)](https://cran.r-project.org/package=reportr) [![CI](https://github.com/jonclayden/reportr/actions/workflows/ci.yaml/badge.svg)](https://github.com/jonclayden/reportr/actions/workflows/ci.yaml) [![codecov](https://codecov.io/gh/jonclayden/reportr/graph/badge.svg?token=WAGDWQjJRM)](https://app.codecov.io/gh/jonclayden/reportr)

# reportr: Message reporting for R

The `reportr` package for R is an alternative to R's standard functions for producing informative output, such as `message()`, `warning()` and `stop()`. It is as simple to use as they are, but it gives you, and the people who call your code, much more control over what is reported and how. In particular,

- **One setting controls verbosity everywhere.** Every message has a level, from `Debug` to `Fatal`, and a single call to `setOutputLevel()` decides which are written. There is no need to thread `verbose` or `quiet` arguments through your functions.
- **The caller decides how serious a problem is.** Messages are signalled as classed R conditions, so code calling a function can escalate, demote or silence particular kinds of problem with `reportAs()`, instead of the function needing an argument such as `mustWork` in `normalizePath()`. With `fallback()`, a function can raise an error which callers may choose to treat as a warning, returning a substitute value.
- **Output is easier to read.** Messages show their level, and are indented to show the depth of the call they came from. Repeated warnings are consolidated into one line, and a stack trace can be shown automatically when debugging.
- **Output can go where it's needed.** Messages can be written to log files, connections or functions as well as the terminal, each with its own level threshold and format.
- **It works with existing code.** R's own messages, warnings and errors, including those from other packages, can be brought under the same control with `withReportrHandlers()`.

It also offers expression substitution in messages, assertions, message filtering, and questions for the user which work in scripts as well as interactively. It is not a full logging solution, but it covers the needs of many packages and scripts. The package is available on CRAN.

Further alternatives in this area include the [`futile.logger`](https://cran.r-project.org/package=futile.logger), [`log4r`](https://cran.r-project.org/package=log4r) and [`logging`](https://cran.r-project.org/package=logging) packages.

## Contents

- [Getting started](#getting-started)
- [Expression substitution](#expression-substitution)
- [Flagging and consolidating messages](#flagging-and-consolidating-messages)
- [Assertions](#assertions)
- [Asking questions](#asking-questions)
- [Prefixes and stack traces](#prefixes-and-stack-traces)
- [Output targets](#output-targets)
- [Message filtering](#message-filtering)
- [Handling R's own messages, warnings and errors](#handling-rs-own-messages-warnings-and-errors)
- [Conditions](#conditions)
- [Letting the caller decide](#letting-the-caller-decide)
- [Options](#options)

## Getting started

The main function in the package is `report()`. It takes a reporting level as its first argument, which determines the priority of the message, followed by the message itself.

The `setOutputLevel()` function sets the minimum level of message which is actually written; messages below this level are discarded. This is demonstrated below, by creating a message of level `Info` with the output level set first to `Warning` (which produces nothing) and then to `Info` (which allows the message to be reported).

```r
library(reportr)

setOutputLevel(Warning)
report(Info, "Test message")

setOutputLevel(Info)
report(Info, "Test message")
## INFO: Test message
```

The reporting levels available, in ascending order of priority, are `Debug`, `Verbose`, `Info`, `Warning`, `Question`, `Error` and `Fatal`. A level can be given as a bare name, as above, or as `OL$Info` or `"Info"`, which are equivalent. If a message is reported before the output level has been set, it defaults to `Info`. There is also an `Ignore` level, below `Debug`, which is used to [suppress classes of message entirely](#letting-the-caller-decide).

Messages at level `Error` or above stop execution, like `stop()`, whatever the current output level:

```r
report(Error, "Something went wrong")
## ERROR: Something went wrong
```

Messages at level `Warning` and above are written to standard error, and others to standard output.

## Expression substitution

Expression substitution is an alternative to `printf`-style syntax for incorporating the values of R expressions into strings. It can make messages more readable, and reduce the need for lots of quotation marks. All `reportr` messages are passed through the `es()` function for expression substitution, which is part of the [`ore` package](https://github.com/jonclayden/ore). For example,

```r
x <- 3
report(Info, "The value of x is #{x}")
## INFO: The value of x is 3

report(Info, "Pi is roughly #{pi}", round=2)
## INFO: Pi is roughly 3.14
```

Everything within the curly braces is evaluated as an R expression, and the result inserted into the string. Extra arguments such as `round` and `signif` are passed on to `es()`; please see `?ore::es` for more details.

## Flagging and consolidating messages

The `flag()` function is called just like `report()`, but stores its message to be reported later, much as R holds on to warnings. Flagged messages are reported when the next message is reported, to keep output in order, or when `reportFlags()` is called. Identical messages are consolidated into one, with a count:

```r
flag(Warning, "Something odd happened")
flag(Warning, "Something odd happened")
report(Info, "Carrying on")
## WARNING: [x2] Something odd happened
## INFO: Carrying on
```

`clearFlags()` discards any stored messages without reporting them.

## Assertions

`assert()` reports a message only if an expression is not `TRUE`. By default the message is an error, but another level may be given:

```r
x <- 3
assert(x > 2, "x is too small")

assert(x > 5, "x is too small", level=Warning)
## WARNING: x is too small
```

An expression which cannot be evaluated, perhaps because it refers to a variable which doesn't exist, counts as a failed assertion.

## Asking questions

In between the `Warning` and `Error` levels is a level called `Question`. This level is used by `ask()`, which prompts the user for input and returns the result as a string. If the output level is above `Question`, or there is nobody to ask, a default value is returned instead.

```r
f <- function() {
    name <- ask("What is your name?", default="Nobody")
    report(Info, "Hello, #{name}")
    invisible(name)
}

setOutputLevel(Info)
f()
## * QUESTION: What is your name? User
## * INFO: Hello, User

setOutputLevel(Error)
(f())
## [1] "Nobody"
```

A `valid` argument may also be given, in which case the question is asked repeatedly until one of the valid responses is given.

Questions work in scripts run with `Rscript`, too, as long as they are run from a terminal: although the session is not interactive, `ask()` reads the answer directly from standard input. If standard input is not a terminal, as when a script is run by a scheduler or with its input redirected, the default is returned. Setting the `reportrAskInScripts` option to `FALSE` makes `ask()` return the default in any non-interactive session, so that a script never waits for input.

## Prefixes and stack traces

Each message is written with a prefix. The default prefix format is `"%d%L: "`, where `%d` expands to one asterisk for each level of function call depth at which the message arose, and `%L` to the level of the message in upper case. The number of asterisks makes it possible to see the structure of the reporting code at a glance, in a long stream of output.

```r
g <- function() report(Info, "Inside g()")
f <- function() {
    report(Info, "Inside f()")
    g()
}

f()
## * INFO: Inside f()
## * * INFO: Inside g()
```

Other escapes are available: `%f` gives the name of the function which raised the message, `%l` the level in lower case, `%p` the process ID, and `%t` the current time, formatted according to the `reportrTimeFormat` option. The default format can be changed with the `reportrPrefixFormat` option:

```r
options(reportrPrefixFormat="[%f] %l: ")
f()
## [f] info: Inside f()
## [g] info: Inside g()
```

Each [output target](#output-targets) can also have its own prefix format. Individual messages, such as headings, can be written without any prefix by passing `prefix=FALSE` to `report()`.

The `Debug` level is slightly special. When the output level is `Debug`, not only are all messages reported, but a stack trace is also written with any message whose level is at least the `reportrStackTraceLevel` option (by default, `Error`). For example,

```r
f <- function(x) if (!is.numeric(x)) report(Error, "x must be numeric")
g <- function(x) f(x)

setOutputLevel(Debug)
g("text")
## * * ERROR: x must be numeric
## --- Begin stack trace ---
## * g("text")
## * * f(x)
## ---  End stack trace  ---
```

## Output targets

By default, messages are written to the terminal. The `setOutputTargets()` function replaces the current output targets, or adds to them, using target objects created by `toTerminal()`, `toFile()` and `toFunction()`. Each target may have its own minimum level, in addition to the overall output level, and its own prefix format. For example, the following writes everything to a log file, with a timestamp, while the terminal shows only warnings and errors:

```r
setOutputLevel(Debug)
old <- setOutputTargets(toTerminal(stdout=Warning),
                        toFile("run.log", prefix="%t %L: "))
```

A file target may also be given a connection rather than a path. A function target is called with the formatted text, the level of the message and its condition object, so it can send messages anywhere at all. The previous targets are returned by `setOutputTargets()`, invisibly, so they can be restored afterwards:

```r
setOutputTargets(old)
```

Passing `add=TRUE` adds the given targets to the current ones, and `getOutputTargets()` returns the current set.

## Message filtering

Sometimes it may be desirable to discard particular messages that would otherwise be reported at the current output level. There are two options that allow this, `reportrMessageFilterIn` and `reportrMessageFilterOut`, each of which takes a regular expression. The "in" filter is applied first, keeping only messages that match it, and then the "out" filter, which keeps only messages that do not match it.

```r
f <- function() {
    report(Info, "One")
    report(Info, "Two")
    report(Info, "Three")
}

options(reportrMessageFilterIn="^T")
f()
## * INFO: Two
## * INFO: Three

options(reportrMessageFilterIn=NULL, reportrMessageFilterOut="^T")
f()
## * INFO: One
```

Filtering affects only what is written; the corresponding [condition](#conditions) is signalled either way, and a filtered error still stops execution.

## Handling R's own messages, warnings and errors

`reportr` can also handle messages, warnings and errors raised by code that does not itself use the package, via the wrapper function `withReportrHandlers()`. Calls to `message()` are reported at level `Info`, `warning()` is flagged at level `Warning`, and `stop()` is reported at level `Error`. This allows all of `reportr`'s output controls to be applied to any code, whether or not it was written with `reportr` in mind.

One benefit is consolidation of repeated warnings. The following function is somewhat artificial, since it does not take advantage of vectorisation in R, but it illustrates the point.

```r
f <- function(xs) {
    ys <- numeric(0)
    for (x in xs) ys <- c(ys, sqrt(x))
    ys
}

f(-5:5)
##  [1]      NaN      NaN      NaN      NaN      NaN 0.000000 1.000000 1.414214
##  [9] 1.732051 2.000000 2.236068
## Warning messages:
## 1: In sqrt(x) : NaNs produced
## 2: In sqrt(x) : NaNs produced
## 3: In sqrt(x) : NaNs produced
## 4: In sqrt(x) : NaNs produced
## 5: In sqrt(x) : NaNs produced
```

This duplication of warnings is verbose and unnecessary, and is consolidated by `reportr` into one line, along with an indication that the message was produced five times:

```r
withReportrHandlers(f(-5:5))
## * WARNING: [x5] NaNs produced
##  [1]      NaN      NaN      NaN      NaN      NaN 0.000000 1.000000 1.414214
##  [9] 1.732051 2.000000 2.236068
```

In R 4.0.0 and later, `reportrHandlers()` installs the same handlers for the rest of the session, so that code need not be wrapped at all. It must be called at top level, and `reportrHandlers(FALSE)` removes the handlers again.

## Conditions

Every message is signalled as an R condition before it is reported, so calling code can intercept it. The condition's class vector always includes one derived from the level of the message, such as `"reportrWarning"`, and errors also inherit from `"error"`, so they behave like any other R error:

```r
tryCatch(report(Error, "Something broke"), error=function(e) conditionMessage(e))
## [1] "Something broke"
```

Unlike `stop()`, though, `reportr` writes the message itself and does not let R print a second one. If nothing handles an error, execution simply ends.

Further classes can be attached to a message using the `class` argument to `report()`, `flag()` or `assert()`. Classed messages are signalled even when the output level means that they will not be written, since the class is there for the benefit of callers. The condition object also carries the level of the message and the call stack in effect when it arose; see `?reportrCondition` for details.

By default, `reportr` conditions below level `Error` do not inherit from `"warning"` or `"message"`, so that unrelated code which handles those classes doesn't silence `reportr` output by accident. Setting the `reportrBaseClasses` option to `TRUE` changes this, so that functions such as `suppressWarnings()` and `suppressMessages()` affect them.

## Letting the caller decide

Often a function cannot know how serious a problem is: a missing file may be fatal in one context and unremarkable in another. The usual workaround is an argument which tells the function what to do, such as the `mustWork` argument to base R's `normalizePath()`, which gives an error, a warning or nothing at all when a path doesn't exist. But that puts the decision in the wrong place, and the argument has to be threaded through every intervening layer.

Instead, the function can describe what happened and attach a class to it. For a problem that should simply be reported, `report()` with a `class` argument is all that's needed:

```r
readValues <- function (text) {
    values <- suppressWarnings(as.numeric(strsplit(text, ",")[[1]]))
    if (anyNA(values))
        report(Warning, "Some values are not numeric", class="invalidValues")
    return(values)
}
```

Callers who do nothing get the message reported as a warning, along with the partial result. Callers who need every value to be valid, or who don't care, use `reportAs()` to say how serious the problem is for them:

```r
readValues("1,2,x")
## * WARNING: Some values are not numeric
## [1]  1  2 NA

reportAs(readValues("1,2,x"), invalidValues=Error)
## * ERROR: Some values are not numeric

reportAs(readValues("1,2,x"), invalidValues=Ignore)
## [1]  1  2 NA
```

Escalating a condition like this is always safe, because execution just stops sooner than it would have. Demoting an error is different, since code which raises an error generally isn't written to carry on afterwards. When a function can't do what was asked of it, but has a value it could sensibly return instead, it can say so with `fallback()`:

```r
findThing <- function (name) {
    if (name != "widget")
        return(fallback(NA, "There is no #{name}", class="missingThing"))
    return("the widget")
}
```

This is an error by default, but one the caller may demote, in which case the function returns the fallback value:

```r
findThing("sprocket")
## * ERROR: There is no sprocket

reportAs(findThing("sprocket"), missingThing=Warning)
## * WARNING: There is no sprocket
## [1] NA
```

Errors raised in any other way, including by `report()` and `assert()`, cannot be demoted, because they have no value to continue with. An attempt to do so produces a warning, and the error stands. As a rule of thumb, choose the default level to suit a caller who does nothing. If the fallback value would mislead code expecting a real result, make it an error; if the problem is visible in the result itself, report it at a lower level and let strict callers escalate it.

For finer control, `reportAs()` also accepts handler functions, which may invoke the `muffleReport` restart to suppress a message, or, for a condition raised by `fallback()`, the `useValue` restart to supply a different value:

```r
reportAs(findThing("sprocket"),
         missingThing=function (cond) invokeRestart("useValue", "a sprocket"))
## [1] "a sprocket"
```

`withReportrHandlers()` accepts the same arguments as `reportAs()`, so the two can be combined.

## Options

The following options affect the behaviour of the package.

| Option | Default | Purpose |
| ------ | ------- | ------- |
| `reportrOutputLevel` | `OL$Info` (with a message) | The output level, usually set with `setOutputLevel()` |
| `reportrPrefixFormat` | `"%d%L: "` | The default prefix format for output targets |
| `reportrTimeFormat` | `"%Y-%m-%d %H:%M:%S"` | The format for times in prefixes, as used by `format()` |
| `reportrStderrLevel` | `OL$Warning` | The minimum level written to standard error by the terminal target |
| `reportrStackTraceLevel` | `OL$Error` | The minimum level at which stack traces are written, at output level `Debug` |
| `reportrMessageFilterIn` | `NULL` | A regular expression which messages must match to be written |
| `reportrMessageFilterOut` | `NULL` | A regular expression which messages must not match to be written |
| `reportrBaseClasses` | `FALSE` | Whether conditions below level `Error` should also inherit from `"warning"` or `"message"` |
| `reportrAskInScripts` | `TRUE` | Whether `ask()` should read from a terminal in non-interactive sessions, such as under `Rscript` |
