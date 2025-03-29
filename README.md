[![CRAN version](http://www.r-pkg.org/badges/version/reportr)](https://cran.r-project.org/package=reportr) [![CI](https://github.com/jonclayden/reportr/actions/workflows/ci.yaml/badge.svg)](https://github.com/jonclayden/reportr/actions/workflows/ci.yaml) [![codecov](https://codecov.io/gh/jonclayden/reportr/graph/badge.svg?token=WAGDWQjJRM)](https://app.codecov.io/gh/jonclayden/reportr)

# reportr: Message reporting for R

The `reportr` package for R is a simple alternative to R's standard functions for producing informative output, such as `message()`, `warning()` and `stop()`. It offers a certain amount more flexibility than these functions, although it is not (yet) a full logging solution. The package is available on CRAN.

Further alternatives in this area include the [`futile.logger`](https://cran.r-project.org/package=futile.logger), [`log4r`](https://cran.r-project.org/package=log4r) and [`logging`](https://cran.r-project.org/package=logging) packages.

## Contents

- [Usage](#usage)
- [Using `reportr` to handle standard messages](#using-reportr-to-handle-standard-messages)
- [Output consolidation](#output-consolidation)
- [Expression substitution](#expression-substitution)
- [Message filtering](#message-filtering)
- [Stack tracing](#stack-tracing)
- [The `Question` reporting level](#the-question-reporting-level)

## Usage

The key functions in the package are `report()` and `flag()`. Both take a reporting level as their first argument, which determines the priority of the message. The functions differ only in that `report()` reports the message to the console immediately, whereas `flag()`, like `warning()`, saves it for reporting later. Flagged messages are reported at the next `report()` call, to keep output in order.

The `setOutputLevel()` function determines the minimal reporting level for which output is actually generated; messages below this level are discarded. This is demonstrated below, by creating a message of level `Info` with the output level set first to `Warning` (which produces nothing) and then to `Info` (which allows the message to be reported).

```r
setOutputLevel(Warning)
report(Info, "Test message")

setOutputLevel(Info)
report(Info, "Test message")
## INFO: Test message
```

The reporting levels available, in ascending order of priority, are currently `Debug`, `Verbose`, `Info`, `Warning`, `Question`, `Error` and `Fatal`. The `Error` level raises R's "abort" condition, like `stop()`, so execution will stop. The `Fatal` level is in practice never used, but can be set as the output level to subdue all reporting.

## Using `reportr` to handle standard messages

It is possible to arrange for `reportr` to handle messages, warnings and errors raised by code that does not itself use `report()` or `flag()`, by using the wrapper function `withReportrHandlers()`. Take, for example, the simple function

```r
f <- function() message("In f()")
```

Ordinarily calling this function produces

```r
f()
## In f()
```

Wrapping this will instead give

```r
withReportrHandlers(f())
## * INFO: In f()
```

Note that calls to `message(...)` will translate to `report(Info, ...)`, `warning(...)` to `flag(Warning, ...)`, and `stop(...)` to `report(Error, ...)`.

This mechanism is not particularly advantageous in itself, but it allows the user to exercise all of the `reportr` control options over all code, whether or not it was written with `reportr` in mind. These facilities are laid out below.

## Output consolidation

Sometimes the same warning is generated many times, and seeing a lot of replication isn't particularly helpful. The following function is somewhat artificial, since it does not take advantage of vectorisation in R, but it illustrates the point.

```r
f <- function(xs) {
    ys <- numeric(0)
    for (x in xs) ys <- c(ys,sqrt(x))
    ys
}
```

Now let's call the function with input that will produce some warnings.

```r
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

This duplication of warnings is verbose and unnecessary, and can be consolidated with `reportr`:

```r
withReportrHandlers(f(-5:5))
## WARNING: [x5] NaNs produced
##  [1]      NaN      NaN      NaN      NaN      NaN 0.000000 1.000000 1.414214
##  [9] 1.732051 2.000000 2.236068
```

Notice that the five duplicated warnings are reported in one line in this case, along with an indication that the message was produced five times.

## Expression substitution

Expression substitution is an alternative to `printf`-style syntax for incorporating the values of R expressions into strings. It can make messages more readable, and reduce the need for lots of quotation marks. All `reportr` messages are passed through the `es()` function for expression substitution, which is part of the [`ore` package](https://github.com/jonclayden/ore). For example,

```r
x <- 3
report(Info, "The value of x is #{x}")
## INFO: The value of x is 3
```

Note the `#{}` syntax. Everything within the curly braces is evaluated as an R expression, and the result inserted into the string. Please see `?ore::es` for more details.

## Message filtering

Sometimes it may be desirable to discard particular messages that would otherwise be reported at the current output level. There are two global options that allow this, `reportrMessageFilterIn` and `reportrMessageFilterOut`, each of which takes a Perl-style regular expression. The "in" filter is applied first, keeping only messages that match its regex, and then the "out" filter, which keeps only messages that do not match its regex.

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

## Stack tracing

The `Debug` reporting level is slightly special. When the current output level is `Debug`, not only are all messages reported, but stack traces are also automatically provided when the message being reported is of level equal to or above the `reportrStackTraceLevel` option (the default is `Error`). For example,

```r
f <- function(x) if (!is.numeric(x)) stop("x must be numeric")
g <- function(x) f(x)

withReportrHandlers(g("text"))
## * * ERROR: x must be numeric (in "f(x)")
## --- Begin stack trace ---
## * g("text")
## * * f(x)
## ---  End stack trace  ---
```

Notice that the number of asterisks in front of the printed message indicates the depth in the stack of the function reporting the message. This can be useful, in a long stream of output, to determine the structure of the reporting code at a glance. The format of this "prefix" can be customised, however: please see `?report` for details.

## The `Question` reporting level

In between the `Warning` and `Error` reporting levels is a level called `Question`. This level is used by a function called `ask()`, which prompts the user for input and returns the result in a string. If the output level is greater than `Question`, or the session is not interactive, a customisable default value is returned.

```r
f <- function() {
    name <- ask("What is your name?", default="Nobody")
    report(OL$Info, "Hello, #{name}")
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
