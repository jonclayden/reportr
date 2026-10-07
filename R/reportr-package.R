#' reportr: A general message and error reporting system
#'
#' The reportr package provides alternatives to [message()], [warning()] and
#' [stop()], with output consolidation, message filtering, expression
#' substitution, automatic stack traces for debugging, and reporting which
#' depends on the current output level. Every message is also signalled as a
#' classed R condition, so that calling code can decide how serious a situation
#' is, rather than the function which detects it having to be told in advance.
#'
#' @section Levels:
#' Every message has a level. The levels are given by the `OL` object, a list
#' whose components are, in ascending order of priority, `Debug`, `Verbose`,
#' `Info`, `Warning`, `Question`, `Error` and `Fatal`. Messages at level `Error`
#' or `Fatal` stop execution. There is also an `Ignore` level, below `Debug`,
#' which is used with [reportAs()] to suppress a class of message entirely.
#'
#' Wherever reportr expects a level, it may be given as `OL$Info`, as the bare
#' name `Info`, or as the string `"Info"`.
#'
#' @section Main functions:
#' * [report()], [flag()] and [assert()] report messages, and [ask()] asks
#'   questions.
#' * [fallback()] raises an error which the caller may choose to demote, in
#'   return for a substitute value.
#' * [reportAs()] changes the level of particular classes of message, and
#'   [withReportrHandlers()] brings all of R's messages, warnings and errors
#'   under reportr's control. See [handlers].
#' * [setOutputLevel()] controls which messages are written, and
#'   [setOutputTargets()] where they are written and how they are formatted.
#'   See [targets].
#' * [reportrCondition()] describes the condition objects signalled for each
#'   message.
#'
#' @section Options:
#' The following options influence reportr's behaviour. Each has a default
#' which applies if it is not set.
#'
#' * `reportrOutputLevel`: The current output level, which is more usually set
#'   using [setOutputLevel()].
#' * `reportrPrefixFormat`: The prefix format used by output targets which
#'   don't specify their own; see [targets]. The default is `"%d%L: "`.
#' * `reportrTimeFormat`: The format used for the time in prefixes, as
#'   understood by [format.POSIXct()]. The default is `"%Y-%m-%d %H:%M:%S"`.
#' * `reportrStderrLevel`: The level at and above which the terminal writes
#'   messages to standard error rather than standard output, unless the
#'   terminal target specifies its own. The default is `Warning`.
#' * `reportrStackTraceLevel`: The level at and above which a stack trace is
#'   written with each message, when the output level is `Debug`. The default
#'   is `Error`.
#' * `reportrMessageFilterIn`, `reportrMessageFilterOut`: Perl-style regular
#'   expressions. If set, only messages which match the first, and do not
#'   match the second, are reported. Filtering affects only what is reported:
#'   the corresponding condition is signalled either way, and a filtered error
#'   is still fatal.
#' * `reportrBaseClasses`: Should reportr conditions also inherit from base R's
#'   `"message"` and `"warning"` classes? The default is `FALSE`; see
#'   [reportrCondition()].
#'
#' @author Jon Clayden
#' @aliases OL
"_PACKAGE"
