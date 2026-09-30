#' Returning a fallback value, with a recoverable error
#'
#' A function which cannot do what was asked of it can use \code{fallback} to
#' report the problem and return a substitute value instead. By default the
#' problem is an error, but one which the caller may choose to demote, in which
#' case the substitute value is returned and execution continues.
#'
#' Often only the caller knows how serious a problem is: a missing file may be
#' fatal in one context and unremarkable in another. The usual workaround is a
#' logical argument such as \code{errorIfMissing}, but that puts the decision
#' in the wrong place, and has to be passed down through every intervening
#' layer. Instead, the function can signal a classed condition, and supply the
#' value it would return if the caller decides that execution should continue:
#'
#' \preformatted{    locateFile <- function (path) \{
#'         if (!file.exists(path))
#'             return(fallback(NULL, "File #\{path\} does not exist",
#'                             class="missingFile"))
#'         ...
#'     \}}
#'
#' Callers who do nothing get an error. Callers for whom a missing file is
#' expected can say so with \code{reportAs(locateFile(path),
#' missingFile=Debug)}, and get \code{NULL} back. A handler may also supply a
#' different value, using the \code{useValue} restart described in
#' \code{\link{reportrCondition}}.
#'
#' Escalating a condition to \code{Error} is always safe, because execution
#' simply stops sooner than it would have. Demoting an error is only safe when
#' the code which signalled it has said what value to continue with, and so
#' \code{fallback} is the only way to signal an error which can be demoted. A
#' condition which should merely be reported, whatever its class, is better
#' signalled with \code{\link{report}}; and a requirement which must hold, with
#' nothing sensible to return if it does not, with \code{\link{assert}}.
#'
#' The level should suit a caller who does nothing, and \code{Error} is usually
#' right when the value returned would mislead code that expected a real
#' result, since the mistake would otherwise surface later and somewhere else.
#' Where the problem is visible in the result itself, such as a partial result
#' containing \code{NA} values, a lower level with \code{\link{report}} and an
#' ordinary return lets strict callers escalate it instead. A lower level may
#' nevertheless be given here, which allows a handler to substitute a value.
#'
#' Like the other signalling functions, \code{fallback} signals its condition
#' even when the current output level means that nothing will be reported.
#'
#' @param value The value to return if the condition is not handled, or is
#'   handled by demoting it. It may be \code{NULL}, but must be given.
#' @param \dots Objects which can be coerced to mode \code{character}, and
#'   optionally arguments to \code{\link[ore]{es}} such as \code{round}. These
#'   are handled exactly as by \code{\link{report}}.
#' @param class A character vector of classes to attach to the condition, or
#'   \code{NULL}. A class derived from the level of the message, such as
#'   \code{"reportrError"}, is always attached in addition to these.
#' @param level The level of the message. See \code{\link{report}} for the
#'   available levels and the ways they may be named.
#' @param call The call to associate with the condition. Defaults to the call
#'   of the function which is signalling.
#'
#' @return The value of \code{value}, or a value supplied by a handler through
#'   the \code{useValue} restart. At level \code{Error} or above
#'   \code{fallback} does not return unless the condition is handled.
#'
#' @examples
#' setOutputLevel(OL$Info)
#'
#' findThing <- function (name) {
#'     if (name != "widget")
#'         return(fallback(NA, "There is no #{name}", class="missingThing"))
#'     return("the widget")
#' }
#'
#' # An error, if the caller does nothing
#' \dontrun{findThing("sprocket")}
#'
#' # Reported as a warning instead, and NA returned
#' reportAs(findThing("sprocket"), missingThing=Warning)
#'
#' # Suppressed entirely
#' reportAs(findThing("sprocket"), missingThing=Ignore)
#'
#' @seealso \code{\link{reportAs}} for acting on these conditions,
#'   \code{\link{reportrCondition}} for the structure of the condition objects,
#'   and \code{\link{report}} for conditions which are only reported.
#' @author Jon Clayden
#' @export
fallback <- function (value, ..., class = NULL, level = OL$Error, call = sys.call(-1))
{
    if (missing(value))
        report(OL$Error, "A value to fall back on must be given")

    level <- .evaluateLevel(level)
    message <- .buildMessage(..., .envir=parent.frame())

    # Unlike the other signalling functions, the value is the point here, so
    # it is returned visibly
    result <- .signal(level, message, class=class, call=call, value=value, recoverable=TRUE)
    return (result)
}
