#' Signalling classed conditions
#' 
#' These functions report a message, exactly as \code{\link{report}} does, but
#' first signal a condition which calling code can intercept. Attaching a class
#' to the condition allows the caller to decide how serious the situation is,
#' rather than the function which detects it having to be told in advance.
#' 
#' A function which cannot do what was asked of it has to decide whether that
#' is an error. Often only the caller knows: a missing file may be fatal in one
#' context and unremarkable in another. Signalling a classed condition lets the
#' function describe what happened and leave the severity to the caller:
#' 
#' \preformatted{    readFile <- function (path) \{
#'         if (!file.exists(path))
#'             return(signalWarning("File #\{path\} does not exist",
#'                                  class="missingFile", default=NULL))
#'         ...
#'     \}}
#' 
#' Callers who do nothing see the message reported at level \code{Warning} and
#' get \code{NULL} back. Callers who care can escalate, demote or suppress it
#' with \code{\link{reportAs}}, or handle it themselves with
#' \code{\link{tryCatch}} or \code{\link{withCallingHandlers}}.
#' 
#' The \code{default} argument gives the value that the signalling function
#' returns if the condition goes unhandled. Supplying it, even as \code{NULL},
#' also marks the condition \emph{recoverable}, which declares that execution
#' can sensibly continue past this point with that value. Only recoverable
#' conditions can have an \code{Error} demoted to a lower level by
#' \code{\link{reportAs}}, since code written on the assumption that an error
#' never returns is not generally safe to resume.
#' 
#' Unlike \code{\link{report}}, these functions always signal their condition,
#' even when the current output level means that nothing will be reported. The
#' condition is the point of calling them, so it is never skipped.
#' 
#' @param level The level of the message. See \code{\link{report}} for the
#'   available levels and the ways they may be named.
#' @param \dots Objects which can be coerced to mode \code{character}, and
#'   optionally arguments to \code{\link[ore]{es}} such as \code{round}. These
#'   are handled exactly as by \code{\link{report}}.
#' @param class A character vector of classes to attach to the condition, or
#'   \code{NULL}. A class derived from the level of the message, such as
#'   \code{"reportrWarning"}, is always attached in addition to these.
#' @param default The value to return if the condition is not handled. If it is
#'   not specified at all the value is \code{NULL}, but the condition is marked
#'   as not recoverable. See Details.
#' @param call The call to associate with the condition. Defaults to the call
#'   of the function which is signalling.
#' @param prefixFormat The format of the string prepended to the message. See
#'   \code{\link{report}}.
#' 
#' @return The value of \code{default}, or a value supplied by a handler
#'   through the \code{useValue} restart, invisibly. \code{signalError} does
#'   not return unless a handler recovers from it.
#' 
#' @examples
#' setOutputLevel(OL$Info)
#' 
#' findThing <- function (name) {
#'     if (name != "widget")
#'         return(signalWarning("There is no #{name}", class="missingThing", default=NA))
#'     return("the widget")
#' }
#' 
#' # Reported as a warning, and NA returned
#' findThing("sprocket")
#' 
#' # Treated as an error instead
#' \dontrun{reportAs(findThing("sprocket"), missingThing=Error)}
#' 
#' # Suppressed entirely
#' reportAs(findThing("sprocket"), missingThing=Ignore)
#' 
#' @seealso \code{\link{reportAs}} for acting on these conditions,
#'   \code{\link{reportrCondition}} for the structure of the condition objects,
#'   and \code{\link{report}} for reporting without a class.
#' @author Jon Clayden
#' @aliases signalDebug signalVerbose signalInfo signalWarning signalError
#' @export
signal <- function (level, ..., class = NULL, default = .noDefault, call = sys.call(-1), prefixFormat = NULL)
{
    .signalPublic(.evaluateLevel(level), ..., class=class, default=default, call=call, prefixFormat=prefixFormat, .envir=parent.frame())
}

#' @rdname signal
#' @export
signalDebug <- function (..., class = NULL, default = .noDefault, call = sys.call(-1), prefixFormat = NULL)
{
    .signalPublic(OL$Debug, ..., class=class, default=default, call=call, prefixFormat=prefixFormat, .envir=parent.frame())
}

#' @rdname signal
#' @export
signalVerbose <- function (..., class = NULL, default = .noDefault, call = sys.call(-1), prefixFormat = NULL)
{
    .signalPublic(OL$Verbose, ..., class=class, default=default, call=call, prefixFormat=prefixFormat, .envir=parent.frame())
}

#' @rdname signal
#' @export
signalInfo <- function (..., class = NULL, default = .noDefault, call = sys.call(-1), prefixFormat = NULL)
{
    .signalPublic(OL$Info, ..., class=class, default=default, call=call, prefixFormat=prefixFormat, .envir=parent.frame())
}

#' @rdname signal
#' @export
signalWarning <- function (..., class = NULL, default = .noDefault, call = sys.call(-1), prefixFormat = NULL)
{
    .signalPublic(OL$Warning, ..., class=class, default=default, call=call, prefixFormat=prefixFormat, .envir=parent.frame())
}

#' @rdname signal
#' @export
signalError <- function (..., class = NULL, default = .noDefault, call = sys.call(-1), prefixFormat = NULL)
{
    .signalPublic(OL$Error, ..., class=class, default=default, call=call, prefixFormat=prefixFormat, .envir=parent.frame())
}

.signalPublic <- function (level, ..., class, default, call, prefixFormat, .envir)
{
    .signal(level, .buildMessage(..., .envir=.envir), class=class, call=call,
            prefixFormat=prefixFormat, default=default, outputLevel=.outputLevel())
}
