#' Handling reportr conditions
#'
#' These functions evaluate an expression in a context where conditions are
#' handled by \code{reportr}. \code{reportAs} changes the level at which
#' particular classes of condition are reported, which is how a caller decides
#' how serious a situation is. \code{withReportrHandlers} additionally arranges
#' for R's own messages, warnings and errors to be reported by \code{reportr},
#' and \code{reportrHandlers} does the same for a whole session.
#'
#' Conditions to remap are given as named arguments, where the name is the
#' class of condition to match and the value is the level at which it should be
#' reported. Levels may be named in any of the usual ways, so \code{Error},
#' \code{OL$Error} and \code{"Error"} are equivalent. Mapping a class to
#' \code{Ignore} suppresses it entirely.
#'
#' \preformatted{    # A missing file is an error here ...
#'     file <- reportAs(readFile(path), missingFile=Error)
#'
#'     # ... but merely worth noting here
#'     file <- reportAs(readFile(path), missingFile=Debug)}
#'
#' Escalating a condition is always safe. Demoting one below \code{Error} is
#' not, because code which signalled an error was generally not written to
#' carry on afterwards. A demotion is therefore honoured only for conditions
#' which were signalled with an explicit \code{default} value, and so declared
#' recoverable; see \code{\link{signal}}. Demoting any other error reports it
#' at the requested level, but it remains fatal.
#'
#' A named argument may also be a function, in which case it is used as a
#' calling handler for that class of condition, exactly as it would be by
#' \code{\link{withCallingHandlers}}. Such a handler may invoke the
#' \code{muffleReport} or \code{useValue} restarts; see
#' \code{\link{reportrCondition}}.
#'
#' \code{withReportrHandlers} also translates conditions raised by code which
#' knows nothing of \code{reportr}: calls to \code{\link{message}} are reported
#' at level \code{Info}, \code{\link{warning}} is flagged at level
#' \code{Warning}, and \code{\link{stop}} is reported at level \code{Error}.
#' The classes of the original condition are carried over to the \code{reportr}
#' condition, and the original is stored in its \code{original} element, so a
#' handler further out can still match it by class.
#'
#' \code{reportrHandlers(TRUE)} installs those same translations for the rest
#' of the session, using \code{\link{globalCallingHandlers}}, so that code need
#' not be wrapped at all. Like that function, it may only be called when no
#' handlers are already established, which in practice means at top level.
#' \code{reportrHandlers(FALSE)} removes them again.
#'
#' @param expr An expression to evaluate.
#' @param \dots Named arguments mapping condition classes to levels, or to
#'   handler functions. See Details.
#' @param install Should the session-wide handlers be installed, or removed?
#'
#' @return \code{reportAs} and \code{withReportrHandlers} return the value of
#'   \code{expr}. \code{reportrHandlers} returns \code{NULL}, invisibly.
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
#' # Reported as a warning by default
#' findThing("sprocket")
#'
#' # Suppressed entirely
#' reportAs(findThing("sprocket"), missingThing=Ignore)
#'
#' # Consolidate duplicated warnings from code that doesn't use reportr
#' withReportrHandlers(sqrt(-5:-1))
#'
#' @seealso \code{\link{signal}} for signalling these conditions, and
#'   \code{\link{reportrCondition}} for their structure.
#' @author Jon Clayden
#' @name handlers
NULL

.handlerDepth <- function ()
{
    depth <- .Workspace$handlerDepth
    if (is.null(depth)) 0L else depth
}

# The classes of a condition which came from its signaller, rather than from
# its level or from R itself. These are carried over when a condition is
# re-signalled at a different level, so that outer handlers still match it
.userClasses <- function (condition)
{
    setdiff(class(condition), c(paste("reportr",names(OL),sep=""), "reportrCondition", "error", "warning", "message", "condition"))
}

.resolveLevel <- function (value)
{
    if (is.character(value) && length(value) == 1L && value %in% names(OL))
        return (OL[[value]])
    else
        return (value)
}

# Build a calling handler which reports a condition at a different level from
# the one it was signalled at
.remapHandler <- function (newLevel)
{
    force(newLevel)

    function (condition)
    {
        oldLevel <- condition$level
        demoting <- (oldLevel >= OL$Error && newLevel < OL$Error)

        if (demoting && !isTRUE(condition$recoverable))
        {
            # The signalling code specified no value to continue with, so it is
            # not safe to resume it. Explain, and return normally so that the
            # condition stands as it was signalled: any handler further out
            # still gets its chance, and the error remains catchable
            .report(OL$Warning, paste("A fatal \"", class(condition)[1], "\" condition cannot be demoted, as it specifies no value to continue with", sep=""))
            return (invisible(NULL))
        }

        # Reporting at level Ignore means reporting nothing at all
        if (newLevel > OL$Ignore)
            .signal(newLevel, conditionMessage(condition), class=.userClasses(condition), call=conditionCall(condition), data=list(original=condition))

        if (demoting || oldLevel < OL$Error)
            invokeRestart("useValue", condition$default)
        else
            invokeRestart("muffleReport")
    }
}

.buildHandlers <- function (expressions, envir)
{
    if (length(expressions) == 0)
        return (list())
    if (is.null(names(expressions)) || any(names(expressions) == ""))
        report(OL$Error, "Conditions to be handled must all be named")

    lapply(expressions, function (expression) {
        if (is.symbol(expression))
        {
            name <- as.character(expression)
            if (name %in% names(OL))
                return (.remapHandler(OL[[name]]))
        }
        value <- eval(expression, envir)
        if (is.function(value))
            return (value)
        else
            return (.remapHandler(.resolveLevel(value)))
    })
}

#' @rdname handlers
#' @export
reportAs <- function (expr, ...)
{
    handlers <- .buildHandlers(as.list(substitute(list(...)))[-1], parent.frame())

    .Workspace$handlerDepth <- .handlerDepth() + 1L
    on.exit(.Workspace$handlerDepth <- .handlerDepth() - 1L)

    # The call is built rather than being written out, so that the handlers can
    # be determined at run time. Evaluating "expr" here forces the promise, and
    # so evaluates the user's expression in their own frame
    eval(as.call(c(list(quote(withCallingHandlers), quote(expr)), handlers)))
}

.messageHandler <- function (m)
{
    .signal(OL$Info, ore.subst("\n$","",m$message), class=.foreignClasses(m), call=m$call, data=list(original=m))
    invokeRestart("muffleMessage")
}

.warningHandler <- function (w)
{
    .signal(OL$Warning, w$message, class=.foreignClasses(w), call=w$call, defer=TRUE, data=list(original=w))
    invokeRestart("muffleWarning")
}

.errorHandler <- function (e)
{
    if (is.null(e$call))
        message <- e$message
    else
        message <- paste(e$message, " (in \"", as.character(e$call)[1], "(", .truncate(paste(as.character(e$call)[-1],collapse=", "),100), ")\")", sep="")
    .signal(OL$Error, message, class=.foreignClasses(e), call=e$call, data=list(original=e))
}

#' @rdname handlers
#' @export
withReportrHandlers <- function (expr, ...)
{
    handlers <- c(.buildHandlers(as.list(substitute(list(...)))[-1], parent.frame()),
                  list(message=.messageHandler, warning=.warningHandler, error=.errorHandler))

    .Workspace$handlerDepth <- .handlerDepth() + 1L
    on.exit(.Workspace$handlerDepth <- .handlerDepth() - 1L)

    result <- eval(as.call(c(list(quote(withCallingHandlers), quote(expr)), handlers)))

    reportFlags()
    return (result)
}

#' @rdname handlers
#' @export
reportrHandlers <- function (install = TRUE)
{
    ours <- list(.messageHandler, .warningHandler, .errorHandler)

    if (isTRUE(install))
        globalCallingHandlers(message=.messageHandler, warning=.warningHandler, error=.errorHandler)
    else
    {
        current <- globalCallingHandlers()
        keep <- !sapply(current, function (handler) any(sapply(ours, identical, handler)))
        globalCallingHandlers(NULL)
        if (any(keep))
            do.call(globalCallingHandlers, current[keep])
    }

    invisible(NULL)
}
