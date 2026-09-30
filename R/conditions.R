#' reportr conditions
#'
#' Every message reported by \code{reportr} is first signalled as an R
#' condition, which allows calling code to intercept it, inspect it, and decide
#' how it should be treated. \code{reportrCondition} constructs such a
#' condition object.
#'
#' The class vector of the condition is assembled from most to least specific:
#' any classes given in the \code{class} argument; a class derived from the
#' level of the message, such as \code{"reportrWarning"}; the general class
#' \code{"reportrCondition"}; and finally \code{"condition"}. The level-derived
#' class is always present, so a condition can always be matched by a handler
#' even when no explicit class is given.
#'
#' Conditions at level \code{Error} or above additionally inherit from
#' \code{"error"}, so they are caught by \code{\link{try}} and by
#' \code{tryCatch(expr, error=...)} in the usual way. If the
#' \code{reportrBaseClasses} option is \code{TRUE}, conditions at level
#' \code{Warning} also inherit from \code{"warning"}, and those at level
#' \code{Info} and below from \code{"message"}, which makes them susceptible to
#' \code{\link{suppressWarnings}} and \code{\link{suppressMessages}}. This is
#' not the default, since it allows unrelated code to silence reportr output.
#'
#' In addition to the standard \code{message} and \code{call} elements, the
#' condition carries the \code{level} of the message, the \code{stack} of calls
#' in effect when it was signalled, and a \code{recoverable} flag. A condition
#' signalled by \code{\link{fallback}} is recoverable, which means that the
#' signalling function has a value to return instead of doing what was asked of
#' it, and that value is stored in the \code{default} element. For any other
#' condition \code{default} is \code{NULL}.
#'
#' Every condition establishes a \code{muffleReport} restart while it is being
#' signalled. Invoking it suppresses the reporting of the message, but does not
#' otherwise affect the flow of control, so an error will still be fatal.
#' Recoverable conditions additionally establish a \code{useValue} restart.
#' Invoking it with a single argument suppresses reporting \emph{and} any
#' error, and provides the value which the signalling function will return in
#' place of its default.
#'
#' @param level The level of the message. See \code{\link{report}}.
#' @param message A character string giving the message, in its final form.
#' @param class A character vector of additional classes for the condition, or
#'   \code{NULL}.
#' @param call The call in which the condition arose, or \code{NULL}.
#' @param data A list of further elements to store in the condition object.
#'
#' @return A condition object.
#'
#' @examples
#' cond <- reportrCondition(OL$Warning, "Something is amiss", class="myWarning")
#' class(cond)
#' conditionMessage(cond)
#'
#' @seealso \code{\link{report}} and \code{\link{fallback}} for signalling
#'   conditions, \code{\link{reportAs}} for handling them, and
#'   \code{\link{conditions}} for R's condition system in general.
#' @author Jon Clayden
#' @export
reportrCondition <- function (level, message, class = NULL, call = NULL, data = list())
{
    level <- .evaluateLevel(level)
    levelClass <- paste("reportr", names(OL)[which(OL == level)], sep="")

    classes <- c(as.character(class), levelClass, "reportrCondition")
    if (level >= OL$Error)
        classes <- c(classes, "error")
    else if (isTRUE(.resolveOption("reportrBaseClasses")))
    {
        if (level == OL$Warning)
            classes <- c(classes, "warning")
        else if (level <= OL$Info)
            classes <- c(classes, "message")
    }

    structure(c(list(message=as.character(message)[1], call=call, level=level), data),
              class=c(classes,"condition"))
}

# The informative classes of a condition raised outside reportr. These are
# prepended to the reportr condition's own classes, so that a handler further
# up the stack can still match the original condition by class
.foreignClasses <- function (condition)
{
    setdiff(class(condition), c("simpleError","simpleWarning","simpleMessage","simpleCondition","error","warning","message","condition"))
}

# TRUE if any reportr-managed handlers are currently established. Used to
# decide whether a message which will not be reported is nevertheless worth
# signalling, since a condition that nothing can observe is not worth building
.handlersActive <- function ()
{
    depth <- .Workspace$handlerDepth
    !is.null(depth) && depth > 0L
}

# The core of the package: signal a condition for a message which is already in
# its final form, report it if nothing intervenes, and abort if it is an error.
# A recoverable condition carries a value for the signalling function to
# return, which a handler may replace, and only such a condition can have an
# error demoted. Returns the value that the signalling function should return
.signal <- function (level, message, class = NULL, call = NULL, plain = FALSE,
                     value = NULL, recoverable = FALSE, outputLevel = .outputLevel(),
                     defer = FALSE, data = list())
{
    fatal <- (level >= OL$Error)

    # A message filtered out entirely still can't stop an error being fatal
    if (is.null(message))
    {
        if (fatal)
            invokeRestart("abort")
        return (invisible(NULL))
    }

    if (!recoverable)
        value <- NULL
    condition <- reportrCondition(level, message, class, call,
                                  c(data, list(stack=sys.calls(), default=value, recoverable=recoverable, plain=plain)))

    body <- function () {
        signalCondition(condition)
        if (defer)
            .bufferFlag(level, message, outputLevel, condition)
        else
            .report(level, message, plain, outputLevel, condition)
        value
    }

    # The useValue restart is only offered for recoverable conditions, since
    # code which did not say what to continue with is not safe to resume
    recovered <- FALSE
    if (recoverable)
    {
        result <- withRestarts(body(),
            muffleReport=function () value,
            useValue=function (newValue) {
                recovered <<- TRUE
                newValue
            })
    }
    else
        result <- withRestarts(body(), muffleReport=function () value)

    if (!recovered && fatal)
        invokeRestart("abort")

    invisible(result)
}
