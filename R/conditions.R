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
#' in effect when it was signalled, the \code{default} value which the
#' signalling function will return if the condition goes unhandled, and a
#' \code{recoverable} flag indicating whether that default was explicitly
#' supplied. See \code{\link{signal}} for the significance of the last of
#' these.
#'
#' Two restarts are established while the condition is being signalled.
#' Invoking \code{muffleReport} suppresses the reporting of the message, but
#' does not otherwise affect the flow of control, so an error will still be
#' fatal. Invoking \code{useValue} with a single argument suppresses reporting
#' \emph{and} the error, and provides the value which the signalling function
#' will return.
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
#' @seealso \code{\link{signal}} for signalling conditions,
#'   \code{\link{reportAs}} for handling them, and
#'   \code{\link{conditions}} for R's condition system in general.
#' @author Jon Clayden
#' @export
reportrCondition <- function (level, message, class = NULL, call = NULL, data = list())
{
    level <- .evaluateLevel(level)

    classes <- c(as.character(class), .levelClass(level), "reportrCondition")
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

.levelClass <- function (level)
{
    paste("reportr", names(OL)[which(OL == level)], sep="")
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
# Returns the value that the signalling function should return
.signal <- function (level, message, class = NULL, call = NULL, prefixFormat = NULL,
                     default = .noDefault, outputLevel = .outputLevel(), defer = FALSE,
                     data = list())
{
    fatal <- (level >= OL$Error)

    # A message filtered out entirely still can't stop an error being fatal
    if (is.null(message))
    {
        if (fatal)
            invokeRestart("abort")
        return (invisible(NULL))
    }

    recoverable <- !identical(default, .noDefault)
    value <- if (recoverable) default else NULL

    condition <- reportrCondition(level, message, class, call,
                                  c(data, list(stack=sys.calls(), default=value, recoverable=recoverable)))

    recovered <- FALSE
    result <- withRestarts({
                               signalCondition(condition)
                               if (defer)
                                   .bufferFlag(level, message, outputLevel)
                               else
                                   .report(level, message, prefixFormat, outputLevel, condition)
                               value
                           },
                           muffleReport = function () value,
                           useValue = function (newValue) {
                               recovered <<- TRUE
                               newValue
                           })

    if (!recovered && fatal)
        invokeRestart("abort")

    invisible(result)
}
