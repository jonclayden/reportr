#' The reportr message reporting system
#' 
#' Functions for reporting informative messages, warnings and errors. These are
#' provided as alternatives to the \code{\link{message}}, \code{\link{warning}}
#' and \code{\link{stop}} functions in base R.
#' 
#' The \code{reportr} system for reporting messages provides certain useful
#' features over the standard R system, such as the incorporation of output
#' consolidation, message filtering, expression substitution, automatic
#' generation of stack traces for debugging, and conditional reporting based on
#' the current ``output level''. Messages of level at least equal to the value
#' of option \code{reportrStderrLevel} are written to standard error
#' (\code{\link{stderr}}); others are written to standard output
#' (\code{\link{stdout}}).
#' 
#' The output level is set by the \code{setOutputLevel} function, and governs
#' whether a particular call to \code{report} will actually report anything.
#' Output levels are described by the \code{OL} object, a list with components
#' \code{Ignore}, \code{Debug}, \code{Verbose}, \code{Info}, \code{Warning},
#' \code{Question}, \code{Error} and \code{Fatal}. Any call to \code{report}
#' using a level lower than the current output level will produce no output.
#' If \code{report} is called before \code{setOutputLevel}, the output level
#' will default to \code{Info} (with a message).
#' 
#' The \code{flag} function is called like \code{report}, but it stores
#' messages for later reporting, like \code{\link{warning}}, rather than
#' reporting them immediately. Stored messages are reported when \code{report}
#' is next called, at which point multiple instances of the same message are
#' consolidated where possible. The user may also manually force stored
#' messages to be reported by calling \code{reportFlags}, or remove them with
#' \code{clearFlags}. Note that the output level at the time that
#' \code{reportFlags} is called (implicitly or explicitly) will determine
#' whether the flags are printed.
#' 
#' The \code{ask} function requests input from the user, using
#' \code{\link{readline}}, at output level \code{Question}. The text argument
#' forms the text of the question, and \code{ask} returns the text entered by
#' the user.
#' 
#' The \code{assert} function asserts that its first argument evaluates to
#' \code{TRUE}, and prints an error message if not (or warning, etc., according
#' to the specified output level for the message).
#' 
#' Every message is signalled as an R condition before it is reported, so that
#' calling code can intercept it. The condition's class vector always includes
#' one derived from the level of the message, such as \code{"reportrInfo"}, so
#' \code{tryCatch(expr, reportrInfo=...)} will match. See
#' \code{\link{reportrCondition}} for the structure of these objects, and
#' \code{\link{signal}} for attaching more meaningful classes of your own.
#' 
#' The call \code{report(Error,\dots)} is similar to \code{stop(\dots)}, except
#' that the message is formatted by reportr rather than by R, and a stack trace
#' will be printed if the current output level is \code{Debug} and the level of
#' the message is at least \code{reportrStackTraceLevel}. Since the condition
#' is signalled before the message is reported, such errors can be caught with
#' \code{\link{try}} or \code{tryCatch(expr, error=...)} in the usual way. If
#' nothing handles the condition the "abort" restart is invoked, which ends
#' execution without R adding a second message of its own. An error is fatal
#' whatever the current output level, and whether or not its message is
#' filtered out of the output.
#' 
#' The \code{withReportrHandlers} function evaluates \code{expr} in a context
#' in which R errors, warnings and messages will be handled by reportr, rather
#' than by the standard R functions. See \code{\link{handlers}}.
#' 
#' The \code{prefixFormat} argument to \code{report} and \code{ask} controls
#' how the output message is formatted. It takes the form of a
#' \code{\link{sprintf}}-style format string, but with different expansions for
#' percent-escapes. Specifically, \code{"\%d"} expands to a series of stars
#' indicating the current stack depth; \code{"\%f"} gives the name of the
#' function calling \code{report} or \code{ask}; \code{"\%l"} and \code{"\%L"}
#' give lower and upper case versions of the level of the message,
#' respectively; \code{"\%p"} expands to the ID of the current R process (see
#' \code{\link{Sys.getpid}}); and \code{"\%t"} expands to the current time,
#' formatted according to the \code{reportrTimeFormat} option. The default is
#' \code{"\%d\%L: "}, giving a prefix such as \code{"* * INFO: "}, but this
#' default can be overridden by setting the \code{reportrPrefixFormat} option.
#' 
#' Messages are written to one or more destinations, which may include files as
#' well as the terminal. See \code{\link{destinations}}.
#' 
#' A number of other options influence the output produced by reportr.
#' \code{getOutputLevel} and \code{setOutputLevel} get and set the
#' \code{reportrOutputLevel} option, which can be set directly if preferred.
#' The options \code{reportrMessageFilterIn} and \code{reportrMessageFilterOut}
#' can contain a single character string representing a Perl regular
#' expression, in which case only messages which match
#' (\code{reportrMessageFilterIn}) or do not match
#' (\code{reportrMessageFilterOut}) the regular expression will be reported.
#' Filtering affects only what is reported: the corresponding condition is
#' signalled either way. The \code{reportrBaseClasses} option controls whether
#' reportr conditions also inherit from R's own \code{"message"} and
#' \code{"warning"} classes; see \code{\link{reportrCondition}}.
#' 
#' @param level The level of output message to produce, or for
#'   \code{setOutputLevel}, the minimum level to display. See Details.
#' @param \dots Objects which can be coerced to mode \code{character}. These
#'   will be passed through function \code{\link[ore]{es}} (from the \code{ore}
#'   package) for expression substitution, and then printed with no space
#'   between them. Options to \code{\link[ore]{es}}, such as \code{round}, may
#'   also be given.
#' @param prefixFormat The format of the string prepended to the message. See
#'   Details.
#' @param default A default return value, to be used when the message is
#'   filtered out or the output level is above \code{Question}.
#' @param valid For \code{ask}, a character vector of valid responses. If
#'   necessary, the question will be asked repeatedly until the user gives a
#'   suitable response. (Matching is not case-sensitive.)
#' @param expr An expression to be evaluated.
#' @param envir For \code{assert}, the environment in which to evaluate the
#'   specified expression.
#' 
#' @return These functions are mainly called for their side effects, but
#'   \code{getOutputLevel} returns the current output level,
#'   \code{withReportrHandlers} returns the value of the evaluated expression,
#'   and \code{ask} returns a character vector of length one giving the user's
#'   response.
#' 
#' @examples
#' setOutputLevel(OL$Warning)
#' report(Info, "Test message")  # no output
#' setOutputLevel(OL$Info)
#' report(Info, "Test message")  # prints the message
#' 
#' flag(Warning, "Test warning")  # no output
#' flag(Warning, "Test warning")  # repeated warning
#' reportFlags()  # consolidates the warnings and prints the message
#' 
#' \dontrun{name <- ask("What is your name?")
#' report(OL$Info, "Hello, #{name}")}
#' 
#' @seealso \code{\link{signal}} for attaching classes to messages, so that
#'   callers can decide how serious they are; \code{\link{handlers}} for acting
#'   on them; \code{\link{reportrCondition}} for the condition objects
#'   themselves; and \code{\link{destinations}} for controlling where output
#'   goes. \code{\link[ore]{es}} (in package \code{ore}) performs the
#'   expression substitution applied to messages. \code{\link{message}},
#'   \code{\link{warning}}, \code{\link{stop}} and \code{\link{condition}} for
#'   the normal R message and condition-signalling framework.
#' @author Jon Clayden
#' 
#' @name reportr
#' @aliases OL
NULL

.resolveOption <- function (name)
{
    value <- getOption(name)
    if (is.null(value))
        value <- .Defaults[[name]]
    return (value)
}

# Resolve a level given as OL$Info, as the bare name Info, or as the string
# "Info". The common case, OL$Info, is a call rather than a name, so it takes
# the cheapest path through this function
.evaluateLevel <- function (level)
{
    expression <- substitute(level, parent.frame())
    if (is.symbol(expression))
    {
        name <- as.character(expression)
        if (name %in% names(OL))
            return (OL[[name]])
    }
    else if (is.character(level) && length(level) == 1L && level %in% names(OL))
        return (OL[[level]])

    return (level)
}

#' @rdname reportr
#' @export
setOutputLevel <- function (level)
{
    level <- .evaluateLevel(level)
    if (level %in% OL$Debug:OL$Fatal)
        options(reportrOutputLevel=level)
    invisible(NULL)
}

#' @rdname reportr
#' @export
getOutputLevel <- function ()
{
    level <- .outputLevel()
    names(level) <- names(which(OL == level))
    return (level)
}

# The output level as a bare integer. This is on the fast path taken by every
# call to report(), including the many which produce no output, so it avoids
# the cost of naming the result
.outputLevel <- function ()
{
    level <- getOption("reportrOutputLevel")
    if (is.null(level))
    {
        setOutputLevel(OL$Info)
        .report(OL$Info, "Output level is not set; defaulting to \"Info\"", prefixFormat="", outputLevel=OL$Info)
        level <- OL$Info
    }
    return (level)
}

# Apply the message filters to a string which is already in its final form.
# Filtering suppresses the reporting of a message, but not the signalling of
# the corresponding condition: a filtered error is still an error
.shouldReport <- function (message)
{
    keep <- TRUE

    filterIn <- .resolveOption("reportrMessageFilterIn")
    filterOut <- .resolveOption("reportrMessageFilterOut")
    if (!is.null(filterIn))
        keep <- keep & (message %~% as.character(filterIn)[1])
    if (!is.null(filterOut))
        keep <- keep & (!(message %~% as.character(filterOut)[1]))

    return (isTRUE(keep))
}

# The evaluation environment is passed explicitly, rather than being inferred
# from the call depth, so that intermediate layers don't break substitution
.buildMessage <- function (..., .envir, round = NULL, signif = NULL)
{
    es(paste(..., sep=""), round=round, signif=signif, envir=.envir)
}

# Simple wrappers, to facilitate mocking in the tests
.interactive <- function() base::interactive()      # nocov
.readline <- function(...) base::readline(...)      # nocov

#' @rdname reportr
#' @export
ask <- function (..., default = NULL, valid = NULL, prefixFormat = NULL)
{
    outputLevel <- .outputLevel()
    message <- .buildMessage(..., .envir=parent.frame())
    if (!.interactive() || outputLevel > OL$Question || !.shouldReport(message))
        return (default)
    else
    {
        reportFlags()
        repeat
        {
            ans <- .readline(paste(.buildPrefix(OL$Question,prefixFormat), message, " ", sep=""))
            if (is.null(valid))
                return (ans)
            else
            {
                match <- (tolower(ans) == tolower(valid))
                if (any(match))
                    return (valid[which(match)[1]])
            }
        }
    }
}

#' @rdname reportr
#' @export
report <- function (level, ..., prefixFormat = NULL)
{
    level <- .evaluateLevel(level)
    outputLevel <- .outputLevel()

    # The fast path: nothing will be reported, so unless the message is fatal
    # or something is listening for it, there is no need to build it at all
    if (outputLevel > level && level < OL$Error && !.handlersActive())
        return (invisible(NULL))

    message <- .buildMessage(..., .envir=parent.frame())

    .signal(level, message, prefixFormat=prefixFormat, outputLevel=outputLevel)
}

# Report a message which is already in its final form, so no expression
# substitution is performed, and no condition is signalled. Everything which
# ends up being rendered passes through here
.report <- function (level, message, prefixFormat = NULL, outputLevel = .outputLevel(), condition = NULL)
{
    if (is.null(message) || outputLevel > level || !.shouldReport(message))
        return (invisible(NULL))

    reportFlags()

    .render(level, message, prefixFormat, outputLevel, condition)

    invisible(NULL)
}

#' @rdname reportr
#' @export
flag <- function (level, ...)
{
    level <- .evaluateLevel(level)
    outputLevel <- .outputLevel()

    message <- .buildMessage(..., .envir=parent.frame())

    # The condition is signalled here, where the message arises, even though
    # the message itself is not reported until later
    .signal(level, message, outputLevel=outputLevel, defer=TRUE)
}

# Store a message which is already in its final form, for later reporting
.bufferFlag <- function (level, message, outputLevel = .outputLevel())
{
    # In debug mode, sufficiently important messages are reported immediately,
    # so that they appear at the point where they arise
    if (outputLevel == OL$Debug && level >= .resolveOption("reportrStackTraceLevel"))
        return (.report(level, message, outputLevel=outputLevel))

    currentFlag <- list(list(level=level, message=message))

    if (!exists("reportrFlags",.Workspace) || is.null(.Workspace$reportrFlags))
        .Workspace$reportrFlags <- currentFlag
    else
        .Workspace$reportrFlags <- c(.Workspace$reportrFlags, currentFlag)

    invisible(NULL)
}

#' @rdname reportr
#' @export
reportFlags <- function ()
{
    if (exists("reportrFlags",.Workspace) && !is.null(.Workspace$reportrFlags))
    {
        levels <- unlist(lapply(.Workspace$reportrFlags, "[[", "level"))
        messages <- unlist(lapply(.Workspace$reportrFlags, "[[", "message"))
        
        # This is before the call to report() to avoid infinite recursion
        clearFlags()
        
        for (message in unique(messages))
        {
            locs <- which(messages == message)
            level <- max(levels[locs])
            # These messages were signalled when they were flagged, so they are
            # reported directly rather than being signalled a second time
            if (length(locs) == 1)
                .report(level, message, prefixFormat="%L: ")
            else
                .report(level, paste("[x",length(locs),"] ",message,sep=""), prefixFormat="%L: ")
        }
    }
}

#' @rdname reportr
#' @export
clearFlags <- function ()
{
    .Workspace$reportrFlags <- NULL
}

#' @rdname reportr
#' @export
assert <- function (expr, ..., level = OL$Error, prefixFormat = NULL, envir = parent.frame())
{
    result <- try(as.logical(eval(substitute(expr), envir)), silent=TRUE)
    if (!isTRUE(result))
    {
        level <- .evaluateLevel(level)
        message <- .buildMessage(..., .envir=envir)
        .signal(level, message, prefixFormat=prefixFormat)
    }
    invisible(NULL)
}
