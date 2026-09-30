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
#' to the specified output level for the message). An error raised by
#' \code{assert} cannot be demoted by the caller, because the function has
#' nothing sensible to continue with; see \code{\link{fallback}} for errors
#' which the caller may choose to demote.
#' 
#' Every message is signalled as an R condition before it is reported, so that
#' calling code can intercept it. The condition's class vector always includes
#' one derived from the level of the message, such as \code{"reportrInfo"}, so
#' \code{tryCatch(expr, reportrInfo=...)} will match. See
#' \code{\link{reportrCondition}} for the structure of these objects.
#' 
#' The \code{class} argument to \code{report}, \code{flag} and \code{assert}
#' attaches classes of your own to the condition. This is worth doing whenever
#' a caller might reasonably want to act on a particular kind of message:
#' \code{\link{reportAs}} can then escalate, demote or suppress it, without the
#' function which detects the problem having to be told in advance how serious
#' it is. For example, a function which returns a partial result might report
#' the omissions at level \code{Warning} with a class, and leave any caller who
#' needs a complete result to escalate that class to \code{Error}. Unlike
#' unclassed messages, classed ones are signalled even when the current output
#' level means that they will not be reported, since the class is there for the
#' benefit of callers. A function which cannot do what was asked of it, but has
#' a value to return instead, should use \code{\link{fallback}}.
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
#' Messages are written to one or more output targets, which may include files
#' as well as the terminal, and each of which formats messages with its own
#' prefix. The default prefix is \code{"\%d\%L: "}, which gives a prefix such
#' as \code{"* * INFO: "}, with one star for each level of the call stack at
#' which the message arose. See \code{\link{targets}} for how to change it.
#' Setting the \code{prefix} argument to \code{report}, \code{assert} or
#' \code{ask} to \code{FALSE} writes that message without any prefix, to any
#' target, which is useful for headings and other formatted output.
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
#' @param class A character vector of classes to attach to the condition, or
#'   \code{NULL}. A class derived from the level of the message, such as
#'   \code{"reportrWarning"}, is always attached in addition to these.
#' @param call The call to associate with the condition. Defaults to the call
#'   of the function which is reporting.
#' @param prefix Logical value: should the message be prefixed, according to
#'   the format for each output target? See Details.
#' @param prefixFormat Deprecated. A value of \code{""} is equivalent to
#'   \code{prefix=FALSE}, and any other value is ignored with a warning.
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
#' @seealso \code{\link{fallback}} for errors which callers may demote;
#'   \code{\link{handlers}} for acting on classed messages;
#'   \code{\link{reportrCondition}} for the condition objects themselves; and
#'   \code{\link{targets}} for controlling where output goes, and how it is
#'   formatted. \code{\link[ore]{es}} (in package \code{ore}) performs the
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
# the cheapest path through this function. The expression may be given
# explicitly, for arguments not called "level"
.evaluateLevel <- function (level, expression = substitute(level, parent.frame()))
{
    if (is.symbol(expression))
    {
        name <- as.character(expression)
        if (name %in% names(OL))
            return (OL[[name]])
    }

    if (is.character(level) && length(level) == 1L && level %in% names(OL))
        return (OL[[level]])
    else
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
        .report(OL$Info, "Output level is not set; defaulting to \"Info\"", plain=TRUE, outputLevel=OL$Info)
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

# Interpret the deprecated "prefixFormat" argument. The empty string, which
# was its main use, means no prefix; anything else is ignored, since prefixes
# now belong to output targets
.resolvePrefix <- function (prefix, prefixFormat)
{
    if (is.null(prefixFormat))
        return (isTRUE(prefix))
    else if (identical(as.character(prefixFormat), ""))
        return (FALSE)

    if (!isTRUE(.Workspace$warnedPrefixFormat))
    {
        .Workspace$warnedPrefixFormat <- TRUE
        .report(OL$Warning, "The \"prefixFormat\" argument is deprecated and will be ignored; set the prefix of an output target instead")
    }
    return (isTRUE(prefix))
}

# Simple wrappers, to facilitate mocking in the tests
.interactive <- function() base::interactive()      # nocov
.readline <- function(...) base::readline(...)      # nocov

#' @rdname reportr
#' @export
ask <- function (..., default = NULL, valid = NULL, prefix = TRUE, prefixFormat = NULL)
{
    prefix <- .resolvePrefix(prefix, prefixFormat)
    outputLevel <- .outputLevel()
    message <- .buildMessage(..., .envir=parent.frame())
    if (!.interactive() || outputLevel > OL$Question || !.shouldReport(message))
        return (default)
    else
    {
        reportFlags()
        repeat
        {
            ans <- .readline(paste(if (prefix) .buildPrefix(OL$Question,.terminalPrefix()) else "", message, " ", sep=""))
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
report <- function (level, ..., class = NULL, call = sys.call(-1), prefix = TRUE, prefixFormat = NULL)
{
    level <- .evaluateLevel(level)
    outputLevel <- .outputLevel()

    # The fast path: nothing will be reported, so unless the message is fatal
    # or something is listening for it, there is no need to build it at all. A
    # classed condition is always signalled, since the class is there for the
    # benefit of callers
    if (outputLevel > level && level < OL$Error && is.null(class) && !.handlersActive())
        return (invisible(NULL))

    message <- .buildMessage(..., .envir=parent.frame())

    .signal(level, message, class=class, call=call, plain=!.resolvePrefix(prefix,prefixFormat), outputLevel=outputLevel)
}

# Report a message which is already in its final form, so no expression
# substitution is performed, and no condition is signalled. Everything which
# ends up being rendered passes through here
.report <- function (level, message, plain = FALSE, outputLevel = .outputLevel(), condition = NULL)
{
    if (is.null(message) || outputLevel > level || !.shouldReport(message))
        return (invisible(NULL))

    reportFlags()

    .render(level, message, outputLevel, condition, plain)

    invisible(NULL)
}

#' @rdname reportr
#' @export
flag <- function (level, ..., class = NULL, call = sys.call(-1))
{
    level <- .evaluateLevel(level)
    outputLevel <- .outputLevel()

    message <- .buildMessage(..., .envir=parent.frame())

    # The condition is signalled here, where the message arises, even though
    # the message itself is not reported until later
    .signal(level, message, class=class, call=call, outputLevel=outputLevel, defer=TRUE)
}

# Store a message which is already in its final form, for later reporting
.bufferFlag <- function (level, message, outputLevel = .outputLevel(), condition = NULL)
{
    # In debug mode, sufficiently important messages are reported immediately,
    # so that they appear at the point where they arise
    if (outputLevel == OL$Debug && level >= .resolveOption("reportrStackTraceLevel"))
        return (.report(level, message, outputLevel=outputLevel, condition=condition))

    # The condition is kept so that the message can be rendered as it would
    # have been where it arose
    currentFlag <- list(list(level=level, message=message, condition=condition))

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
        flags <- .Workspace$reportrFlags
        levels <- unlist(lapply(flags, "[[", "level"))
        messages <- unlist(lapply(flags, "[[", "message"))
        
        # This is before the call to report() to avoid infinite recursion
        clearFlags()
        
        for (message in unique(messages))
        {
            locs <- which(messages == message)
            level <- max(levels[locs])
            # These messages were signalled when they were flagged, so they are
            # reported directly rather than being signalled a second time.
            # Repeated messages are rendered as the most severe instance was
            condition <- flags[[locs[which.max(levels[locs])]]]$condition
            if (length(locs) > 1)
                message <- paste("[x",length(locs),"] ",message,sep="")
            .report(level, message, condition=condition)
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
assert <- function (expr, ..., class = NULL, level = OL$Error, call = sys.call(-1), prefix = TRUE, prefixFormat = NULL, envir = parent.frame())
{
    result <- try(as.logical(eval(substitute(expr), envir)), silent=TRUE)
    if (!isTRUE(result))
    {
        level <- .evaluateLevel(level)
        message <- .buildMessage(..., .envir=envir)
        .signal(level, message, class=class, call=call, plain=!.resolvePrefix(prefix,prefixFormat))
    }
    invisible(NULL)
}
