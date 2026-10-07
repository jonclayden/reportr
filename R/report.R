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

#' @rdname report
#' @order 6
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

#' Report messages, warnings and errors
#'
#' These functions report informative messages, warnings and errors, as
#' alternatives to [message()], [warning()] and [stop()]. `report()` reports a
#' message immediately, while `flag()` stores it to be reported later. `assert()`
#' reports a message only if an expression is not `TRUE`, and `ask()` asks the
#' user a question.
#'
#' Each message has a level, such as `Info` or `Warning`, which may be given as
#' `OL$Info`, as the bare name `Info`, or as the string `"Info"`; see [reportr]
#' for the full list. A message is reported only if its level is at least the
#' current output level (see [setOutputLevel()]), and it is then written to
#' each of the current output targets which accepts it (see [targets]).
#'
#' The parts of the message given in `...` are pasted together with no
#' separator, after expression substitution by [ore::es()], so `"x is #{x}"`
#' includes the value of `x` in the message. Messages are written with a
#' prefix, such as `"* * INFO: "`, whose format is set by each output target.
#' Setting `prefix = FALSE` writes a message without any prefix, to every
#' target, which is useful for headings and other formatted output.
#'
#' @section Conditions:
#' Every message is signalled as an R condition before it is reported, so that
#' calling code can intercept it. Its class vector always includes one derived
#' from the level of the message, such as `"reportrInfo"`, so
#' `tryCatch(expr, reportrInfo=...)` will match. See [reportrCondition()] for
#' the structure of these objects.
#'
#' The `class` argument attaches classes of your own to the condition. This is
#' worth doing whenever a caller might reasonably want to act on a particular
#' kind of message: [reportAs()] can then escalate, demote or suppress it,
#' without the function which detects the problem having to be told in advance
#' how serious it is. For example, a function which returns a partial result
#' might report the omissions at level `Warning` with a class, and leave any
#' caller who needs a complete result to escalate that class to `Error`. Unlike
#' unclassed messages, classed ones are signalled even when the current output
#' level means that they will not be reported, since the class is there for the
#' benefit of callers.
#'
#' @section Errors:
#' A message at level `Error` or above stops execution, like [stop()], but the
#' message is formatted by reportr rather than by R, and a stack trace is
#' written if the current output level is `Debug` and the level of the message
#' is at least the `reportrStackTraceLevel` option. Since the condition is
#' signalled before the message is reported, such errors can be caught with
#' [try()] or `tryCatch(expr, error=...)` in the usual way. If nothing handles
#' the condition the `"abort"` restart is invoked, which ends execution without
#' R adding a second message of its own. An error is fatal whatever the current
#' output level, and whether or not its message is filtered out of the output.
#'
#' An error raised by `report()` or `assert()` cannot be demoted by the caller,
#' because the function which raised it has nothing sensible to continue with.
#' A function which cannot do what was asked of it, but has a value to return
#' instead, should use [fallback()].
#'
#' @section Flags:
#' `flag()` is called like `report()`, but stores the message for later
#' reporting, rather than reporting it immediately. Its condition is signalled
#' straight away, though. Stored messages are reported when a message is next
#' reported, at which point multiple instances of the same message are
#' consolidated, and each is written as it would have been where it arose.
#' `reportFlags()` reports stored messages immediately, and `clearFlags()`
#' discards them. The output level at the time the stored messages are
#' reported determines whether they are written. At output level `Debug`,
#' flagged messages at or above the `reportrStackTraceLevel` option are
#' reported immediately, so that they appear where they arise.
#'
#' @section Questions:
#' `ask()` requests input from the user, using [readline()], at level
#' `Question`. It returns the text entered by the user, or `default` if the
#' session is not interactive, the output level is above `Question`, or the
#' question is filtered out.
#'
#' @param level The level of the message. See Details.
#' @param ... Objects which can be coerced to mode `character`. These are pasted
#'   together and passed through [ore::es()] for expression substitution.
#'   Arguments to [ore::es()], such as `round`, may also be given.
#' @param class A character vector of classes to attach to the condition, or
#'   `NULL`. A class derived from the level of the message, such as
#'   `"reportrWarning"`, is always attached in addition to these.
#' @param call The call to associate with the condition. Defaults to the call
#'   of the function which is reporting.
#' @param prefix Logical value: should the message be prefixed, according to
#'   the format for each output target?
#' @param prefixFormat Deprecated. A value of `""` is equivalent to
#'   `prefix = FALSE`, and any other value is ignored with a warning.
#' @param default For `ask()`, the value to return if the question is not
#'   asked.
#' @param valid For `ask()`, a character vector of valid responses. If
#'   necessary, the question will be asked repeatedly until the user gives a
#'   suitable response. Matching is not case-sensitive.
#' @param expr For `assert()`, an expression which should evaluate to `TRUE`.
#'   An expression which cannot be evaluated counts as a failure.
#' @param envir For `assert()`, the environment in which to evaluate `expr` and
#'   to substitute expressions into the message.
#'
#' @return `ask()` returns the user's response as a character string, or the
#'   value of `default`. The other functions are called for their side
#'   effects, and return `NULL`, invisibly.
#'
#' @examples
#' setOutputLevel(OL$Warning)
#' report(Info, "Test message")   # no output
#' setOutputLevel(OL$Info)
#' report(Info, "Test message")   # prints the message
#'
#' flag(Warning, "Test warning")  # no output
#' flag(Warning, "Test warning")  # repeated warning
#' reportFlags()  # consolidates the warnings and prints the message
#'
#' x <- 3
#' report(Info, "The value of x is #{x}")
#' report(Info, "A heading", prefix=FALSE)
#'
#' assert(x > 2, "x is too small")
#' tryCatch(report(Warning, "Something is amiss", class="myWarning"),
#'          myWarning=function (cond) conditionMessage(cond))
#'
#' \dontrun{
#' name <- ask("What is your name?")
#' report(Info, "Hello, #{name}")
#' }
#'
#' @seealso [fallback()] for errors which callers may demote; [reportAs()] for
#'   acting on classed messages; [setOutputLevel()] and [targets] for
#'   controlling what is written, and where; and [reportrCondition()] for the
#'   condition objects themselves.
#' @author Jon Clayden
#' @order 1
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

#' @rdname report
#' @order 2
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

#' @rdname report
#' @order 3
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

#' @rdname report
#' @order 4
#' @export
clearFlags <- function ()
{
    .Workspace$reportrFlags <- NULL
}

#' @rdname report
#' @order 5
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
