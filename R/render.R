#' The output level
#'
#' The output level is the master gate for reported messages: a message whose
#' level is below it is not written to any output target. `setOutputLevel()`
#' sets it, and `getOutputLevel()` returns it.
#'
#' The output level is stored in the `reportrOutputLevel` option, which may
#' also be set directly. If a message is reported before the output level has
#' been set, it is set to `Info`, with a message to say so.
#'
#' The output level governs only what is written. Errors are fatal whatever the
#' output level, and the conditions corresponding to messages which are not
#' written are still signalled when they have a class, or when handlers
#' established by [reportAs()] or [withReportrHandlers()] are active. At level
#' `Debug`, a stack trace is also written with any message at or above the
#' level given by the `reportrStackTraceLevel` option. Output targets may apply
#' stricter thresholds of their own; see [targets].
#'
#' @param level The minimum level of message to write. Levels may be named in
#'   any of the usual ways, so `Info`, `OL$Info` and `"Info"` are equivalent;
#'   see [reportr] for the full list. Levels outside the range `Debug` to
#'   `Fatal` are ignored.
#'
#' @return `setOutputLevel()` returns `NULL`, invisibly. `getOutputLevel()`
#'   returns the current output level, as an integer named after the level.
#'
#' @examples
#' setOutputLevel(Warning)
#' getOutputLevel()
#' report(Info, "Not written")
#'
#' setOutputLevel(OL$Info)
#' report(Info, "Written")
#'
#' @seealso [report()], and [targets] for controlling where messages are
#'   written.
#' @author Jon Clayden
#' @export
setOutputLevel <- function (level)
{
    level <- .evaluateLevel(level)
    if (level %in% OL$Debug:OL$Fatal)
        options(reportrOutputLevel=level)
    invisible(NULL)
}

#' @rdname setOutputLevel
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

#' Output targets
#'
#' Messages accepted for reporting are written to one or more output targets,
#' which may be the terminal, files, connections or arbitrary functions. By
#' default there is a single target, which writes to the terminal.
#' `setOutputTargets()` replaces the current targets with those given, or adds
#' to them, and `getOutputTargets()` returns them. `toTerminal()`, `toFile()`
#' and `toFunction()` create targets.
#'
#' The current output level (see [setOutputLevel()]) is the master gate: a
#' message which it suppresses never reaches any target. A target may
#' additionally set its own level threshold, so that, for example, a log file
#' captures everything from `Debug` upwards while the terminal shows only
#' `Info` and above. A threshold of `NULL`, the default, accepts every message
#' which passes the master gate. Levels may be named in any of the usual ways,
#' so `Info`, `OL$Info` and `"Info"` are equivalent.
#'
#' The terminal target writes messages below its `stderr` level to standard
#' output, and the rest to standard error.
#'
#' Files named by path are opened when they are first written to, and closed
#' when a call to `setOutputTargets()` no longer includes them. Connections
#' passed to `toFile()` are used as they are, and never closed by reportr.
#'
#' @section Prefixes:
#' Each target has its own prefix format, which describes the string prepended
#' to each message. It takes the form of a [sprintf()]-style format string, but
#' with different expansions for percent-escapes:
#'
#' * `%d` expands to a series of stars indicating the stack depth at which the
#'   message arose;
#' * `%f` gives the name of the function which raised the message;
#' * `%l` and `%L` give lower and upper case versions of the level of the
#'   message;
#' * `%p` expands to the ID of the current R process (see [Sys.getpid()]); and
#' * `%t` expands to the current time, formatted according to the
#'   `reportrTimeFormat` option.
#'
#' A prefix of `NULL`, the default, uses the value of the `reportrPrefixFormat`
#' option at the time the message is written, whose own default is `"%d%L: "`,
#' giving a prefix such as `"* * INFO: "`. A prefix of `FALSE` or `""` means no
#' prefix. Individual messages can be written without any prefix, whatever the
#' target, using the `prefix` argument to [report()].
#'
#' @param ... For `setOutputTargets()`, target objects created by the
#'   constructor functions, or lists of them such as those returned by
#'   `getOutputTargets()`. Arguments may be named, and otherwise a name is
#'   chosen for each target. Giving no targets at all silences all output,
#'   unless `add` is `TRUE`.
#' @param add Logical value: if `TRUE`, the targets given are added to the
#'   current ones, rather than replacing them.
#' @param prefix The prefix format for messages written to this target. See
#'   the Prefixes section.
#' @param stdout,stderr The minimum levels of message written to standard
#'   output and standard error, respectively. If `NULL`, all messages passing
#'   the master gate are written to standard output, and those at or above the
#'   level given by the `reportrStderrLevel` option (by default, `Warning`) are
#'   written to standard error instead.
#' @param file The path to a file, or a connection.
#' @param level The minimum level of message which this target will accept, or
#'   `NULL` to accept any which passes the master gate.
#' @param append If `file` is a path, should the file be appended to rather
#'   than overwritten?
#' @param fun A function, which is called with three arguments: the formatted
#'   text, the level of the message, and the condition object (which may be
#'   `NULL`).
#'
#' @return `setOutputTargets()` invisibly returns the targets which were in
#'   effect before the call, so that they can be restored later.
#'   `getOutputTargets()` returns a named list of the current targets. The
#'   constructors return a single target object.
#'
#' @examples
#' \dontrun{
#' # Log everything to a file, while the terminal shows warnings and above
#' setOutputLevel(OL$Debug)
#' old <- setOutputTargets(toTerminal(stdout=Warning),
#'                         toFile("run.log", prefix="%t %L: "))
#'
#' # Add another target to the current ones
#' setOutputTargets(log=toFunction(function (text, level, condition) ...), add=TRUE)
#'
#' # Restore the targets in effect before
#' setOutputTargets(old)
#' }
#'
#' @seealso [report()] and [setOutputLevel()]
#' @author Jon Clayden
#' @name targets
NULL

.target <- function (type, ...)
{
    structure(list(type=type, ...), class="reportrTarget")
}

.targetPrefix <- function (prefix)
{
    if (isFALSE(prefix))
        return ("")
    else if (is.null(prefix))
        return (NULL)
    else
        return (as.character(prefix)[1])
}

#' @rdname targets
#' @export
toTerminal <- function (prefix = NULL, stdout = NULL, stderr = NULL)
{
    .target("terminal", prefix=.targetPrefix(prefix),
            stdout=.evaluateLevel(stdout, substitute(stdout)),
            stderr=.evaluateLevel(stderr, substitute(stderr)))
}

#' @rdname targets
#' @export
toFile <- function (file, prefix = NULL, level = NULL, append = TRUE)
{
    if (is.character(file))
        file <- path.expand(file[1])
    else if (!inherits(file, "connection"))
        report(OL$Error, "A file target must be given a path or a connection")
    .target("file", prefix=.targetPrefix(prefix), level=.evaluateLevel(level), file=file, append=isTRUE(append))
}

#' @rdname targets
#' @export
toFunction <- function (fun, prefix = NULL, level = NULL)
{
    fun <- match.fun(fun)
    .target("function", prefix=.targetPrefix(prefix), level=.evaluateLevel(level), fun=fun)
}

#' @export
print.reportrTarget <- function (x, ...)
{
    description <- switch(x$type, terminal="the terminal", file=if (is.character(x$file)) paste("file", x$file) else "a connection", "a function")
    cat("Output target writing to ", description, "\n", sep="")
    invisible(x)
}

.defaultTargets <- function ()
{
    list(terminal=toTerminal())
}

.targets <- function ()
{
    targets <- .Workspace$targets
    if (is.null(targets))
        targets <- .defaultTargets()
    return (targets)
}

# Flatten a list of targets and lists of targets, giving every target a name
.collectTargets <- function (args)
{
    targets <- list()
    argNames <- names(args)
    for (i in seq_along(args))
    {
        arg <- args[[i]]
        if (inherits(arg, "reportrTarget"))
        {
            arg <- list(arg)
            names(arg) <- if (is.null(argNames)) "" else argNames[i]
        }
        else if (!is.list(arg) || !all(sapply(arg, inherits, "reportrTarget")))
            report(OL$Error, "Output targets must be created by toTerminal(), toFile() or toFunction()")
        else if (is.null(names(arg)))
            names(arg) <- rep("", length(arg))
        targets <- c(targets, arg)
    }

    if (length(targets) == 0)
        return (targets)

    defaultNames <- sapply(targets, function (target) {
        switch(target$type, terminal="terminal", file=if (is.character(target$file)) target$file else "connection", "function")
    })
    targetNames <- ifelse(names(targets) == "", defaultNames, names(targets))
    names(targets) <- make.unique(targetNames)
    return (targets)
}

#' @rdname targets
#' @export
setOutputTargets <- function (..., add = FALSE)
{
    # A target given as "add" would otherwise be taken silently as this flag
    if (!is.logical(add) || length(add) != 1L || is.na(add))
        report(OL$Error, "The \"add\" argument must be TRUE or FALSE")

    old <- .targets()
    targets <- .collectTargets(if (add) c(list(old), list(...)) else list(...))

    # Close any files which are no longer targets
    paths <- unlist(lapply(targets, function (target) if (target$type == "file" && is.character(target$file)) target$file))
    connections <- .Workspace$connections
    if (is.null(connections))
        connections <- list()
    for (path in setdiff(names(connections), paths))
    {
        try(close(connections[[path]]), silent=TRUE)
        connections[[path]] <- NULL
    }
    .Workspace$connections <- connections

    .Workspace$targets <- targets
    invisible(old)
}

#' @rdname targets
#' @export
getOutputTargets <- function ()
{
    .targets()
}

# The connection for a file target, which is opened the first time it's needed
.targetConnection <- function (target)
{
    if (!is.character(target$file))
        return (target$file)

    connection <- .Workspace$connections[[target$file]]
    if (is.null(connection))
    {
        connection <- file(target$file, open=ifelse(target$append,"at","wt"))
        connections <- .Workspace$connections
        if (is.null(connections))
            connections <- list()
        connections[[target$file]] <- connection
        .Workspace$connections <- connections
    }
    return (connection)
}

.truncate <- function (strings, maxLength)
{
    lengths <- nchar(strings)
    strings <- substr(strings, 1, maxLength)
    lines <- ore.split(ore("\n",syntax="fixed"), strings, simplify=FALSE)
    strings <- sapply(lines, "[", 1)
    strings <- paste(strings, ifelse(lengths>maxLength | sapply(lines,length)>1, " ...", ""), sep="")
    return (strings)
}

# The calls leading up to the point where a message arose, which are those in
# the condition for the message where there is one
.getCallStack <- function (calls = sys.calls())
{
    callStrings <- .truncate(as.character(calls), 100)

    # Drop everything up to and including the innermost handler-establishing
    # call, together with the frames it uses to install the handlers
    handlerFunLoc <- which(callStrings %~% "^(withReportrHandlers|reportAs)\\(")
    if (length(handlerFunLoc) > 0)
    {
        lastFrame <- handlerFunLoc[length(handlerFunLoc)]
        while (lastFrame < length(callStrings) && callStrings[lastFrame+1] %~% "^(eval|withCallingHandlers|withVisible)\\(")
            lastFrame <- lastFrame + 1
        callStrings <- callStrings[-seq_len(lastFrame)]
    }

    # Everything from the function which raised the message onwards is dropped,
    # including R's own machinery for warnings and errors raised in C code
    raisingFunLoc <- which(callStrings %~% "^\\.?(ask|assert|fallback|flag|report|reportFlags|signal|signalSimpleWarning|handleSimpleError|message|warning|stop)\\(")
    if (length(raisingFunLoc) > 0)
        callStrings <- callStrings[-(raisingFunLoc[1]:length(callStrings))]

    return (callStrings)
}

.buildPrefix <- function (level, format = NULL, calls = sys.calls())
{
    if (!is.null(format))
        prefix <- as.character(format)[1]
    else
        prefix <- as.character(.resolveOption("reportrPrefixFormat"))[1]

    if (prefix == "")
        return (prefix)
    else
    {
        if (prefix %~% "\\%(d|f)")
            stack <- .getCallStack(calls)

        if (prefix %~% "\\%d")
            prefix <- ore.subst(ore("%d",syntax="fixed"), paste(rep("* ",length(stack)),collapse=""), prefix, all=TRUE)
        if (prefix %~% "\\%f")
        {
            # There is no function to name at top level
            name <- if (length(stack) == 0) "" else ore.subst("^([\\w.]+)\\(.+$","\\1",stack[length(stack)])
            prefix <- ore.subst(ore("%f",syntax="fixed"), name, prefix, all=TRUE)
        }
        if (prefix %~% "\\%l")
            prefix <- ore.subst(ore("%l",syntax="fixed"), tolower(names(OL)[which(OL==level)]), prefix, all=TRUE)
        if (prefix %~% "\\%L")
            prefix <- ore.subst(ore("%L",syntax="fixed"), toupper(names(OL)[which(OL==level)]), prefix, all=TRUE)
        if (prefix %~% "\\%p")
            prefix <- ore.subst(ore("%p",syntax="fixed"), as.character(Sys.getpid()), prefix, all=TRUE)
        if (prefix %~% "\\%t")
            prefix <- ore.subst(ore("%t",syntax="fixed"), format(Sys.time(),.resolveOption("reportrTimeFormat")), prefix, all=TRUE)

        return (prefix)
    }
}

.buildStackTrace <- function (calls = sys.calls())
{
    stack <- .getCallStack(calls)
    if (length(stack) == 0)
        return ("")

    depths <- sapply(seq_along(stack), function(i) paste(rep("* ",i),collapse=""))
    paste("--- Begin stack trace ---\n",
          paste(depths, stack, "\n", sep="", collapse=""),
          "---  End stack trace  ---\n", sep="")
}

# The prefix format used by the terminal, for output which doesn't pass
# through the targets, such as questions
.terminalPrefix <- function ()
{
    for (target in .targets())
    {
        if (target$type == "terminal")
            return (target$prefix)
    }
    return (NULL)
}

# Write a message which is already in its final form to each target which
# accepts it. The output level has already been checked by this point. The
# stack is taken from the condition where possible, since a flagged message
# is rendered some time after it arose
.render <- function (level, message, outputLevel = .outputLevel(), condition = NULL, plain = FALSE)
{
    targets <- .targets()
    if (length(targets) == 0)
        return (invisible(NULL))

    calls <- if (is.null(condition$stack)) sys.calls() else condition$stack

    trace <- ""
    if (outputLevel == OL$Debug && level >= .resolveOption("reportrStackTraceLevel"))
        trace <- .buildStackTrace(calls)

    for (target in targets)
    {
        if (target$type == "terminal")
        {
            stderrLevel <- if (is.null(target$stderr)) .resolveOption("reportrStderrLevel") else target$stderr
            if (level >= stderrLevel)
                con <- stderr()
            else if (is.null(target$stdout) || level >= target$stdout)
                con <- stdout()
            else
                next
        }
        else if (!is.null(target$level) && level < target$level)
            next

        prefix <- if (plain) "" else .buildPrefix(level, target$prefix, calls)
        text <- paste(prefix, message, "\n", trace, sep="")

        if (target$type == "function")
            target$fun(text, level, condition)
        else if (target$type == "file")
            cat(text, file=.targetConnection(target))
        else
            cat(text, file=con)
    }

    invisible(NULL)
}
