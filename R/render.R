#' Report destinations
#'
#' Messages accepted for reporting are written to one or more destinations,
#' which may be the terminal, files, connections or arbitrary functions. By
#' default there is a single destination, named \code{"terminal"}, which
#' reproduces the standard behaviour of writing to standard output or standard
#' error according to the \code{reportrStderrLevel} option.
#'
#' The current output level (see \code{\link{setOutputLevel}}) is the master
#' gate: a message which it suppresses never reaches any destination. Each
#' destination then applies its own \code{level} threshold, so a log file can
#' capture everything from \code{Debug} upwards while the terminal shows only
#' \code{Info} and above.
#'
#' Removing the \code{"terminal"} destination sends output to the remaining
#' destinations only, which is how reporting to a file \emph{instead of} the
#' terminal is achieved.
#'
#' @param con A connection, the path to a file, or a function. A function is
#'   called with three arguments: the formatted text, the level of the message,
#'   and the condition object (which may be \code{NULL}).
#' @param name A string naming the destination, used to remove it later. If
#'   \code{NULL}, a file path is used as its own name; otherwise a name is
#'   generated.
#' @param level The minimum level of message which this destination will
#'   accept. See \code{\link{report}} for the available levels.
#' @param prefixFormat The format of the string prepended to messages sent to
#'   this destination. If given, it takes precedence over any format specified
#'   by the reporting call or the \code{reportrPrefixFormat} option. See
#'   \code{\link{report}} for the available escapes.
#' @param append If \code{con} is a file path, should the file be appended to
#'   rather than overwritten?
#'
#' @return \code{addReportDestination} invisibly returns the name of the
#'   destination added. \code{reportDestinations} returns a character vector of
#'   the names of the current destinations. The others return \code{NULL},
#'   invisibly.
#'
#' @examples
#' \dontrun{
#' # Log everything to a file, while the terminal shows warnings and above
#' setOutputLevel(OL$Debug)
#' addReportDestination("run.log", prefixFormat="%t %L: ")
#' addReportDestination(stdout(), name="terminal", level=OL$Warning)
#'
#' clearReportDestinations()
#' }
#'
#' @seealso \code{\link{report}}
#' @author Jon Clayden
#' @name destinations
NULL

.terminalDestination <- function ()
{
    list(name="terminal", con=NULL, level=OL$Debug, prefixFormat=NULL, terminal=TRUE, opened=FALSE)
}

.destinations <- function ()
{
    destinations <- .Workspace$destinations
    if (is.null(destinations))
        destinations <- list(terminal=.terminalDestination())
    return (destinations)
}

#' @rdname destinations
#' @export
addReportDestination <- function (con, name = NULL, level = OL$Debug, prefixFormat = NULL, append = TRUE)
{
    level <- .evaluateLevel(level)
    opened <- FALSE

    if (is.character(con))
    {
        if (is.null(name))
            name <- con[1]
        con <- file(con[1], open=ifelse(isTRUE(append),"at","wt"))
        opened <- TRUE
    }

    destinations <- .destinations()
    if (is.null(name))
        name <- paste("destination", length(destinations)+1L, sep="")

    destinations[[name]] <- list(name=name, con=con, level=level, prefixFormat=prefixFormat, terminal=FALSE, opened=opened)
    .Workspace$destinations <- destinations

    invisible(name)
}

#' @rdname destinations
#' @export
removeReportDestination <- function (name)
{
    destinations <- .destinations()
    for (currentName in as.character(name))
    {
        destination <- destinations[[currentName]]
        if (!is.null(destination))
        {
            if (isTRUE(destination$opened))
                try(close(destination$con), silent=TRUE)
            destinations[[currentName]] <- NULL
        }
    }
    .Workspace$destinations <- destinations
    invisible(NULL)
}

#' @rdname destinations
#' @export
clearReportDestinations <- function ()
{
    for (destination in .destinations())
    {
        if (isTRUE(destination$opened))
            try(close(destination$con), silent=TRUE)
    }
    .Workspace$destinations <- NULL
    invisible(NULL)
}

#' @rdname destinations
#' @export
reportDestinations <- function ()
{
    names(.destinations())
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

.getCallStack <- function ()
{
    callStrings <- .truncate(as.character(sys.calls()), 100)

    # Drop everything up to and including the innermost handler-establishing
    # call, together with the frames it uses to install the handlers
    handlerFunLoc <- which(callStrings %~% "^(withReportrHandlers|reportAs)\\(")
    if (length(handlerFunLoc) > 0)
    {
        lastFrame <- handlerFunLoc[length(handlerFunLoc)]
        while (lastFrame < length(callStrings) && callStrings[lastFrame+1] %~% "^(eval|withCallingHandlers)\\(")
            lastFrame <- lastFrame + 1
        callStrings <- callStrings[-seq_len(lastFrame)]
    }

    raisingFunLoc <- which(callStrings %~% "^\\.?(ask|assert|fallback|flag|report|reportFlags|signal|message|warning|stop)\\(")
    if (length(raisingFunLoc) > 0)
        callStrings <- callStrings[-(raisingFunLoc[1]:length(callStrings))]

    return (callStrings)
}

.buildPrefix <- function (level, format = NULL)
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
            stack <- .getCallStack()

        if (prefix %~% "\\%d")
            prefix <- ore.subst(ore("%d",syntax="fixed"), paste(rep("* ",length(stack)),collapse=""), prefix, all=TRUE)
        if (prefix %~% "\\%f")
            prefix <- ore.subst(ore("%f",syntax="fixed"), ore.subst("^([\\w.]+)\\(.+$","\\1",stack[length(stack)]), prefix, all=TRUE)
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

.buildStackTrace <- function ()
{
    stack <- .getCallStack()
    if (length(stack) == 0)
        return ("")

    depths <- sapply(seq_along(stack), function(i) paste(rep("* ",i),collapse=""))
    paste("--- Begin stack trace ---\n",
          paste(depths, stack, "\n", sep="", collapse=""),
          "---  End stack trace  ---\n", sep="")
}

# Write a message which is already in its final form to each active
# destination. The output level has already been checked by this point
.render <- function (level, message, prefixFormat = NULL, outputLevel = .outputLevel(), condition = NULL)
{
    destinations <- .destinations()
    if (length(destinations) == 0)
        return (invisible(NULL))

    trace <- ""
    if (outputLevel == OL$Debug && level >= .resolveOption("reportrStackTraceLevel"))
        trace <- .buildStackTrace()

    for (destination in destinations)
    {
        if (level < destination$level)
            next

        format <- if (is.null(destination$prefixFormat)) prefixFormat else destination$prefixFormat
        text <- paste(.buildPrefix(level,format), message, "\n", trace, sep="")

        if (is.function(destination$con))
            destination$con(text, level, condition)
        else if (destination$terminal)
            cat(text, file=if (level >= .resolveOption("reportrStderrLevel")) stderr() else stdout())
        else
            cat(text, file=destination$con)
    }

    invisible(NULL)
}
