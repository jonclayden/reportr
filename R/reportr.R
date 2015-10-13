.resolveOption <- function (name)
{
    value <- getOption(name)
    if (is.null(value))
        value <- .Defaults[[name]]
    return (value)
}

.evaluateLevel <- function (level)
{
    name <- as.character(substitute(level,parent.frame()))
    if (length(name) == 1 && name %in% names(OL))
        return (OL[[name]])
    else
        return (level)
}

setOutputLevel <- function (level)
{
    level <- .evaluateLevel(level)
    if (level %in% OL$Debug:OL$Fatal)
        options(reportrOutputLevel=level)
    invisible(NULL)
}

getOutputLevel <- function ()
{
    if (is.null(getOption("reportrOutputLevel")))
    {
        setOutputLevel(OL$Info)
        report(OL$Info, "Output level is not set; defaulting to \"Info\"", prefixFormat="")
        return (OL$Info)
    }
    else
        return (getOption("reportrOutputLevel"))
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

withReportrHandlers <- function (expr)
{
    withCallingHandlers(expr, message=function (m) {
        report(OL$Info, ore.subst("\n$","",m$message))
        invokeRestart("muffleMessage")
    }, warning=function (w) {
        flag(OL$Warning, w$message)
        invokeRestart("muffleWarning")
    }, error=function (e) {
        if (is.null(e$call))
            report(OL$Error, e$message)
        else
            report(OL$Error, e$message, " (in \"", as.character(e$call)[1], "(", .truncate(paste(as.character(e$call)[-1],collapse=", "),100), ")\")")
    })
}

.getCallStack <- function ()
{
    callStrings <- .truncate(as.character(sys.calls()), 100)
    
    handlerFunLoc <- which(callStrings %~% "^withReportrHandlers\\(")
    if (length(handlerFunLoc) > 0)
        callStrings <- callStrings[-seq_len(handlerFunLoc[length(handlerFunLoc)]+1)]
    
    reportrFunLoc <- which(callStrings %~% "^(ask|flag|report|reportFlags)\\(")
    if (length(reportrFunLoc) > 0)
        callStrings <- callStrings[-(reportrFunLoc[length(reportrFunLoc)]:length(callStrings))]
    
    filterIn <- .resolveOption("reportrStackFilterIn")
    filterOut <- .resolveOption("reportrStackFilterOut")
    if (!is.null(filterIn))
        callStrings <- callStrings[callStrings %~% as.character(filterIn)[1]]
    if (!is.null(filterOut))
        callStrings <- callStrings[!(callStrings %~% as.character(filterOut)[1])]
    
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
            prefix <- ore.subst(ore("%p",syntax="fixed"), Sys.getpid(), prefix, all=TRUE)

        return (prefix)
    }
}

.buildMessage <- function (..., round = NULL, signif = NULL)
{
    # This assumes that the environment containing relevant variables is the grandparent of the current one
    message <- es(paste(..., sep=""), round=round, signif=signif, envir=parent.frame(2))
    keep <- TRUE
    
    filterIn <- .resolveOption("reportrMessageFilterIn")
    filterOut <- .resolveOption("reportrMessageFilterOut")
    if (!is.null(filterIn))
        keep <- keep & (message %~% as.character(filterIn)[1])
    if (!is.null(filterOut))
        keep <- keep & (!(message %~% as.character(filterOut)[1]))
    
    if (keep)
        return (message)
    else
        return (NULL)
}

ask <- function (..., default = NULL, prefixFormat = NULL)
{
    outputLevel <- getOutputLevel()
    message <- .buildMessage(...)
    if (outputLevel > OL$Question || is.null(message))
        return (default)
    else
    {
        reportFlags()
        ans <- readline(paste(.buildPrefix(OL$Question,prefixFormat), message, " ", sep=""))
        return (ans)
    }
}

report <- function (level, ..., prefixFormat = NULL)
{
    level <- .evaluateLevel(level)
    outputLevel <- getOutputLevel()
    message <- .buildMessage(...)
    if (outputLevel > level || is.null(message))
        return (invisible(NULL))
    
    reportFlags()
    
    if (level >= .resolveOption("reportrStderrLevel"))
        cat(paste(.buildPrefix(level,prefixFormat), message, "\n", sep=""), file=stderr())
    else
        cat(paste(.buildPrefix(level,prefixFormat), message, "\n", sep=""))
    
    if (outputLevel == OL$Debug)
    {
        if (level >= .resolveOption("reportrStackTraceLevel"))
        {
            stack <- .getCallStack()
            cat("--- Begin stack trace ---\n", file=stderr())
            for (i in 1:length(stack))
                cat(rep("* ", i), stack[i], "\n", sep="", file=stderr())
            cat("---  End stack trace  ---\n", file=stderr())
        }
    }
    
    if (level == OL$Error)
        invokeRestart("abort")
}

flag <- function (level, ...)
{
    level <- .evaluateLevel(level)
    if (getOutputLevel() == OL$Debug)
    {
        if (level >= .resolveOption("reportrStackTraceLevel"))
        {
            report(level, ...)
            return (invisible(NULL))
        }
    }
    
    message <- .buildMessage(...)
    if (is.null(message))
        return (invisible(NULL))
    currentFlag <- list(list(level=level, message=message))
    
    if (!exists("reportrFlags",.Workspace) || is.null(.Workspace$reportrFlags))
        .Workspace$reportrFlags <- currentFlag
    else
        .Workspace$reportrFlags <- c(.Workspace$reportrFlags, currentFlag)
}

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
            if (length(locs) == 1)
                report(level, message, prefixFormat="%L: ")
            else
                report(level, paste("[x",length(locs),"] ",message,sep=""), prefixFormat="%L: ")
        }
    }
}

clearFlags <- function ()
{
    .Workspace$reportrFlags <- NULL
}
