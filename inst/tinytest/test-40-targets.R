options(reportrStderrLevel=OL$Fatal, reportrBaseClasses=FALSE, reportrMessageFilterOut=NULL, reportrStackTraceLevel=OL$Error, reportrPrefixFormat=NULL)
setOutputLevel(OL$Debug)

stderrOf <- function (expr) capture.output(expr, type="message")

# By default there is just a terminal target
expect_equal(names(getOutputTargets()), "terminal")
expect_inherits(getOutputTargets()$terminal, "reportrTarget")

# A file target receives output alongside the terminal, and then instead of it
# once the terminal target is removed. The file is only opened when needed
logFile <- tempfile()
old <- setOutputTargets(toTerminal(), toFile(logFile, prefix="%L: "))
expect_equal(names(old), "terminal")
expect_equal(names(getOutputTargets()), c("terminal",logFile))
expect_false(file.exists(logFile))
expect_stdout(report(OL$Info,"to both"), "to both", fixed=TRUE)
setOutputTargets(getOutputTargets()[logFile])
expect_silent(report(OL$Info,"to file only"))
setOutputTargets(old)
expect_equal(names(getOutputTargets()), "terminal")
expect_equal(readLines(logFile), c("INFO: to both","INFO: to file only"))

# Setting the targets again doesn't reopen a file, so a file which is not
# appended to is only truncated once
setOutputTargets(toFile(logFile, prefix="%L: ", append=FALSE))
report(OL$Info, "first")
setOutputTargets(getOutputTargets())
report(OL$Info, "second")
setOutputTargets(old)
expect_equal(readLines(logFile), c("INFO: first","INFO: second"))
unlink(logFile)

# Each target may apply its own level threshold, below the output level
logFile <- tempfile()
setOutputTargets(log=toFile(logFile, level=Warning, prefix="%L: "))
expect_equal(names(getOutputTargets()), "log")
report(OL$Info, "not in the file")
report(OL$Warning, "in the file")
setOutputTargets(old)
expect_equal(readLines(logFile), "WARNING: in the file")
unlink(logFile)

# The terminal has separate thresholds for standard output and standard error
setOutputTargets(toTerminal(stdout=Warning))
expect_silent(report(Info, "hidden"))
expect_stdout(report(Warning, "shown"), "shown", fixed=TRUE)
setOutputTargets(toTerminal(stdout="Info", stderr=OL$Warning))
expect_silent(report(Verbose, "hidden"))
expect_stdout(report(Info, "shown"), "shown", fixed=TRUE)
expect_true(any(grepl("to stderr", stderrOf(report(Warning, "to stderr")), fixed=TRUE)))

# ... and by default it follows the reportrStderrLevel option at the time
setOutputTargets(old)
options(reportrStderrLevel=OL$Info)
expect_true(any(grepl("to stderr", stderrOf(report(Info, "to stderr")), fixed=TRUE)))
options(reportrStderrLevel=OL$Fatal)
expect_stdout(report(Info, "to stdout"), "to stdout", fixed=TRUE)

# Prefixes belong to targets, and by default follow the reportrPrefixFormat
# option at the time
setOutputTargets(toTerminal(prefix="[%l] "))
expect_stdout(report(Info, "custom"), "[info] custom", fixed=TRUE)
setOutputTargets(toTerminal(prefix=FALSE))
expect_stdout(report(Info, "bare"), "^bare$")
setOutputTargets(old)
options(reportrPrefixFormat="%L> ")
expect_stdout(report(Info, "from option"), "INFO> from option", fixed=TRUE)
options(reportrPrefixFormat=NULL)

# A message can be written without a prefix, to every target
logFile <- tempfile()
setOutputTargets(toTerminal(), toFile(logFile, prefix="%t %L: "))
expect_stdout(report(Info, "HEADING", prefix=FALSE), "^HEADING$")
setOutputTargets(old)
expect_equal(readLines(logFile), "HEADING")
unlink(logFile)
expect_true(tryCatch(report(Info, "m", class="k", prefix=FALSE), k=function(cond) cond$plain))

# ... which is also what the deprecated prefixFormat="" means
expect_stdout(report(Info, "HEADING", prefixFormat=""), "^HEADING$")
expect_stdout(assert(FALSE, "PLAIN", level=Info, prefixFormat=""), "^PLAIN$")

# Any other prefixFormat is ignored, with a warning given only once
workspace <- reportr:::.Workspace
workspace$warnedPrefixFormat <- NULL
expect_stdout(report(Info, "ignored format", prefixFormat="%p "), "deprecated", fixed=TRUE)
expect_stdout(report(Info, "ignored format", prefixFormat="%p "), "^[* ]*INFO: ignored format$")

# Plain messages stay plain when remapped
expect_stdout(reportAs(report(Info, "PLAIN", class="k", prefix=FALSE), k=Warning), "^PLAIN$")

# Targets can be added to the current set, and all of them can be removed
captured <- character(0)
setOutputTargets(getOutputTargets(), fn=toFunction(function (text, level, condition) captured <<- c(captured, text)))
expect_equal(names(getOutputTargets()), c("terminal","fn"))
expect_stdout(report(OL$Info,"captured message"), "captured message", fixed=TRUE)
expect_true(grepl("^[* ]*INFO: captured message\n$", captured))
setOutputTargets()
expect_equal(length(getOutputTargets()), 0L)
expect_silent(report(OL$Info, "silenced"))
setOutputTargets(old)

# A function target is given the level and the condition object
levels <- integer(0)
classes <- character(0)
setOutputTargets(toFunction(function (text, level, condition) {
    levels <<- c(levels, level)
    classes <<- c(classes, class(condition)[1])
}))
report(Warning, "classed", class="myThing")
setOutputTargets(old)
expect_equal(levels, OL$Warning)
expect_equal(classes, "myThing")

# The add argument appends to the current targets rather than replacing them,
# and the previous targets are still returned
setOutputTargets(toTerminal(), fn=toFunction(function (...) NULL))
previous <- setOutputTargets(toFunction(function (...) NULL), add=TRUE)
expect_equal(names(previous), c("terminal","fn"))
expect_equal(names(getOutputTargets()), c("terminal","fn","function"))
setOutputTargets(add=TRUE)
expect_equal(names(getOutputTargets()), c("terminal","fn","function"))
expect_error(setOutputTargets(add=toTerminal()), "must be TRUE or FALSE")
expect_equal(names(getOutputTargets()), c("terminal","fn","function"))
setOutputTargets(old)

# Duplicate names are made unique, and invalid targets are rejected
setOutputTargets(toTerminal(), toTerminal())
expect_equal(names(getOutputTargets()), c("terminal","terminal.1"))
setOutputTargets(old)
expect_error(setOutputTargets("terminal"), "must be created by")
expect_error(toFile(42), "path or a connection")

# Flagged messages are rendered as they would have been where they arose
captured <- character(0)
setOutputTargets(toFunction(function (text, level, condition) captured <<- c(captured, text), prefix="%d%f %L: "))
inner <- function () flag(Warning, "deep flag")
outer <- function () inner()
inner()
outer()
reportFlags()
setOutputTargets(old)
expect_equal(length(captured), 1L)
expect_true(grepl("inner WARNING: [x2] deep flag", captured, fixed=TRUE))
depth <- function (text) nchar(gsub("[^*]", "", sub(":.*$", "", text)))
report(Info, "reference")
setOutputTargets(toFunction(function (text, level, condition) captured <<- text, prefix="%d%L: "))
f <- function () report(Info, "at depth")
f()
direct <- depth(captured)
outer()
reportFlags()
setOutputTargets(old)
expect_equal(depth(captured), direct + 1, info=captured)

# ... including warnings raised in C code, where R's own frames are omitted,
# along with those outside the handlers
captured <- character(0)
setOutputTargets(toFunction(function (text, level, condition) captured <<- c(captured, text), prefix="%d%f %L: "))
roots <- function (x) sqrt(x)
invisible(withReportrHandlers(roots(-1)))
setOutputTargets(old)
expect_equal(captured, "* roots WARNING: NaNs produced\n")

# Questions use the terminal's prefix, unless asked not to
if (at_home())
{
    ns <- getNamespace("reportr")
    oldInteractive <- get(".interactive", ns)
    oldReadline <- get(".readline", ns)
    unlockBinding(".interactive", ns)
    unlockBinding(".readline", ns)
    assign(".interactive", function() TRUE, envir=ns)
    prompt <- NULL
    assign(".readline", function (text) { prompt <<- text; "y" }, envir=ns)

    setOutputTargets(toTerminal(prefix="%L? "))
    ask("Continue")
    expect_equal(prompt, "QUESTION? Continue ")
    ask("Continue", prefix=FALSE)
    expect_equal(prompt, "Continue ")
    setOutputTargets(old)

    assign(".interactive", oldInteractive, envir=ns)
    assign(".readline", oldReadline, envir=ns)
}

# At top level, there is no function to name
expect_equal(reportr:::.buildPrefix(OL$Info, "%f%L", calls=list()), "INFO")

# The timestamp escape
expect_true(grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", reportr:::.buildPrefix(OL$Info,"%t")))
options(reportrTimeFormat="%Y")
expect_true(grepl("^\\d{4}$", reportr:::.buildPrefix(OL$Info,"%t")))
options(reportrTimeFormat=NULL)

setOutputLevel(OL$Info)
