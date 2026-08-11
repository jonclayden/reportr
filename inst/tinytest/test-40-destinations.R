options(reportrStderrLevel=OL$Fatal, reportrBaseClasses=FALSE, reportrMessageFilterOut=NULL, reportrStackTraceLevel=OL$Error)
setOutputLevel(OL$Debug)

expect_equal(reportDestinations(), "terminal")

# A file destination receives output alongside the terminal, and then instead
# of it once the terminal destination is removed
logFile <- tempfile()
addReportDestination(logFile, prefixFormat="%L: ")
expect_equal(reportDestinations(), c("terminal",logFile))
expect_stdout(report(OL$Info,"to both"), "to both", fixed=TRUE)
removeReportDestination("terminal")
expect_silent(report(OL$Info,"to file only"))
clearReportDestinations()
expect_equal(reportDestinations(), "terminal")
expect_equal(readLines(logFile), c("INFO: to both","INFO: to file only"))
unlink(logFile)

# Each destination applies its own level threshold, below the output level
logFile <- tempfile()
addReportDestination(logFile, name="log", level=OL$Warning, prefixFormat="%L: ")
report(OL$Info, "not in the file")
report(OL$Warning, "in the file")
clearReportDestinations()
expect_equal(readLines(logFile), "WARNING: in the file")
unlink(logFile)

# A destination may be a function
captured <- character(0)
addReportDestination(function (text, level, condition) captured <<- c(captured, text), name="fn")
removeReportDestination("terminal")
report(OL$Info, "captured message")
expect_equal(length(captured), 1L)
expect_true(grepl("captured message", captured[1], fixed=TRUE))

# ... which is given the level and the condition object
levels <- integer(0)
classes <- character(0)
clearReportDestinations()
addReportDestination(function (text, level, condition) {
                         levels <<- c(levels, level)
                         classes <<- c(classes, class(condition)[1])
                     }, name="fn")
removeReportDestination("terminal")
signalWarning("classed", class="myThing")
clearReportDestinations()
expect_equal(levels, OL$Warning)
expect_equal(classes, "myThing")

# Destinations can be removed individually, and by name
addReportDestination(tempfile(), name="one")
addReportDestination(tempfile(), name="two")
expect_equal(reportDestinations(), c("terminal","one","two"))
removeReportDestination("one")
expect_equal(reportDestinations(), c("terminal","two"))
clearReportDestinations()
expect_equal(reportDestinations(), "terminal")

# The timestamp escape
expect_true(grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", reportr:::.buildPrefix(OL$Info,"%t")))
options(reportrTimeFormat="%Y")
expect_true(grepl("^\\d{4}$", reportr:::.buildPrefix(OL$Info,"%t")))
options(reportrTimeFormat=NULL)

setOutputLevel(OL$Info)
