options(reportrOutputLevel=NULL, reportrStderrLevel=OL$Fatal)

expect_stdout(getOutputLevel(), "Output level is not set", fixed=TRUE)
options(reportrOutputLevel=OL$Debug)
expect_equivalent(getOutputLevel(), OL$Debug)
setOutputLevel(OL$Info)
expect_equivalent(getOutputLevel(), OL$Info)

setOutputLevel(OL$Warning)
expect_silent(report(OL$Info,"Test message"))

setOutputLevel(OL$Info)
expect_stdout(report(OL$Info,"Test message"), "Test message", fixed=TRUE)
expect_stdout(report(Info,"Test message"), "Test message", fixed=TRUE)
expect_stdout(report("Info","Test message"), "Test message", fixed=TRUE)

flag(OL$Warning, "Test warning")
flag(OL$Warning, "Test warning")
expect_stdout(reportFlags(), "[x2]", fixed=TRUE)

expect_warning(sqrt(-1), "NaNs produced", fixed=TRUE)
expect_stdout(withReportrHandlers(sqrt(-1)), "NaNs produced", fixed=TRUE)

f <- function() message("Howdy")
expect_stdout(withReportrHandlers(f()), "* INFO: Howdy", fixed=TRUE)

setOutputLevel(OL$Debug)
options(reportrStackTraceLevel=OL$Info)
expect_stdout(flag(Info,"Converted to report"), "Converted to report", fixed=TRUE)
expect_stdout(withReportrHandlers(f()), "* f()", fixed=TRUE)

setOutputLevel(OL$Info)
options(reportrMessageFilterOut="^T")
expect_null(report(OL$Info, "Test message"))
options(reportrMessageFilterOut=NULL)
