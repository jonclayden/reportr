options(reportrStderrLevel=OL$Fatal, reportrBaseClasses=FALSE, reportrMessageFilterOut=NULL, reportrStackTraceLevel=OL$Error)
setOutputLevel(OL$Info)

# Conditions raised by code that knows nothing of reportr are translated
f <- function () message("Howdy")
expect_stdout(withReportrHandlers(f()), "INFO: Howdy", fixed=TRUE)
expect_stdout(withReportrHandlers(warning("careful")), "WARNING: careful", fixed=TRUE)
expect_error(withReportrHandlers(stop("bang")), "bang")

# Repeated warnings are consolidated
expect_stdout(withReportrHandlers(for (i in 1:3) sqrt(-1)), "[x3]", fixed=TRUE)

# The value of the expression is returned
expect_equal(withReportrHandlers(1 + 1), 2)
expect_equal(withReportrHandlers({ message("noise"); "value" }), "value")

# The classes of a foreign condition are carried over, so outer handlers match
myError <- function () stop(structure(class=c("myPkgError","error","condition"), list(message="custom", call=NULL)))
expect_equal(tryCatch(withReportrHandlers(myError()), myPkgError=function(cond) "matched"), "matched")
expect_true(tryCatch(withReportrHandlers(myError()), myPkgError=function(cond) inherits(cond$original,"myPkgError")))
expect_true(tryCatch(withReportrHandlers(myError()), myPkgError=function(cond) inherits(cond,"reportrError")))

myWarning <- function () warning(structure(class=c("myPkgWarning","warning","condition"), list(message="odd", call=NULL)))
expect_equal(tryCatch(withReportrHandlers(myWarning()), myPkgWarning=function(cond) "matched"), "matched")

# Text arriving from outside reportr is never passed through es()
expect_stdout(withReportrHandlers(message("literal #{40+2}")), "#{40+2}", fixed=TRUE)
expect_stdout(withReportrHandlers(warning("literal #{40+2}")), "#{40+2}", fixed=TRUE)

# withReportrHandlers also accepts class mappings
findThing <- function () { report(Warning, "missing", class="missingThing"); NA }
expect_silent(withReportrHandlers(findThing(), missingThing=Ignore))
expect_error(withReportrHandlers(findThing(), missingThing=Error), "missing")
expect_equal(withReportrHandlers(findThing(), missingThing=Ignore), NA)

# Flagged messages are signalled where they arise, not where they are reported
seen <- NULL
h <- function () { flag(OL$Warning, "deferred"); "done" }
expect_equal(withCallingHandlers(h(), reportrWarning=function(cond) seen <<- conditionMessage(cond)), "done")
expect_equal(seen, "deferred")
expect_stdout(reportFlags(), "WARNING: deferred", fixed=TRUE)

# Session-wide handlers can be installed and removed again. This has to happen
# in a subprocess, because globalCallingHandlers() may only be called when no
# handlers are already established, which is not true under the test runner
if (at_home() && getRversion() >= "4.0.0")
{
    script <- tempfile(fileext=".R")
    writeLines(c(sprintf(".libPaths(%s)", paste(deparse(.libPaths()),collapse="")),
                 "library(reportr)",
                 "setOutputLevel(OL$Info)",
                 "options(reportrStderrLevel=OL$Fatal)",
                 "cat(paste0('handlers:', length(globalCallingHandlers()), '\\n'))",
                 "reportrHandlers(TRUE)",
                 "cat(paste0('handlers:', length(globalCallingHandlers()), '\\n'))",
                 "message('a plain message')",
                 "reportrHandlers(FALSE)",
                 "cat(paste0('handlers:', length(globalCallingHandlers()), '\\n'))",
                 "message('untouched')"), script)
    output <- suppressWarnings(system2(file.path(R.home("bin"),"Rscript"), shQuote(script), stdout=TRUE, stderr=TRUE))
    unlink(script)

    expect_equal(grep("^handlers:", output, value=TRUE), c("handlers:0","handlers:3","handlers:0"))
    expect_true(any(grepl("INFO: a plain message", output, fixed=TRUE)))
    expect_true(any(grepl("^untouched$", output)))
}

# Wrapping an expression doesn't change whether its result is visible
expect_false(withVisible(withReportrHandlers(invisible(1)))$visible)
expect_true(withVisible(withReportrHandlers(1))$visible)
expect_false(withVisible(reportAs(invisible(1)))$visible)
expect_true(withVisible(reportAs(1))$visible)
