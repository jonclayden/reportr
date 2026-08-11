options(reportrStderrLevel=OL$Fatal, reportrBaseClasses=FALSE, reportrMessageFilterOut=NULL, reportrStackTraceLevel=OL$Error)
setOutputLevel(OL$Info)

# The class vector runs from most to least specific
expect_equal(class(reportrCondition(OL$Info,"m")), c("reportrInfo","reportrCondition","condition"))
expect_equal(class(reportrCondition(OL$Warning,"m")), c("reportrWarning","reportrCondition","condition"))
expect_equal(class(reportrCondition(OL$Question,"m")), c("reportrQuestion","reportrCondition","condition"))
expect_equal(class(reportrCondition(OL$Warning,"m",class="custom")), c("custom","reportrWarning","reportrCondition","condition"))
expect_equal(class(reportrCondition(OL$Warning,"m",class=c("a","b"))), c("a","b","reportrWarning","reportrCondition","condition"))

# Error and Fatal always inherit from "error"
expect_equal(class(reportrCondition(OL$Error,"m")), c("reportrError","reportrCondition","error","condition"))
expect_equal(class(reportrCondition(OL$Fatal,"m")), c("reportrFatal","reportrCondition","error","condition"))

# Levels may be named in any of the usual ways
expect_equal(class(reportrCondition(Warning,"m"))[1], "reportrWarning")
expect_equal(class(reportrCondition("Warning","m"))[1], "reportrWarning")

# The standard condition accessors work
cond <- reportrCondition(OL$Info, "hello", call=quote(f(x)))
expect_equal(conditionMessage(cond), "hello")
expect_equal(conditionCall(cond), quote(f(x)))
expect_equal(cond$level, OL$Info)

# Base classes are opt-in
options(reportrBaseClasses=TRUE)
expect_true(inherits(reportrCondition(OL$Warning,"m"), "warning"))
expect_true(inherits(reportrCondition(OL$Info,"m"), "message"))
expect_true(inherits(reportrCondition(OL$Debug,"m"), "message"))
expect_false(inherits(reportrCondition(OL$Question,"m"), "message"))
options(reportrBaseClasses=FALSE)
expect_false(inherits(reportrCondition(OL$Warning,"m"), "warning"))
expect_false(inherits(reportrCondition(OL$Info,"m"), "message"))

# Errors reported by reportr are now catchable in the usual ways
expect_error(report(OL$Error, "bang"), "bang")
expect_error(report(OL$Fatal, "doom"), "doom")
expect_true(inherits(try(report(OL$Error,"bang"), silent=TRUE), "try-error"))
expect_equal(tryCatch(report(OL$Error,"bang"), error=function(e) conditionMessage(e)), "bang")

# An unclassified message still carries a class derived from its level
expect_equal(tryCatch(report(OL$Info,"x"), reportrInfo=function(cond) "matched"), "matched")
expect_equal(tryCatch(report(OL$Warning,"x"), reportrCondition=function(cond) "matched"), "matched")

# The condition records the level, stack and recoverability
cond <- tryCatch(report(OL$Error,"details"), error=function(e) e)
expect_equal(cond$level, OL$Error)
expect_true(length(cond$stack) > 0L)
expect_false(cond$recoverable)

# A message filtered out of the output is still signalled, and still fatal
options(reportrMessageFilterOut="^hidden")
expect_error(report(OL$Error, "hidden failure"))
expect_null(report(OL$Info, "hidden info"))
expect_equal(tryCatch(report(OL$Info,"hidden info"), reportrInfo=function(cond) "signalled"), "signalled")
options(reportrMessageFilterOut=NULL)

# An assertion whose expression reports an error is a failed assertion, just as
# it is when the expression raises an error in R's own way
expect_stdout(assert(3 + "n", "could not be evaluated", level=OL$Info), "could not be evaluated", fixed=TRUE)
expect_stdout(assert(report(OL$Error,"inner"), "could not be evaluated", level=OL$Info), "could not be evaluated", fixed=TRUE)

# The muffleReport restart suppresses output without affecting the result
expect_silent(withCallingHandlers(report(OL$Info,"quiet"), reportrInfo=function(cond) invokeRestart("muffleReport")))

# The useValue restart recovers, even from an error
expect_equal(withCallingHandlers(report(OL$Error,"recover"), reportrError=function(cond) invokeRestart("useValue", 99)), 99)
