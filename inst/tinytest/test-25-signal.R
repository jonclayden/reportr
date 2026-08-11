options(reportrStderrLevel=OL$Fatal, reportrBaseClasses=FALSE, reportrMessageFilterOut=NULL, reportrStackTraceLevel=OL$Error)
setOutputLevel(OL$Info)

# An unhandled signal is reported at its nominal level
expect_stdout(signalWarning("a problem", class="myClass"), "WARNING: a problem", fixed=TRUE)

# Each wrapper signals at its own level
expect_equal(tryCatch(signalDebug("m"), reportrDebug=function(cond) cond$level), OL$Debug)
expect_equal(tryCatch(signalVerbose("m"), reportrVerbose=function(cond) cond$level), OL$Verbose)
expect_equal(tryCatch(signalInfo("m"), reportrInfo=function(cond) cond$level), OL$Info)
expect_equal(tryCatch(signalWarning("m"), reportrWarning=function(cond) cond$level), OL$Warning)
expect_equal(tryCatch(signalError("m"), reportrError=function(cond) cond$level), OL$Error)

# The general form accepts all three ways of naming a level
expect_equal(tryCatch(signal(OL$Warning,"m"), reportrWarning=function(cond) "ok"), "ok")
expect_equal(tryCatch(signal(Warning,"m"), reportrWarning=function(cond) "ok"), "ok")
expect_equal(tryCatch(signal("Warning","m"), reportrWarning=function(cond) "ok"), "ok")

# Classes are additive, and the level class is always present
expect_equal(tryCatch(signalWarning("m",class="mine"), mine=function(cond) class(cond)[1:3]),
             c("mine","reportrWarning","reportrCondition"))

# signalError is fatal, and carries its class
expect_error(signalError("fatal thing", class="myError"), "fatal thing")
expect_equal(tryCatch(signalError("m",class="myError"), myError=function(cond) "caught"), "caught")

# The default is returned when the condition goes unhandled
expect_null(suppressWarnings(signalWarning("a problem", class="myClass")))
expect_equal(signalWarning("a problem", class="myClass", default=NA), NA)
expect_equal(signalWarning("a problem", class="myClass", default=list(1,2))[[2]], 2)

# A default of NULL is distinguishable from no default at all
expect_true(tryCatch(signalWarning("m", class="k", default=NULL), k=function(cond) cond$recoverable))
expect_false(tryCatch(signalWarning("m", class="k"), k=function(cond) cond$recoverable))
expect_null(tryCatch(signalWarning("m", class="k", default=NULL), k=function(cond) cond$default))

# Expression substitution uses the frame of the calling function
f <- function () { x <- 7; signalInfo("x is #{x}") }
expect_stdout(f(), "x is 7", fixed=TRUE)
g <- function () { y <- 2; signal(OL$Info, "y is #{y}") }
expect_stdout(g(), "y is 2", fixed=TRUE)

# The call is recorded, so handlers can see where the condition arose
h <- function (a) signalWarning("m", class="k")
expect_equal(as.character(tryCatch(h(1), k=function(cond) conditionCall(cond))[[1]]), "h")

# Conditions are signalled even when the output level means nothing is reported
setOutputLevel(OL$Warning)
expect_equal(tryCatch(signalInfo("quiet", class="quietClass"), quietClass=function(cond) "signalled"), "signalled")
expect_silent(signalInfo("quiet", class="quietClass"))
setOutputLevel(OL$Info)

# Restarts are available to handlers
expect_equal(withCallingHandlers(signalWarning("m", class="k", default="d"),
                                 k=function(cond) invokeRestart("useValue", "recovered")), "recovered")
expect_equal(withCallingHandlers(signalError("m", class="k", default="d"),
                                 k=function(cond) invokeRestart("useValue", "recovered")), "recovered")
expect_silent(withCallingHandlers(signalWarning("m", class="k"),
                                  k=function(cond) invokeRestart("muffleReport")))
