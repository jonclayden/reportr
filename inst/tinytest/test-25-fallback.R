options(reportrStderrLevel=OL$Fatal, reportrBaseClasses=FALSE, reportrMessageFilterOut=NULL, reportrStackTraceLevel=OL$Error)
setOutputLevel(OL$Info)

# By default a fallback is an error, which carries its class
expect_error(fallback(NULL, "fatal thing", class="myError"), "fatal thing")
expect_equal(tryCatch(fallback(NULL, "m", class="myError"), reportrError=function(cond) cond$level), OL$Error)
expect_equal(tryCatch(fallback(NULL, "m", class="myError"), myError=function(cond) "caught"), "caught")

# Classes are additive, and the level class is always present
expect_equal(tryCatch(fallback(NULL, "m", class="mine"), mine=function(cond) class(cond)[1:3]),
             c("mine","reportrError","reportrCondition"))

# The level may be given, in any of the usual ways
expect_stdout(fallback(NA, "a problem", class="myClass", level=Warning), "WARNING: a problem", fixed=TRUE)
expect_equal(tryCatch(fallback(NA, "m", level=OL$Info), reportrInfo=function(cond) "ok"), "ok")
expect_equal(tryCatch(fallback(NA, "m", level="Info"), reportrInfo=function(cond) "ok"), "ok")

# Below Error, the value is returned when the condition goes unhandled
expect_equal(fallback(NA, "a problem", level=Warning), NA)
expect_null(fallback(NULL, "a problem", level=Warning))
expect_equal(fallback(list(1,2), "a problem", level=Warning)[[2]], 2)
expect_stdout(print(reportAs(fallback(NA, "m", class="k"), k=Ignore)), "NA", fixed=TRUE)
expect_true(withVisible(reportAs(fallback(NA, "m", class="k"), k=Ignore))$visible)

# The value must be given
expect_error(fallback(level=Warning), "must be given")
expect_error(fallback(class="k", level=Warning), "must be given")

# The condition is recoverable, and carries its value
expect_true(tryCatch(fallback(NULL, "m", class="k"), k=function(cond) cond$recoverable))
expect_equal(tryCatch(fallback("v", "m", class="k"), k=function(cond) cond$default), "v")

# Expression substitution uses the frame of the calling function
f <- function () { x <- 7; fallback(NULL, "x is #{x}", level=Info) }
expect_stdout(f(), "x is 7", fixed=TRUE)

# The call is recorded, so handlers can see where the condition arose
h <- function (a) fallback(NULL, "m", class="k")
expect_equal(as.character(tryCatch(h(1), k=function(cond) conditionCall(cond))[[1]]), "h")

# Conditions are signalled even when the output level means nothing is reported
setOutputLevel(OL$Warning)
expect_equal(tryCatch(fallback(NULL, "quiet", class="quietClass", level=Info), quietClass=function(cond) "signalled"), "signalled")
expect_silent(fallback(NULL, "quiet", class="quietClass", level=Info))
setOutputLevel(OL$Info)

# Restarts are available to handlers, including useValue for errors
expect_equal(withCallingHandlers(fallback("d", "m", class="k", level=Warning),
                                 k=function(cond) invokeRestart("useValue", "recovered")), "recovered")
expect_equal(withCallingHandlers(fallback("d", "m", class="k"),
                                 k=function(cond) invokeRestart("useValue", "recovered")), "recovered")
expect_silent(withCallingHandlers(fallback("d", "m", class="k", level=Warning),
                                  k=function(cond) invokeRestart("muffleReport")))

# A typical guard, in a function which resolves something
locate <- function (name)
{
    if (name != "present")
        return(fallback(NULL, "Can't find #{name}", class="missingThing"))
    return("found")
}
expect_equal(locate("present"), "found")
expect_error(locate("absent"), "Can't find absent")
expect_null(reportAs(locate("absent"), missingThing=Ignore))
