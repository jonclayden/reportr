options(reportrStderrLevel=OL$Fatal, reportrBaseClasses=FALSE, reportrMessageFilterOut=NULL, reportrStackTraceLevel=OL$Error)
setOutputLevel(OL$Info)

# The callee describes what happened; it takes no "errorIfMissing"-style flag
findThing <- function (name)
{
    if (name != "widget")
    {
        report(Warning, "There is no #{name}", class="missingThing")
        return(NA)
    }
    return("the widget")
}

# Left alone, the condition is reported at the level it was signalled at
expect_stdout(findThing("sprocket"), "WARNING: There is no sprocket", fixed=TRUE)

# The caller can escalate it, however the level is spelled
expect_error(reportAs(findThing("sprocket"), missingThing=Error), "There is no sprocket")
expect_error(reportAs(findThing("sprocket"), missingThing=OL$Error), "There is no sprocket")
expect_error(reportAs(findThing("sprocket"), missingThing="Error"), "There is no sprocket")

# ... demote it ...
expect_silent(reportAs(findThing("sprocket"), missingThing=Debug))
expect_stdout(reportAs(findThing("sprocket"), missingThing=Info), "INFO: There is no sprocket", fixed=TRUE)

# ... or suppress it entirely, while the function carries on as usual
expect_silent(reportAs(findThing("sprocket"), missingThing=Ignore))
expect_equal(reportAs(findThing("sprocket"), missingThing=Ignore), NA)

# The successful path is untouched
expect_equal(reportAs(findThing("widget"), missingThing=Error), "the widget")
expect_silent(reportAs(findThing("widget"), missingThing=Error))

# Unrelated classes are left alone
expect_stdout(reportAs(findThing("sprocket"), somethingElse=Ignore), "WARNING: There is no sprocket", fixed=TRUE)

# Several classes can be remapped at once
probe <- function ()
{
    report(Warning, "no exe", class="missingExecutable")
    flag(Info, "old option", class="deprecatedOption")
    return("done")
}
expect_silent(reportAs(probe(), missingExecutable=Ignore, deprecatedOption=Ignore))
expect_equal(reportAs(probe(), missingExecutable=Ignore, deprecatedOption=Ignore), "done")

# A fallback error can be demoted, and its value is used
risky <- function () fallback("fallback", "cannot continue", class="riskyThing")
expect_error(risky(), "cannot continue")
expect_equal(reportAs(risky(), riskyThing=Warning), "fallback")
expect_stdout(reportAs(risky(), riskyThing=Warning), "WARNING: cannot continue", fixed=TRUE)
expect_equal(reportAs(risky(), riskyThing=Ignore), "fallback")

# Any other error has no value to continue with, so it can't be demoted and
# stays fatal
fatal <- function () report(Error, "truly fatal", class="fatalThing")
expect_error(reportAs(fatal(), fatalThing=Warning), "truly fatal")
expect_stdout(try(reportAs(fatal(), fatalThing=Warning), silent=TRUE), "cannot be demoted", fixed=TRUE)
asserting <- function () { assert(FALSE, "assumption failed", class="assumption"); "continued" }
expect_error(reportAs(asserting(), assumption=Ignore), "assumption failed")

# ... but an assertion signalled at a lower level can be escalated or ignored
lenient <- function () { assert(FALSE, "assumption failed", class="assumption", level=Warning); "continued" }
expect_error(reportAs(lenient(), assumption=Error), "assumption failed")
expect_equal(reportAs(lenient(), assumption=Ignore), "continued")

# Escalation is always allowed
expect_error(reportAs(probe(), missingExecutable=Error), "no exe")

# Named arguments may be handler functions rather than levels
expect_equal(reportAs(risky(), riskyThing=function(cond) invokeRestart("useValue","substituted")), "substituted")
expect_silent(reportAs(findThing("sprocket"), missingThing=function(cond) invokeRestart("muffleReport")))

# Remapping nests
expect_error(reportAs(reportAs(findThing("sprocket"), missingThing=Info), missingThing=Error))

# A remapped fallback stays recoverable, so an outer caller can demote it again
# or supply a value of its own
expect_equal(reportAs(reportAs(risky(), riskyThing=Fatal), riskyThing=Ignore), "fallback")
expect_equal(reportAs(reportAs(risky(), riskyThing=Warning), riskyThing=function(cond) invokeRestart("useValue","outer")), "outer")

# Conditions to remap must be named
expect_error(reportAs(findThing("widget"), Error), "must all be named")
