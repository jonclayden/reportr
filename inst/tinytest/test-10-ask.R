setOutputLevel(OL$Info)

if (at_home())
{
    # Monkey-patch the masked functions to simulate interactivity
    ns <- getNamespace("reportr")
    unlockBinding(".interactive", ns)
    unlockBinding(".stdinIsTerminal", ns)
    assign(".interactive", function() FALSE, envir=ns)
    assign(".stdinIsTerminal", function() FALSE, envir=ns)

    expect_false(reportr:::.interactive())
    expect_null(ask("This question will not be answered"))
    expect_equal(ask("This question will be answered with 'y'",default="y"), "y")

    assign(".interactive", function() TRUE, envir=ns)
    unlockBinding(".readline", ns)
    assign(".readline", function(...) "y", envir=ns)

    expect_true(reportr:::.interactive())
    expect_equal(ask("This question will be answered with 'y'"), "y")
    expect_equal(ask("This question will be answered with 'Y'",valid=c("Y","N")), "Y")

    # A non-interactive session whose standard input is a terminal, as when a
    # script is run with Rscript, reads from it directly
    assign(".interactive", function() FALSE, envir=ns)
    assign(".stdinIsTerminal", function() TRUE, envir=ns)
    unlockBinding(".readStdin", ns)
    answers <- c("maybe", "n")
    prompts <- NULL
    assign(".readStdin", function (prompt) {
        prompts <<- c(prompts, prompt)
        if (length(answers) == 0)
            return (character(0))
        answer <- answers[1]
        answers <<- answers[-1]
        return (answer)
    }, envir=ns)

    expect_equal(ask("Continue?", valid=c("y","n"), prefix=FALSE), "n")
    expect_equal(prompts, c("Continue? ", "Continue? "))

    # The end of input gives the default, rather than asking forever
    expect_equal(ask("Continue?", default="y"), "y")
    expect_equal(ask("Continue?", default="y", valid=c("y","n")), "y")

    # The option turns this off, so that scripts don't wait for input
    answers <- "n"
    options(reportrAskInScripts=FALSE)
    expect_equal(ask("Continue?", default="y"), "y")
    expect_equal(answers, "n")
    options(reportrAskInScripts=NULL)
    expect_equal(ask("Continue?", default="y"), "n")

    # The output level still applies
    answers <- "n"
    setOutputLevel(OL$Error)
    expect_equal(ask("Continue?", default="y"), "y")
    setOutputLevel(OL$Info)
}
