setOutputLevel(OL$Info)

if (at_home())
{
    # Monkey-patch the masked functions to simulate interactivity
    ns <- getNamespace("reportr")
    unlockBinding(".interactive", ns)
    assign(".interactive", function() FALSE, envir=ns)
    
    expect_false(reportr:::.interactive())
    expect_null(ask("This question will not be answered"))
    expect_equal(ask("This question will be answered with 'y'",default="y"), "y")
    
    assign(".interactive", function() TRUE, envir=ns)
    unlockBinding(".readline", ns)
    assign(".readline", function(...) "y", envir=ns)
    
    expect_true(reportr:::.interactive())
    expect_equal(ask("This question will be answered with 'y'"), "y")
    expect_equal(ask("This question will be answered with 'Y'",valid=c("Y","N")), "Y")
}
