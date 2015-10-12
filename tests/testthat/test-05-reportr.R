context("Message reporting and output levels")

test_that("message reporting works", {
    library(ore)
    
    options(reportrStderrLevel=OL$Error)
    
    options(reportrOutputLevel=OL$Debug)
    expect_equal(getOutputLevel(), OL$Debug)
    setOutputLevel(OL$Info)
    expect_equal(getOutputLevel(), OL$Info)
    
    setOutputLevel(OL$Warning)
    expect_output(report(OL$Info,"Test message"), "", fixed=TRUE)
    
    setOutputLevel(OL$Info)
    expect_output(report(OL$Info,"Test message"), "Test message", fixed=TRUE)
    expect_output(report(Info,"Test message"), "Test message", fixed=TRUE)
    expect_output(report("Info","Test message"), "Test message", fixed=TRUE)

    flag(OL$Warning, "Test warning")
    flag(OL$Warning, "Test warning")
    expect_output(reportFlags(), "[x2]", fixed=TRUE)
    
    expect_warning(sqrt(-1), "NaNs produced", fixed=TRUE)
    expect_output({
        withReportrHandlers(sqrt(-1))
        reportFlags()
    }, "NaNs produced", fixed=TRUE)
})
