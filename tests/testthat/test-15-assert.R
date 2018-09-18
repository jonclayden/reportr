context("Assertions")

test_that("we can make assertions", {
    setOutputLevel(OL$Info)
    expect_null(assert(1 == 1, "Arithmetic failure"))
    expect_output(assert(NA == NA, "NAs don't test equal to themselves", level=OL$Info), "NAs don't test equal to themselves")
})
