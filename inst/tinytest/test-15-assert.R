setOutputLevel(OL$Info)

expect_null(assert(1 == 1, "Arithmetic failure"))
expect_stdout(assert(NA == NA, "NAs don't test equal to themselves", level=OL$Info), "NAs don't test equal to themselves")
# The case below produces an error
expect_stdout(assert(3 + "n", "Numeric and character vectors can't be added", level=Info), "Numeric and character vectors can't be added")
