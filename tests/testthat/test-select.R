context("select")

x <- as(mtcars, "DFrame")

test_that("selectIf", {
    x <- selectIf(x, predicate = is.double)
    expect_s4_class(x, "DFrame")
    expect_true(hasRownames(x))
})
