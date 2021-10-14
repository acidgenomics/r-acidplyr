context("mutate")

x <- as(mtcars, "DFrame")

test_that("mutateAll", {
    expect_identical(x[["gear"]][1L], 4)  # nolint
    x <- mutateAll(x, fun = log, base = 2L)
    expect_s4_class(x, "DFrame")
    ## Check that log2 calculation was applied correctly.
    expect_identical(x[["gear"]][1L], 2)  # nolint
    expect_true(hasRownames(x))
})

test_that("mutateAt", {
    x <- mutateAt(x, vars = c("mpg", "cyl"), log, base = 2L)
    expect_s4_class(x, "DFrame")
    expect_true(hasRownames(x))
})

test_that("mutateIf", {
    x <- mutateIf(x, predicate = is.double, fun = as.integer)
    expect_s4_class(x, "DFrame")
    expect_true(hasRownames(x))
})

test_that("transmuteAt", {
    x <- transmuteAt(x, vars = c("mpg", "cyl"), log, base = 2L)
    expect_s4_class(x, "DFrame")
    expect_true(hasRownames(x))
})

test_that("transmuteIf", {
    x <- transmuteIf(x, predicate = is.double, fun = as.integer)
    expect_s4_class(x, "DFrame")
    expect_true(hasRownames(x))
})
