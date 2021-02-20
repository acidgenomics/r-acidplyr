context("rbindToDataFrame")

test_that("Matched and named input", {
    x <- list(
        "aa" = c("a" = 1L, "b" = 2L),
        "bb" = c("a" = 3L, "b" = 4L)
    )
    object <- rbindToDataFrame(x)
    expected <- DataFrame(
        "a" = c(1L, 3L),
        "b" = c(2L, 4L),
        row.names = c("aa", "bb")
    )
    ## FIXME NAMES MISMATCH HERE.
    ## FIXME OBJECT NEEDS TO STRIP NAMES PER ROW....
    expect_identical(object, expected)
})

test_that("Mismatched dimensions", {
    x <- list(
        c(1L, 2L),
        c(3L, 4L, 5L)
    )
    rbindToDataFrame(x)

    to
})

test_that("NULL input", {
})
