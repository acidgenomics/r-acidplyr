context("collapseToString")

groceries <- c(NA, NA, "milk", "eggs", "eggs", "veggies")
mpgString <- "18.1, 18.7, 21, 21.4, 22.8"

test_that("atomic", {
    expect_identical(
        object = collapseToString(
            object = groceries,
            sort = FALSE,
            unique = FALSE
        ),
        expected = "NA, NA, milk, eggs, eggs, veggies"
    )
    expect_identical(
        object = collapseToString(
            object = groceries,
            sort = TRUE,
            unique = FALSE
        ),
        expected = "eggs, eggs, milk, veggies, NA, NA"
    )
    expect_identical(
        object = collapseToString(
            object = groceries,
            sort = TRUE,
            unique = TRUE
        ),
        expected = "eggs, milk, veggies, NA"
    )
})

test_that("data.frame", {
    x <- head(datasets::mtcars)
    x <- collapseToString(x, sort = TRUE, unique = TRUE)
    x <- x[["mpg"]]
    expect_identical(object = x, expected = mpgString)
})

test_that("DataFrame", {
    x <- head(as(mtcars, "DataFrame"))
    x <- collapseToString(x, sort = TRUE, unique = TRUE)
    x <- x[["mpg"]]
    expect_identical(object = x, expected = mpgString)
})

test_that("integer", {
    expect_identical(
        object = collapseToString(seq_len(5L)),
        expected = "1, 2, 3, 4, 5"
    )
})

test_that("logical", {
    expect_identical(
        object = collapseToString(c(TRUE, FALSE), sort = TRUE),
        expected = "FALSE, TRUE"
    )
    ## NA and NaN should stay fixed even when sorting is enabled
    expect_identical(
        object = collapseToString(c(NA, NaN), sort = TRUE),
        expected = "NA, NaN"
    )
    expect_identical(
        object = collapseToString(c(NaN, NA), sort = TRUE),
        expected = "NaN, NA"
    )
})

test_that("matrix", {
    x <- head(as(mtcars, "matrix"))
    x <- collapseToString(x, sort = TRUE, unique = TRUE)
    x <- x[1L, "mpg", drop = TRUE]
    expect_identical(object = unname(x), expected = mpgString)
})

test_that("numeric", {
    expect_identical(
        object = collapseToString(c(3.141593, 6.0221409e+23)),
        expected = "3.141593, 6.0221409e+23"
    )
})

test_that("scalar early return", {
    expect_identical(collapseToString(1L), 1L)
})
