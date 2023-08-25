data(join, package = "AcidTest", envir = environment())

x <- as(join[["members"]], "DFrame")
y <- as(join[["instruments"]], "DFrame")
rownames(x) <- x[["name"]]
rownames(y) <- y[["name"]]
by <- "name"

test_that("antiJoin", {
    expect_identical(
        object = antiJoin(x = x, y = y, by = by),
        expected = DataFrame(
            "name" = "Mick",
            "band" = "Stones",
            row.names = "Mick"
        )
    )
})

test_that("antiJoin : Duplicate and NA values in 'by'", {
    x2 <- x
    x2[[by]][[2L]] <- x2[[by]][[1L]]
    expect_error(
        object = antiJoin(x = x2, y = y, by = by),
        regexp = "not unique"
    )
    x2 <- x
    x2[[by]][[1L]] <- NA
    expect_error(
        object = antiJoin(x = x2, y = y, by = by),
        regexp = "NA"
    )
    y2 <- y
    y2[[by]][[2L]] <- y2[[by]][[1L]]
    expect_error(
        object = antiJoin(x = x, y = y2, by = by),
        regexp = "not unique"
    )
    y2 <- y
    y2[[by]][[1L]] <- NA
    expect_error(
        object = antiJoin(x = x, y = y2, by = by),
        regexp = "NA"
    )
})

test_that("fullJoin", {
    expect_identical(
        object = fullJoin(x = x, y = y, by = by),
        expected = DataFrame(
            "name" = c("Mick", "John", "Paul", "Keith"),
            "band" = c("Stones", "Beatles", "Beatles", NA),
            "plays" = c(NA, "guitar", "bass", "guitar"),
            row.names = c("Mick", "John", "Paul", "Keith")
        )
    )
})

test_that("fullJoin : Duplicate and NA values in 'by'", {
    x2 <- x
    x2[[by]][[2L]] <- x2[[by]][[1L]]
    expect_error(
        object = fullJoin(x = x2, y = y, by = by),
        regexp = "not unique"
    )
    x2 <- x
    x2[[by]][[1L]] <- NA
    expect_error(
        object = fullJoin(x = x2, y = y, by = by),
        regexp = "NA"
    )
    y2 <- y
    y2[[by]][[2L]] <- y2[[by]][[1L]]
    expect_error(
        object = fullJoin(x = x, y = y2, by = by),
        regexp = "not unique"
    )
    y2 <- y
    y2[[by]][[1L]] <- NA
    expect_error(
        object = fullJoin(x = x, y = y2, by = by),
        regexp = "NA"
    )
})

test_that("innerJoin", {
    expect_identical(
        object = innerJoin(x = x, y = y, by = by),
        expected = DataFrame(
            "name" = c("John", "Paul"),
            "band" = c("Beatles", "Beatles"),
            "plays" = c("guitar", "bass"),
            row.names = c("John", "Paul")
        )
    )
})

test_that("innerJoin : Duplicate and NA values in 'by'", {
    x2 <- x
    x2[[by]][[2L]] <- x2[[by]][[1L]]
    expect_error(
        object = innerJoin(x = x2, y = y, by = by),
        regexp = "not unique"
    )
    x2 <- x
    x2[[by]][[1L]] <- NA
    expect_error(
        object = innerJoin(x = x2, y = y, by = by),
        regexp = "NA"
    )
    y2 <- y
    y2[[by]][[2L]] <- y2[[by]][[1L]]
    expect_error(
        object = innerJoin(x = x, y = y2, by = by),
        regexp = "not unique"
    )
    y2 <- y
    y2[[by]][[1L]] <- NA
    expect_error(
        object = innerJoin(x = x, y = y2, by = by),
        regexp = "NA"
    )
})

test_that("leftJoin", {
    expect_identical(
        object = leftJoin(x = x, y = y, by = by),
        expected = DataFrame(
            "name" = c("Mick", "John", "Paul"),
            "band" = c("Stones", "Beatles", "Beatles"),
            "plays" = c(NA, "guitar", "bass"),
            row.names = c("Mick", "John", "Paul")
        )
    )
})

test_that("leftJoin : Duplicate and NA values in 'by'", {
    x2 <- x
    x2[[by]][[2L]] <- x2[[by]][[1L]]
    expect_identical(
        object = leftJoin(x = x2, y = y, by = by),
        expected = DataFrame(
            "name" = c("Mick", "Mick", "Paul"),
            "band" = c("Stones", "Beatles", "Beatles"),
            "plays" = c(NA, NA, "bass"),
            row.names = c("Mick", "John", "Paul")
        )
    )
    x2 <- x
    x2[[by]] <- NA
    expect_error(
        object = leftJoin(x = x2, y = y, by = by),
        regexp = "NA"
    )
    x2 <- x
    x2[[by]][[1L]] <- NA
    expect_identical(
        object = leftJoin(x = x2, y = y, by = by),
        expected = DataFrame(
            "name" = c(NA, "John", "Paul"),
            "band" = c("Stones", "Beatles", "Beatles"),
            "plays" = c(NA, "guitar", "bass"),
            row.names = c("Mick", "John", "Paul")
        )
    )
    y2 <- y
    y2[[by]][[2L]] <- y2[[by]][[1L]]
    expect_error(
        object = leftJoin(x = x, y = y2, by = by),
        regexp = "not unique"
    )
    y2 <- y
    y2[[by]][[1L]] <- NA
    expect_error(
        object = leftJoin(x = x, y = y2, by = by),
        regexp = "NA"
    )
})

test_that("rightJoin", {
    expect_identical(
        object = rightJoin(x = x, y = y, by = by),
        expected = DataFrame(
            "name" = c("John", "Paul", "Keith"),
            "plays" = c("guitar", "bass", "guitar"),
            "band" = c("Beatles", "Beatles", NA),
            row.names = c("John", "Paul", "Keith")
        )
    )
})

## FIXME Need to double check our not unique and NA handling here.
test_that("rightJoin : Duplicate and NA values in 'by'", {
    x2 <- x
    x2[[by]][[2L]] <- x2[[by]][[1L]]
    expect_error(
        object = rightJoin(x = x2, y = y, by = by),
        regexp = "not unique"
    )
    x2 <- x
    x2[[by]][[1L]] <- NA
    expect_error(
        object = rightJoin(x = x2, y = y, by = by),
        regexp = "NA"
    )
    y2 <- y
    y2[[by]][[2L]] <- y2[[by]][[1L]]
    expect_identical(
        object = rightJoin(x = x, y = y2, by = by),
        expected = DataFrame(
            "name" = c("John", "John", "Keith"),
            "plays" = c("guitar", "bass", "guitar"),
            "band" = c("Beatles", "Beatles", NA),
            row.names = c("John", "Paul", "Keith")
        )
    )
    y2 <- y
    y2[[by]][[1L]] <- NA
    expect_error(
        object = rightJoin(x = x, y = y2, by = by),
        regexp = "NA"
    )
})

test_that("semiJoin", {
    expect_identical(
        object = semiJoin(x = x, y = y, by = by),
        expected = DataFrame(
            "name" = c("John", "Paul"),
            "band" = c("Beatles", "Beatles"),
            row.names = c("John", "Paul")
        )
    )
})

test_that("semiJoin : Duplicate and NA values in 'by'", {
    x2 <- x
    x2[[by]][[2L]] <- x2[[by]][[1L]]
    expect_error(
        object = semiJoin(x = x2, y = y, by = by),
        regexp = "not unique"
    )
    x2 <- x
    x2[[by]][[1L]] <- NA
    expect_error(
        object = semiJoin(x = x2, y = y, by = by),
        regexp = "NA"
    )
    y2 <- y
    y2[[by]][[2L]] <- y2[[by]][[1L]]
    expect_error(
        object = semiJoin(x = x, y = y2, by = by),
        regexp = "not unique"
    )
    y2 <- y
    y2[[by]][[1L]] <- NA
    expect_error(
        object = semiJoin(x = x, y = y2, by = by),
        regexp = "NA"
    )
})

test_that("Matched rows", {
    df1 <- DataFrame(
        "id" = as.factor(seq(4L)),
        "genotype" = as.factor(rep(x = c("wt", "ko"), each = 2L))
    )
    df2 <- DataFrame(
        "id" = as.factor(seq(4L)),
        "treatment" = as.factor(rep(x = c("control", "expt"), times = 2L))
    )
    expect_identical(
        object = leftJoin(df1, df2, by = "id"),
        expected = DataFrame(
            "id" = as.factor(seq(4L)),
            "genotype" = as.factor(rep(x = c("wt", "ko"), each = 2L)),
            "treatment" = as.factor(rep(x = c("control", "expt"), times = 2L))
        )
    )
})

test_that("Unmatched rows", {
    df1 <- DataFrame(
        "id" = as.factor(seq(4L)),
        "genotype" = as.factor(rep(x = c("wt", "ko"), each = 2L))
    )
    df2 <- DataFrame(
        "id" = as.factor(seq(4L)),
        "treatment" = as.factor(rep(x = c("control", "expt"), times = 2L))
    )
    ## Reverse the row order of df2.
    df2 <- df2[rev(seq_len(nrow(df2))), ]
    expect_identical(
        object = leftJoin(df1, df2, by = "id"),
        expected = DataFrame(
            "id" = as.factor(seq(4L)),
            "genotype" = as.factor(rep(x = c("wt", "ko"), each = 2L)),
            "treatment" = as.factor(rep(x = c("control", "expt"), times = 2L))
        )
    )
})

test_that("Uneven rows", {
    df1 <- DataFrame(
        "id" = as.factor(seq(4L)),
        "genotype" = as.factor(rep(x = c("wt", "ko"), each = 2L))
    )
    df2 <- DataFrame(
        "id" = as.factor(seq(2L)),
        "treatment" = as.factor(rep(x = c("control", "expt"), times = 1L))
    )
    expect_identical(
        object = leftJoin(df1, df2, by = "id"),
        expected = DataFrame(
            "id" = as.factor(seq(4L)),
            "genotype" = as.factor(rep(x = c("wt", "ko"), each = 2L)),
            "treatment" = as.factor(c("control", "expt", NA, NA))
        )
    )
})
