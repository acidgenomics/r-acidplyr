context("unlistToDataFrame")

IntegerList <- AcidGenerics::IntegerList

object1 <- list(
    "a" = list(
        "aa" = seq(from = 1L, to = 3L),
        "bb" = seq(from = 4L, to = 6L)
    ),
    "b" = list(
        "cc" = seq(from = 7L, to = 9L),
        "dd" = seq(from = 10L, to = 12L)
    ),
    "c" = list(
        "ee" = seq(from = 13L, to = 15L),
        "ff" = seq(from = 16L, to = 18L)
    )
)

object2 <- list(
    "a" = IntegerList(
        "aa" = seq(from = 1L, to = 3L),
        "bb" = seq(from = 4L, to = 6L)
    ),
    "b" = IntegerList(
        "cc" = seq(from = 7L, to = 9L),
        "dd" = seq(from = 10L, to = 12L)
    ),
    "c" = IntegerList(
        "ee" = seq(from = 13L, to = 15L),
        "ff" = seq(from = 16L, to = 18L)
    )
)

test_that("list : no S4, recursive (purrr)", {
    object <- unlistToDataFrame(object1, recursive = TRUE)
    expected <- DataFrame(
        "aa" = c(
            seq(from = 1L, to = 3L),
            rep(NA, times = 6L)
        ),
        "bb" = c(
            seq(from = 4L, to = 6L),
            rep(NA, times = 6L)
        ),
        "cc" = c(
            rep(NA, times = 3L),
            seq(from = 7L, to = 9L),
            rep(NA, times = 3L)
        ),
        "dd" = c(
            rep(NA, times = 3L),
            seq(from = 10L, to = 12L),
            rep(NA, times = 3L)
        ),
        "ee" = c(
            rep(NA, times = 6L),
            seq(from = 13L, to = 15L)
        ),
        "ff" = c(
            rep(NA, times = 6L),
            seq(from = 16L, to = 18L)
        )
    )
    expect_identical(object, expected)
})

test_that("list : no S4, non-recursive (I)", {
    object <- unlistToDataFrame(object1, recursive = FALSE)
    expected <- DataFrame(
        "a" = list(
            "aa" = seq(from = 1L, to = 3L),
            "bb" = seq(from = 4L, to = 6L)
        ),
        "b" = list(
            "cc" = seq(from = 7L, to = 9L),
            "dd" = seq(from = 10L, to = 12L)
        ),
        "c" = list(
            "ee" = seq(from = 13L, to = 15L),
            "ff" = seq(from = 16L, to = 18L)
        )
    )
    expect_identical(object, expected)
})
