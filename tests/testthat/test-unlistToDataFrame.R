context("unlistToDataFrame")

test_that("Column bind listed vectors into DataFrame", {
    x <- list(
        "aaa" = seq(from = 1L, to = 3L),
        "bbb" = seq(from = 4L, to = 6L)
    )
    expected <- DataFrame(
        "aaa" = seq(from = 1L, to = 3L),
        "bbb" = seq(from = 4L, to = 6L)
    )
    expect_identical(
        object = unlistToDataFrame(x),
        expected = expected
    )
})

IntegerList <- AcidGenerics::IntegerList  # nolint

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

object3 <- list(
    "a" = list(
        "aa1" = list(
            "aaa1" = seq(from = 1L, to = 3L),
            "aaa2" = seq(from = 4L, to = 6L)
        ),
        "aa2" = list(
            "aaa3" = seq(from = 7L, to = 9L),
            "aaa4" = seq(from = 10L, to = 12L)
        )
    ),
    "b" = list(
        "bb1" = list(
            "bbb1" = seq(from = 13L, to = 15L),
            "bbb2" = seq(from = 16L, to = 18L)
        ),
        "bb2" = list(
            "bbb3" = seq(from = 19L, to = 21L),
            "bbb4" = seq(from = 22L, to = 24L)
        )
    )
)

object4 <- list(
    "a" = list(
        "aa1" = IntegerList(
            "aaa1" = seq(from = 1L, to = 3L),
            "aaa2" = seq(from = 4L, to = 6L)
        ),
        "aa2" = IntegerList(
            "aaa3" = seq(from = 7L, to = 9L),
            "aaa4" = seq(from = 10L, to = 12L)
        )
    ),
    "b" = list(
        "bb1" = IntegerList(
            "bbb1" = seq(from = 13L, to = 15L),
            "bbb2" = seq(from = 16L, to = 18L)
        ),
        "bb2" = IntegerList(
            "bbb3" = seq(from = 19L, to = 21L),
            "bbb4" = seq(from = 22L, to = 24L)
        )
    )
)

expected1 <- DataFrame(
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

expected2 <- DataFrame(
    "aa1.aaa1" = c(
        seq(from = 1L, to = 3L),
        rep(NA, times = 3L)
    ),
    "aa1.aaa2" = c(
        seq(from = 4L, to = 6L),
        rep(NA, times = 3L)
    ),
    "aa2.aaa3" = c(
        seq(from = 7L, to = 9L),
        rep(NA, times = 3L)
    ),
    "aa2.aaa4" = c(
        seq(from = 10L, to = 12L),
        rep(NA, times = 3L)
    ),
    "bb1.bbb1" = c(
        rep(NA, times = 3L),
        seq(from = 13L, to = 15L)
    ),
    "bb1.bbb2" = c(
        rep(NA, times = 3L),
        seq(from = 16L, to = 18L)
    ),
    "bb2.bbb3" = c(
        rep(NA, times = 3L),
        seq(from = 19L, to = 21L)
    ),
    "bb2.bbb4" = c(
        rep(NA, times = 3L),
        seq(from = 22L, to = 24L)
    )
)

expected3 <- DataFrame(
    "a" = I(SimpleList(I(list(
        "aa" = seq(from = 1L, to = 3L),
        "bb" = seq(from = 4L, to = 6L)
    )))),
    "b" = I(SimpleList(I(list(
        "cc" = seq(from = 7L, to = 9L),
        "dd" = seq(from = 10L, to = 12L)
    )))),
    "c" = I(SimpleList(I(list(
        "ee" = seq(from = 13L, to = 15L),
        "ff" = seq(from = 16L, to = 18L)
    ))))
)

test_that("list : recursive with `purrr::map_dfr()`", {
    for (object in list(object1, object2)) {
        expect_identical(
            object = unlistToDataFrame(!!object, recursive = TRUE),
            expected = expected1
        )
    }
    for (object in list(object3, object4)) {
        expect_identical(
            object = unlistToDataFrame(!!object, recursive = TRUE),
            expected = expected2
        )
    }
})

test_that("list : non-recursive with `I()`", {
    for (object in list(object1, object2)) {
        expect_identical(
            object = unlistToDataFrame(!!object, recursive = TRUE),
            expected = expected1
        )
    }
    for (object in list(object3, object4)) {
        expect_identical(
            object = unlistToDataFrame(!!object, recursive = TRUE),
            expected = expected2
        )
    }
})
