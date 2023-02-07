test_that("Matched input", {
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
    expect_identical(object, expected)
})

test_that("Unmatched input", {
    x <- list(
        "aa" = seq(from = 1L, to = 3L),
        "bb" = seq(from = 4L, to = 7L)
    )
    names(x[["aa"]]) <- c("a", "b", "c")
    names(x[["bb"]]) <- c("b", "c", "d", "e")
    object <- rbindToDataFrame(x)
    expected <- DataFrame(
        "a" = c(1L, NA_integer_),
        "b" = c(2L, 4L),
        "c" = c(3L, 5L),
        "d" = c(NA_integer_, 6L),
        "e" = c(NA_integer_, 7L),
        row.names = c("aa", "bb")
    )
    expect_identical(object, expected)
})

test_that("List nested down 2 levels", {
    object <- list(
        "SimpleList" = list(
            "a" = SimpleList(
                "aa" = seq(from = 1L, to = 3L),
                "bb" = seq(from = 4L, to = 6L)
            ),
            "b" = SimpleList(
                "cc" = seq(from = 7L, to = 9L),
                "dd" = seq(from = 10L, to = 12L)
            ),
            "c" = SimpleList(
                "ee" = seq(from = 13L, to = 15L),
                "ff" = seq(from = 16L, to = 18L)
            )
        ),
        "IntegerList" = list(
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
    )
    ## FIXME Not sure how to construct this to get checks to pass.
    expected <- list(
        "SimpleList" = DataFrame(
            "aa" = SimpleList(
                seq(from = 1L, to = 3L),
                NULL,
                NULL
            ),
            "bb" = SimpleList(
                seq(from = 4L, to = 6L),
                NULL,
                NULL
            ),
            "cc" = SimpleList(
                NULL,
                seq(from = 7L, to = 9L),
                NULL
            ),
            "dd" = SimpleList(
                NULL,
                seq(from = 10L, to = 12L),
                NULL
            ),
            "ee" = SimpleList(
                NULL,
                NULL,
                seq(from = 13L, to = 15L)
            ),
            "ff" = SimpleList(
                NULL,
                NULL,
                seq(from = 16L, to = 18L)
            ),
            row.names = c("a", "b", "c")
        ),
        "IntegerList" = DataFrame(
            "x1" = I(list(
                IntegerList(
                    "aa" = seq(from = 1L, to = 3L),
                    "bb" = seq(from = 4L, to = 6L)
                ),
                IntegerList(
                    "cc" = seq(from = 7L, to = 9L),
                    "dd" = seq(from = 10L, to = 12L)
                ),
                IntegerList(
                    "ee" = seq(from = 13L, to = 15L),
                    "ff" = seq(from = 16L, to = 18L)
                )
            )),
            row.names = c("a", "b", "c")
        )
    )
    for (i in seq_along(objects)) {
        expect_identical(
            object = rbindToDataFrame(object[[!!i]]),
            expected = expected[[!!i]]
        )
    }
})

test_that("List nested down 3 levels", {
    object <- list(
        "SimpleList" = list(
            "a" = list(
                "aa1" = SimpleList(
                    "aaa1" = seq(from = 1L, to = 3L),
                    "aaa2" = seq(from = 4L, to = 6L)
                ),
                "aa2" = SimpleList(
                    "aaa3" = seq(from = 7L, to = 9L),
                    "aaa4" = seq(from = 10L, to = 12L)
                )
            ),
            "b" = list(
                "bb1" = SimpleList(
                    "bbb1" = seq(from = 13L, to = 15L),
                    "bbb2" = seq(from = 16L, to = 18L)
                ),
                "bb2" = SimpleList(
                    "bbb3" = seq(from = 19L, to = 21L),
                    "bbb4" = seq(from = 22L, to = 24L)
                )
            )
        ),
        "IntegerList" = list(
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
    )
    expected <- list(
        "SimpleList" = DataFrame(
            "aa1" = I(list(
                SimpleList(
                    "aaa1" = seq(from = 1L, to = 3L),
                    "aaa2" = seq(from = 4L, to = 6L)
                ),
                NULL
            )),
            "aa2" = I(list(
                SimpleList(
                    "aaa3" = seq(from = 7L, to = 9L),
                    "aaa4" = seq(from = 10L, to = 12L)
                ),
                NULL
            )),
            "bb1" = I(list(
                NULL,
                SimpleList(
                    "bbb1" = seq(from = 13L, to = 15L),
                    "bbb2" = seq(from = 16L, to = 18L)
                )
            )),
            "bb2" = I(list(
                NULL,
                SimpleList(
                    "bbb3" = seq(from = 19L, to = 21L),
                    "bbb4" = seq(from = 22L, to = 24L)
                )
            )),
            row.names = c("a", "b")
        ),
        "IntegerList" = DataFrame(
            "aa1" = I(list(
                IntegerList(
                    "aaa1" = seq(from = 1L, to = 3L),
                    "aaa2" = seq(from = 4L, to = 6L)
                ),
                NULL
            )),
            "aa2" = I(list(
                IntegerList(
                    "aaa3" = seq(from = 7L, to = 9L),
                    "aaa4" = seq(from = 10L, to = 12L)
                ),
                NULL
            )),
            "bb1" = I(list(
                NULL,
                IntegerList(
                    "bbb1" = seq(from = 13L, to = 15L),
                    "bbb2" = seq(from = 16L, to = 18L)
                )
            )),
            "bb2" = I(list(
                NULL,
                IntegerList(
                    "bbb3" = seq(from = 19L, to = 21L),
                    "bbb4" = seq(from = 22L, to = 24L)
                )
            )),
            row.names = c("a", "b")
        )
    )
    for (i in seq_along(objects)) {
        expect_identical(
            object = rbindToDataFrame(object[[!!i]]),
            expected = expected[[!!i]]
        )
    }
})

test_that("Don't allow evaluation of top-level S4 elements", {
    x <- list(
        "a" = IntegerList(
            "aa" = seq(from = 1L, to = 3L),
            "bb" = seq(from = 4L, to = 6L)
        ),
        "b" = IntegerList(
            "cc" = seq(from = 7L, to = 9L),
            "dd" = seq(from = 10L, to = 12L)
        ),
        "c" = "hello"
    )
    object <- rbindToDataFrame(x)
    expected <- DataFrame(
        "x1" = I(list(
            IntegerList(
                "aa" = seq(from = 1L, to = 3L),
                "bb" = seq(from = 4L, to = 6L)
            ),
            IntegerList(
                "cc" = seq(from = 7L, to = 9L),
                "dd" = seq(from = 10L, to = 12L)
            ),
            "hello"
        )),
        row.names = c("a", "b", "c")
    )
    expect_identical(object, expected)
})
