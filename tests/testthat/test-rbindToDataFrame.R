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
    object <- list()
    expected <- list()
    ## This checks for unwanted coercion of top-level S4 elements.
    object[["IntegerList"]] <- list(
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
    expected[["IntegerList"]] <- DataFrame(
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
    object[["list"]] <- list(
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
    expected[["list"]] <- list(
        "aa" = list(
            seq(from = 1L, to = 3L),
            NULL,
            NULL
        ),
        "bb" = list(
            seq(from = 4L, to = 6L),
            NULL,
            NULL
        ),
        "cc" = list(
            NULL,
            seq(from = 7L, to = 9L),
            NULL
        ),
        "dd" = list(
            NULL,
            seq(from = 10L, to = 12L),
            NULL
        ),
        "ee" = list(
            NULL,
            NULL,
            seq(from = 13L, to = 15L)
        ),
        "ff" = list(
            NULL,
            NULL,
            seq(from = 16L, to = 18L)
        )
    )
    expected[["list"]] <-
        as(do.call(what = cbind, args = expected[["list"]]), "DFrame")
    rownames(expected[["list"]]) <- names(object[["list"]])
    for (i in seq_along(object)) {
        expect_identical(
            object = rbindToDataFrame(object[[!!i]]),
            expected = expected[[!!i]]
        )
    }
})

test_that("List nested down 3 levels", {
    object <- list()
    expected <- list()
    object[["IntegerList"]] <- list(
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
    expected[["IntegerList"]] <- DataFrame(
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
    object[["list"]] <- list(
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
    expected[["list"]] <- list(
        "aa1" = list(
            list(
                "aaa1" = seq(from = 1L, to = 3L),
                "aaa2" = seq(from = 4L, to = 6L)
            ),
            NULL
        ),
        "aa2" = list(
            list(
                "aaa3" = seq(from = 7L, to = 9L),
                "aaa4" = seq(from = 10L, to = 12L)
            ),
            NULL
        ),
        "bb1" = list(
            NULL,
            list(
                "bbb1" = seq(from = 13L, to = 15L),
                "bbb2" = seq(from = 16L, to = 18L)
            )
        ),
        "bb2" = list(
            NULL,
            list(
                "bbb3" = seq(from = 19L, to = 21L),
                "bbb4" = seq(from = 22L, to = 24L)
            )
        )
    )
    expected[["list"]] <-
        as(do.call(what = cbind, args = expected[["list"]]), "DFrame")
    rownames(expected[["list"]]) <- names(object[["list"]])
    for (i in seq_along(object)) {
        expect_identical(
            object = rbindToDataFrame(object[[!!i]]),
            expected = expected[[!!i]]
        )
    }
})
