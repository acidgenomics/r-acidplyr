test_that("DFrameList : everything named", {
    dfl <- DataFrameList(
        "A" = DataFrame(
            "a" = c(1L, 2L),
            "b" = c(3L, 4L),
            row.names = c("aa", "bb")
        ),
        "B" = DataFrame(
            "a" = 5L,
            "b" = 6L,
            row.names = "cc"
        )
    )
    expect_identical(
        object = unlist2(dfl),
        expected = DataFrame(
            "name" = c("A", "A", "B"),
            "rowname" = c("aa", "bb", "cc"),
            "a" = c(1L, 2L, 5L),
            "b" = c(3L, 4L, 6L)
        )
    )
    expect_identical(
        object = unlist(dfl),
        expected = DataFrame(
            "a" = c(1L, 2L, 5L),
            "b" = c(3L, 4L, 6L),
            row.names = c("A.aa", "A.bb", "B.cc")
        )
    )
})

test_that("DFrameList : only colnames", {
    dfl <- DataFrameList(
        DataFrame(
            "a" = c(1L, 2L),
            "b" = c(3L, 4L)
        ),
        DataFrame(
            "a" = 5L,
            "b" = 6L
        )
    )
    expect_identical(
        object = unlist2(dfl),
        expected = DataFrame(
            "name" = c(1L, 1L, 2L),
            "rowname" = c(1L, 2L, 1L),
            "a" = c(1L, 2L, 5L),
            "b" = c(3L, 4L, 6L)
        )
    )
    expect_identical(
        object = unlist(dfl),
        expected = DataFrame(
            "a" = c(1L, 2L, 5L),
            "b" = c(3L, 4L, 6L)
        )
    )
})
