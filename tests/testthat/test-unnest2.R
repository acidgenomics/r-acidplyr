test_that("DFrame", {
    object <- as.DataFrame(list(
        "col1" = CharacterList(
            c("a", "b", "c", "d"),
            c("e", "f", "g"),
            c("h", "i")
        ),
        "col2" = IntegerList(
            seq(from = 1L, to = 2L),
            seq(from = 3L, to = 4L),
            seq(from = 5L, to = 6L)
        ),
        "col3" = c("a", "b", "c")
    ))
    expect_identical(
        object = unnest2(object, col = "col1"),
        expected = as.DataFrame(list(
            "col1" = c(
                "a", "b", "c",
                "d", "e", "f",
                "g", "h", "i"
            ),
            "col2" = IntegerList(
                seq(from = 1L, to = 2L),
                seq(from = 1L, to = 2L),
                seq(from = 1L, to = 2L),
                seq(from = 1L, to = 2L),
                seq(from = 3L, to = 4L),
                seq(from = 3L, to = 4L),
                seq(from = 3L, to = 4L),
                seq(from = 5L, to = 6L),
                seq(from = 5L, to = 6L)
            ),
            "col3" = c(
                "a", "a", "a", "a",
                "b", "b", "b",
                "c", "c"
            )
        ))
    )
    expect_identical(
        object = unnest2(object, col = "col2"),
        expected = as.DataFrame(list(
            "col1" = CharacterList(
                c("a", "b", "c", "d"),
                c("a", "b", "c", "d"),
                c("e", "f", "g"),
                c("e", "f", "g"),
                c("h", "i"),
                c("h", "i")
            ),
            "col2" = seq(from = 1L, to = 6L),
            "col3" = rep(c("a", "b", "c"), each = 2L)
        ))
    )
})
