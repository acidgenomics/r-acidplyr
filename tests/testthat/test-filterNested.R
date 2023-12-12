test_that("DFrame", {
    pattern <- "^[C]+$"
    object <- DataFrame(
        "V1" = c("a", "b", "c"),
        "V2" = I(list(
            c("aa", "bb", "CC"),
            c("dd", "ee", "ff"),
            c("gg", "hh", "ii")
        ))
    )
    expect_identical(
        object = filterNested(
            object = object,
            pattern = pattern,
            ignoreCase = FALSE
        ),
        expected = DataFrame(
            "V1" = "a",
            "V2" = I(list(c("aa", "bb", "CC")))
        )
    )
    expect_identical(
        object = filterNested(
            object = object,
            pattern = pattern,
            ignoreCase = TRUE
        ),
        expected = DataFrame(
            "V1" = c("a", "c"),
            "V2" = I(list(
                c("aa", "bb", "CC"),
                c("gg", "hh", "ii")
            ))
        )
    )
})

test_that("DFrame : no matches", {
    expect_error(
        object = filterNested(
            object = DataFrame(),
            pattern = "a"
        ),
        regexp = "Failed to match pattern"
    )
})
