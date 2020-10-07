context("melt")

test_that("Default", {
    for (object in list(
        "matrix" = mat,
        "DataFrame" = df
    )) {
        x <- melt(object)
        expect_s4_class(x, "DataFrame")
    }
})

test_that("Per row filtering", {
    object <- mat
    gene <- rownames(object)[[2L]]
    object[gene, ] <- 0L
    x <- melt(object, min = 1L, minMethod = "perRow")
    expect_false(any(gene %in% x[["value"]]))
})

test_that("trans", {
    mapply(
        trans = eval(formals(`melt,matrix`)[["trans"]]),
        expected = list(
            "identity" = c(1, 5, 9, 13, 2, 6),  # nolint
            "log2" = c(1.000, 2.585, 3.322, 3.807, 1.585, 2.807),
            "log10" = c(0.301, 0.778, 1.000, 1.146, 0.477, 0.845)
        ),
        FUN = function(trans, expected) {
            object <- melt(
                object = mat,
                min = 1L,
                minMethod = "perRow",
                trans = trans
            )
            expect_s4_class(object, "DataFrame")
            object <- round(head(object[["value"]]), digits = 3L)
            expect_identical(object, expected)
        },
        SIMPLIFY = FALSE
    )
})
