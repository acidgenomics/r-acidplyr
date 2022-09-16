test_that("DataFrame", {
    long <- melt(mat)
    wide <- cast(long)
    expect_identical(as.matrix(wide), mat)
})
