test_that("DFrame", {
    long <- melt(mat)
    wide <- cast(long)
    expect_identical(as.matrix(wide), mat)
})
