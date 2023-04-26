## nolint start

data(
    DFrame, matrix,
    package = "AcidTest",
    envir = environment()
)
data(
    mtcars,
    package = "datasets",
    envir = environment()
)

df <- DFrame
mat <- matrix

DataFrame <- S4Vectors::DataFrame
IntegerList <- IRanges::IntegerList
hasRownames <- goalie::hasRownames

## nolint end
