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

## nolint start
DataFrame <- S4Vectors::DataFrame
hasRownames <- goalie::hasRownames
## nolint end
