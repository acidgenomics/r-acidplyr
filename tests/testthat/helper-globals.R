## nolint start

data(
    DataFrame, matrix,
    package = "AcidTest",
    envir = environment()
)
data(
    mtcars,
    package = "datasets",
    envir = environment()
)

df <- DataFrame
mat <- matrix

DataFrame <- S4Vectors::DataFrame
hasRownames <- goalie::hasRownames

## nolint end
