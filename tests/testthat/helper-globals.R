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

DataFrame <- AcidGenerics::DataFrame
hasRownames <- goalie::hasRownames

## nolint end
