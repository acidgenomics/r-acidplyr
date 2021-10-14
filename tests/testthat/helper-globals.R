## nolint start

data(
    DFrame, matrix,  # FIXME
    package = "AcidTest",
    envir = environment()
)
data(
    mtcars,
    package = "datasets",
    envir = environment()
)

df <- DFrame  # FIXME
mat <- matrix

DataFrame <- AcidGenerics::DataFrame
hasRownames <- goalie::hasRownames

## nolint end
