## nolint start

data <- utils::data

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

CharacterList <- IRanges::CharacterList
DataFrame <- S4Vectors::DataFrame
DataFrameList <- IRanges::DataFrameList
IntegerList <- IRanges::IntegerList
as.DataFrame <- pipette::as.DataFrame
hasRownames <- goalie::hasRownames

## nolint end
