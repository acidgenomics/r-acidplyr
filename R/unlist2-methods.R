#' @name unlist2
#' @inherit AcidGenerics::unlist2
#' @note Updated 2023-11-30.
#'
#' @param ... Additional arguments.
#'
#' @param nameCol `character(1)`.
#' Column name to assign `names` of input object.
#'
#' @param rownameCol `character(1)`.
#' Column name to assign `rownames` of input object.
#'
#' @examples
#' ## DFrameList ====
#' suppressPackageStartupMessages({
#'     library(IRanges)
#' })
#' dfl <- DataFrameList(
#'     "A" = DataFrame(
#'         "a" = c(1L, 2L),
#'         "b" = c(3L, 4L),
#'         row.names = c("aa", "bb")
#'     ),
#'     "B" = DataFrame(
#'         "a" = 5L,
#'         "b" = 6L,
#'         row.names = "cc"
#'     )
#' )
#' ## Our variant, which assigns into columns.
#' x <- unlist2(dfl)
#' print(x)
#' ## Compare with the standard variant, which modifies rownames.
#' y <- unlist(dfl)
#' print(y)
NULL



## Updated 2023-11-30.
`unlist2,DFrameList` <- # nolint
    function(
        x,
        nameCol = "name",
        rownameCol = "rowname"
    ) {
        if (!hasLength(x)) {
            return(DataFrame())
        }
        if (!hasNames(x)) {
            names(x) <- seq_along(x)
        }
        lst <- Map(
            name = names(x),
            df = x,
            MoreArgs = list(
                "nameCol" = nameCol,
                "rownameCol" = rownameCol
            ),
            f = function(name, df, nameCol, rownameCol) {
                if (!hasRows(df)) {
                    return(DataFrame())
                }
                assert(
                    hasColnames(df),
                    areDisjointSets(c(nameCol, rownameCol), colnames(df))
                )
                if (!hasRownames(df)) {
                    rownames(df) <- seq_len(nrow(df))
                }
                df[[nameCol]] <- name
                df[[rownameCol]] <- rownames(df)
                j <- c(
                    c(nameCol, rownameCol),
                    setdiff(colnames(df), c(nameCol, rownameCol))
                )
                df <- df[, j, drop = FALSE]
                rownames(df) <- NULL
                df
            }
        )
        df <- do.call(what = rbind, args = lst)
        if (allAreMatchingRegex(x = df[[nameCol]], pattern = "^[0-9]+$")) {
            df[[nameCol]] <- as.integer(df[[nameCol]])
        }
        if (allAreMatchingRegex(x = df[[rownameCol]], pattern = "^[0-9]+$")) {
            df[[rownameCol]] <- as.integer(df[[rownameCol]])
        }
        df
    }



#' @rdname unlist2
#' @export
setMethod(
    f = "unlist2",
    signature = signature(x = "DFrameList"),
    definition = `unlist2,DFrameList`
)
