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
#' object <- as.DataFrame(list(
#'     "col1" = CharacterList(
#'         c("a", "b", "c", "d"),
#'         c("e", "f", "g"),
#'         c("h", "i")
#'     ),
#'     "col2" = IntegerList(
#'         seq(from = 1L, to = 2L),
#'         seq(from = 3L, to = 4L),
#'         seq(from = 5L, to = 6L)
#'     ),
#'     "col3" = c("a", "b", "c")
#' ))
#' print(object)
#' x <- unnest2(object, col = "col1")
#' print(x)
#' y <- unnest2(object, col = "col2")
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
                    rownames(df) <- seq_along(df)
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
        df
    }



#' @rdname unlist2
#' @export
setMethod(
    f = "unlist2",
    signature = signature(x = "DFrameList"),
    definition = `unlist2,DFrameList`
)
