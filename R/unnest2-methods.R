#' @name unnest2
#' @inherit AcidGenerics::unnest2
#' @note Updated 2023-08-24.
#'
#' @param col `character(1)`.
#' Name of the list-column to unnest into long format.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' ## DFrame ====
#' suppressPackageStartupMessages({
#'     library(IRanges)
#'     library(pipette)
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



## Updated 2023-08-24.
`unnest2,DFrame` <- # nolint
    function(object, col) {
        assert(
            isString(col),
            isSubset(col, colnames(object)),
            hasRows(object)
        )
        assert(
            isAny(object[[col]], c("List", "list")),
            msg = sprintf("{.var %s} is not a list-column.", col)
        )
        spl <- split(x = object, f = seq_len(nrow(object)))
        lst <- lapply(
            X = spl,
            FUN = function(row) {
                vals <- row[[col]][[1L]]
                if (length(vals) == 0L) {
                    vals <- NA
                }
                if (length(vals) > 1L) {
                    row <- row[
                        rep(seq_len(nrow(row)), each = length(vals)), ,
                        drop = FALSE
                    ]
                }
                row[[col]] <- vals
                row
            }
        )
        out <- do.call(what = rbind, args = lst)
        assert(is(out, "DFrame"))
        rownames(out) <- NULL
        out
    }



#' @rdname unnest2
#' @export
setMethod(
    f = "unnest2",
    signature = signature(object = "DFrame"),
    definition = `unnest2,DFrame`
)
