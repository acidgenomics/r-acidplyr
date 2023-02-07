## FIXME Rework this using rbindlist approach from data.table?
## Check Cellosaurus code for example...



#' @name rbindToDataFrame
#' @inherit AcidGenerics::rbindToDataFrame
#' @note Updated 2021-02-20.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' x <- list(
#'     "a" = list(
#'         "aa" = seq(from = 1L, to = 3L),
#'         "bb" = seq(from = 4L, to = 6L)
#'     ),
#'     "b" = list(
#'         "cc" = seq(from = 7L, to = 9L),
#'         "dd" = seq(from = 10L, to = 12L)
#'     ),
#'     "c" = list(
#'         "ee" = seq(from = 13L, to = 15L),
#'         "ff" = seq(from = 16L, to = 18L)
#'     )
#' )
#' print(x)
#' y <- rbindToDataFrame(x)
#' print(y)
NULL



## Consider using `autopadZeros()` here when names are not defined.
## Updated 2021-02-20.
`rbindToDataFrame,list` <- # nolint
    function(x) {
        assert(hasLength(x))
        ## Don't allow evaluation of top-level S4 elements (e.g. IntegerList).
        if (any(bapply(X = x, FUN = isS4))) {
            return(DataFrame(
                "x1" = I(unname(x)),
                row.names = names(x)
            ))
        }
        hasNames <- c("rows" = TRUE, "cols" = TRUE)
        if (!hasNames(x)) {
            hasNames[["rows"]] <- FALSE
            names(x) <- paste0("x", seq_along(x))
        }
        if (isTRUE(requireNamespace("parallel", quietly = TRUE))) {
            .lapply <- parallel::mclapply
        } else {
            .lapply <- lapply
        }
        scalarList <- .lapply(
            X = x,
            FUN = function(x) {
                vapply(
                    X = x,
                    FUN = function(x) {
                        is.atomic(x) && identical(length(x), 1L)
                    },
                    FUN.VALUE = logical(1L),
                    USE.NAMES = TRUE
                )

            }
        )
        names(scalarList) <- names(x)
        dimnames <- list(
            names(x),
            unique(unlist(
                lapply(X = scalarList, FUN = names),
                recursive = FALSE,
                use.names = FALSE
            ))
        )
        atomicCols <- .lapply(
            X = scalarList,
            FUN = function(x) {
                names(x)[x]
            }
        )
        ## FIXME How to regenerate the colsList?
        args <- Map(
            col = colsList,
            atomic = atomicCols,
            f = function(col, atomic) {
                col <- unname(col)
                if (isTRUE(atomic)) {
                    do.call(what = c, args = col)
                } else {
                    ## Replace any nested NAs with NULL for lists.
                    col <- lapply(
                        X = col,
                        FUN = function(x) {
                            if (identical(x, NA)) {
                                NULL
                            } else {
                                x
                            }
                        }
                    )
                    do.call(what = I, args = list(I(col)))
                }
            }
        )
        if (isFALSE(hasNames[["rows"]])) {
            rowNames <- NULL
        } else {
            rowNames <- dimnames[[1L]]
        }
        args <- append(x = args, values = list("row.names" = rowNames))
        df <- do.call(what = DataFrame, args = args)
        assert(
            identical(nrow(df), length(x)),
            identical(length(dimnames[[2L]]), ncol(df))
        )
        df
    }



#' @rdname rbindToDataFrame
#' @export
setMethod(
    f = "rbindToDataFrame",
    signature = signature(x = "list"),
    definition = `rbindToDataFrame,list`
)
