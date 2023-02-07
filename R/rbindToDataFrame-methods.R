## NOTE Consider using data.table::rbindlist if everything is scalar and the
## input dataset is large.

## NOTE Consider using `autopadZeros()` here when names are not defined.



#' @name rbindToDataFrame
#' @inherit AcidGenerics::rbindToDataFrame
#' @note Updated 2023-02-07.
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



## Updated 2023-02-07.
`rbindToDataFrame,list` <- # nolint
    function(x) {
        assert(hasLength(x))
        ## Don't allow evaluation of top-level S4 elements (e.g. IntegerList).
        if (any(bapply(X = x, FUN = isS4))) {
            return(DataFrame("x1" = I(unname(x)), row.names = names(x)))
        }
        if (isTRUE(requireNamespace("parallel", quietly = TRUE))) {
            .Map <- parallel::mcMap  # nolint
            .lapply <- parallel::mclapply
        } else {
            .Map <- Map # nolint
            .lapply <- lapply
        }
        anyS4 <- any(unlist(.lapply(
            X = x,
            FUN = function(x) {
                bapply(X = x, FUN = isS4)
            }
        )))
        if (
            isFALSE(anyS4) &&
            isTRUE(requireNamespace("data.table", quietly = TRUE))
        ) {
            df <- data.table::rbindlist(l = x, use.names = TRUE, fill = FALSE)
            df <- as(df, "DataFrame")
            assert(identical(nrow(df), length(x)))
            rownames(df) <- names(x)
            return(df)
        }
        if (hasNames(x)) {
            setRownames <- TRUE
        } else {
            setRownames <- FALSE
            names(x) <- paste0("x", seq_along(x))
        }




        isScalarList <- .lapply(
            X = x,
            FUN = function(x) {
                x <- vapply(
                    X = x,
                    FUN = function(x) {
                        ## FIXME This is too slow: isScalarAtomic(x)
                        is.atomic(x) && identical(length(x), 1L)
                    },
                    FUN.VALUE = logical(1L),
                    USE.NAMES = TRUE
                )
                if (!hasNames(x)) {
                    names(x) <- paste0("x", seq_along(x))
                }
                x
            }
        )
        names(isScalarList) <- names(x)
        dimnames <- list(
            names(x),
            unique(unlist(
                lapply(X = isScalarList, FUN = names),
                recursive = FALSE,
                use.names = FALSE
            ))
        )
        isScalarList2 <- .lapply(
            X = isScalarList,
            colnames = dimnames[[2L]],
            FUN = function(x, colnames) {
                x <- colnames %in% names(x)[x]
                names(x) <- colnames
                x
            }
        )
        isScalarMat <- do.call(what = rbind, args = isScalarList2)
        ## FIXME If this returns true and data.table is installed, try
        ## using that first.


        colsList <- list()
        length(colsList) <- ncol(isScalarMat)
        names(colsList) <- dimnames[[2L]]
        ## FIXME This is still quite a bit slower compared to rbindlist.
        ## FIXME Need to rework our NULL handling here?
        colsList <- .lapply(
            X = dimnames[[2L]],
            rownames = dimnames[[1L]],
            lst = x,
            FUN = function(j, rownames, lst) {
                lapply(
                    X = rownames,
                    j = j,
                    lst = lst,
                    FUN = function(i, j, lst) {
                        lst[[i]][[j]]
                    }
                )
            }
        )
        names(colsList) <- dimnames[[2L]]
        args <- .Map(
            col = colsList,
            isScalar = isScalarMat,
            f = function(col, isScalar) {
                col <- unname(col)
                if (all(isScalar)) {
                    do.call(what = c, args = col)
                } else {
                    do.call(what = I, args = list(I(col)))
                }
            }
        )
        if (isTRUE(setRownames)) {
            rowNames <- names(x)
        } else {
            rowNames <- NULL
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
