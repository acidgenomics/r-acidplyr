## Look to data.table rbindlist C code for performance optimization.
## https://github.com/Rdatatable/data.table/blob/master/src/rbindlist.c



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
        if (hasNames(x)) {
            hasRownames <- TRUE
        } else {
            hasRownames <- FALSE
            names(x) <- paste0("x", seq_along(x))
        }
        if (isTRUE(requireNamespace("parallel", quietly = TRUE))) {
            .mcMap <- parallel::mcMap # nolint
            .mclapply <- parallel::mclapply
        } else {
            .mcMap <- Map # nolint
            .mclapply <- lapply
        }
        isScalarList <- .mclapply(
            X = x,
            FUN = function(x) {
                vapply(
                    X = x,
                    FUN = function(x) {
                        ## Much slower when using `isScalarAtomic` here.
                        is.atomic(x) && identical(length(x), 1L)
                    },
                    FUN.VALUE = logical(1L),
                    USE.NAMES = TRUE
                )
            }
        )
        dimnames <- list(
            names(x),
            unique(unlist(
                lapply(X = isScalarList, FUN = names),
                recursive = FALSE,
                use.names = FALSE
            ))
        )
        assert(
            !is.null(dimnames[[2L]]),
            msg = "Nested list elements are not named."
        )
        y <- rep(TRUE, length(dimnames[[2L]]))
        names(y) <- dimnames[[2L]]
        isScalarList2 <- .mcMap(
            x = isScalarList,
            f = function(x, y) {
                y[names(x)] <- x
                y
            },
            MoreArgs = list("y" = y),
            USE.NAMES = TRUE
        )
        ## Need to coerce to data.frame here, otherwise matrix won't be sized
        ## correctly in downstream `Map` call.
        isScalarCols <- as.data.frame(do.call(
            what = rbind, args = isScalarList2
        ))
        colsList <- .mcMap(
            colname = dimnames[[2L]],
            f = function(colname, rownames, lst) {
                Map(
                    rowname = rownames,
                    f = function(rowname, colname, lst) {
                        value <- tryCatch(
                            expr = lst[[rowname]][[colname]],
                            error = function(e) NULL
                        )
                        if (is.null(value)) {
                            value <- NA
                        }
                        value
                    },
                    MoreArgs = list(
                        "colname" = colname,
                        "lst" = lst
                    ),
                    USE.NAMES = TRUE
                )
            },
            MoreArgs = list(
                "rownames" = dimnames[[1L]],
                "lst" = x
            ),
            USE.NAMES = TRUE
        )
        assert(areSameLength(colsList, isScalarCols))
        args <- .mcMap(
            col = colsList,
            isScalar = isScalarCols,
            f = function(col, isScalar) {
                col <- unname(col)
                if (all(isScalar)) {
                    do.call(what = c, args = col)
                } else {
                    # Replace any nested NAs with NULL for lists.
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
            },
            USE.NAMES = TRUE
        )
        if (isTRUE(hasRownames)) {
            rownames <- dimnames[[1L]]
        } else {
            rownames <- NULL
        }
        args <- append(x = args, values = list("row.names" = rownames))
        df <- do.call(what = DataFrame, args = args)
        assert(identical(nrow(df), length(x)))
        df
    }



#' @rdname rbindToDataFrame
#' @export
setMethod(
    f = "rbindToDataFrame",
    signature = signature(x = "list"),
    definition = `rbindToDataFrame,list`
)
