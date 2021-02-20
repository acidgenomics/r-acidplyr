#' @name rbindToDataFrame
#' @inherit AcidGenerics::mapToDataFrame
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



## Consider using `autopadZeros` here to pad automatically generated positional
## row and column names.
## Updated 2021-02-19.
`rbindToDataFrame,list` <-  # nolint
    function(x) {
        assert(hasLength(x))
        hasNames <- c("rows" = TRUE, "cols" = TRUE)
        if (!hasNames(x)) {
            hasNames[["rows"]] <- FALSE
            names(x) <- paste0("x", seq_along(x))
        }
        scalarList <- list()
        length(scalarList) <- length(x)
        names(scalarList) <- names(x)
        for (i in seq_along(x)) {
            if (!hasNames(x[[i]])) {
                if (isTRUE(hasNames[["cols"]])) {
                    hasNames[["cols"]] <- FALSE
                }
                names(x[[i]]) <- paste0("x", seq_along(x[[i]]))
            }
            scalarList[[i]] <- bapply(X = x[[i]], FUN = isScalarAtomic)
        }
        dimnames <- list(
            names(x),
            unique(unlist(
                lapply(X = scalarList, FUN = names),
                recursive = FALSE,
                use.names = FALSE
            ))
        )
        scalarMat <- matrix(
            data = TRUE,
            nrow = length(dimnames[[1L]]),
            ncol = length(dimnames[[2L]]),
            dimnames = dimnames
        )
        for (i in seq_along(scalarList)) {
            for (j in seq_along(scalarList[[i]])) {
                rn <- names(scalarList)[[i]]
                cn <- names(scalarList[[i]])[[j]]
                if (isFALSE(scalarList[[i]][[j]])) {
                    scalarMat[rn, cn] <- FALSE
                }
            }
        }
        atomicCols <- apply(X = scalarMat, MARGIN = 2L, FUN = all)
        colsList <- list()
        length(colsList) <- ncol(scalarMat)
        names(colsList) <- dimnames[[2L]]
        for (i in dimnames[[1L]]) {
            for (j in dimnames[[2L]]) {
                value <- tryCatch(
                    expr = x[[i]][[j]],
                    error = function(e) NULL
                )
                if (is.null(value)) value <- NA
                colsList[[j]][[i]] <- value
            }
        }
        assert(
            identical(names(colsList), names(atomicCols)),
            all(bapply(X = colsList, FUN = hasLength, n = length(x)))
        )
        args <- mapply(
            col = colsList,
            atomic = atomicCols,
            FUN = function(col, atomic) {
                col <- unname(col)
                if (isTRUE(atomic)) {
                    do.call(what = c, args = col)
                } else {
                    do.call(what = I, args = list(col))
                }
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
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
    signature = signature("list"),
    definition = `rbindToDataFrame,list`
)
