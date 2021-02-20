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



## Updated 2021-02-19.
`rbindToDataFrame,list` <-  # nolint
    function(x) {
        assert(hasLength(x))
        rowNames <- names(x)
        hasPosNames <- c("rows" = FALSE, "cols" = FALSE)
        if (!hasNames(x)) {
            hasPosNames[["rows"]] <- TRUE
            names(x) <- paste0("x", seq_along(x))
        }
        scalarList <- list()
        length(scalarList) <- length(x)
        names(scalarList) <- names(x)
        for (i in seq_along(x)) {
            xx <- x[[i]]
            xx <- bapply(X = xx, FUN = isScalarAtomic)
            if (!hasNames(xx)) {
                if (isFALSE(hasPosNames[["cols"]])) {
                    hasPosNames[["cols"]] <- TRUE
                }
                names(xx) <- paste0("x", seq_along(xx))
            }
            scalarList[[i]] <- xx
        }
        rm(xx)
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
        rm(rn, cn)
        atomicCols <- apply(X = scalarMat, MARGIN = 2L, FUN = all)
        colsList <- list()
        length(colsList) <- ncol(scalarMat)
        names(colsList) <- dimnames[[2L]]
        for (i in dimnames[[1L]]) {
            for (j in dimnames[[2L]]) {
                value <- x[[i]][[j]]
                if (is.null(value)) value <- NA
                colsList[[j]][[i]] <- value
            }
        }
        rm(value)
        assert(
            identical(names(colsList), names(atomicCols)),
            all(bapply(X = colsList, FUN = hasLength, n = length(x)))
        )
        args <- mapply(
            col = colsList,
            atomic = atomicCols,
            FUN = function(col, atomic) {
                if (isTRUE(atomic)) {
                    do.call(what = c, args = col)
                } else {
                    do.call(what = I, args = list(col))
                }
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
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
