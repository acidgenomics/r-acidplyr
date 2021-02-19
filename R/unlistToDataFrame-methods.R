## FIXME Consider renaming this to `mapToDataFrame()`?



#' @name unlistToDataFrame
#' @inherit AcidGenerics::unlistToDataFrame
#' @note Updated 2021-02-19.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @seealso
#' - `purrr::map_dfr()`.
#' - `plyr::ldply()` (*deprecated*).
#' - `pipette::as.DataFrame()`.
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
#' y <- unlistToDataFrame(x)
#' print(y)
NULL



## This step handles nested S4 List elements.
##
## The `purrr::map_dfr()` method doesn't expect complex S4 input and can do
## weird, unexpected things. The function will return `group`, `group_name`, and
## `value` columns in this case, which we don't want here. So we're hardening
## against unexpected return by not allowing complex S4 items in recursive mode.
##
## Updated 2021-02-19.
.decodeNestedList <- function(x) {
    lapply(
        X = x,
        FUN = function(x) {
            if (is.list(x)) {
                .decodeNestedList(x)
            } else if (is(x, "List")) {
                as.list(x)
            } else {
                assert(allAreAtomic(x))
                x
            }
        }
    )
}



## Updated 2021-02-19.
.unlistDfBase <- function(x) {
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



## Updated 2021-02-19.
.unlistDfMapDfr <- function(x) {
    x <- .decodeNestedList(x)
    idCol <- "name"
    assert(areDisjointSets(idCol, names(x)))
    y <- map_dfr(.x = x, .f = ~.x, .id = idCol)
    ## This doesn't get set when we bind a simple list of atomic vectors.
    if (isSubset(idCol, colnames(y))) {
        y[[idCol]] <- as.factor(y[[idCol]])
    }
    y <- as(y, "DataFrame")
    y
}



## Updated 2021-02-19.
`unlistToDataFrame,list` <-  # nolint
    function(
        x,
        method = c("map_dfr", "base")
    ) {
        assert(hasLength(x))
        switch(
            EXPR = match.arg(method),
            "map_dfr" = .unlistDfMapDfr(x),
            "base" = .unlistDfBase(x)
        )
    }



#' @rdname unlistToDataFrame
#' @export
setMethod(
    f = "unlistToDataFrame",
    signature = signature("list"),
    definition = `unlistToDataFrame,list`
)
