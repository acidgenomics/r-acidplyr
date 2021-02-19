## FIXME MOVE THIS TO PIPETTE AND REWORK INTO AS.DATAFRAME.



#' @name unlistToDataFrame
#' @inherit AcidGenerics::unlistToDataFrame
#' @note Updated 2021-02-19.
#'
#' @inheritParams AcidRoxygen::params
#' @param recursive `logical(1)`.
#'  - `TRUE`: Recursively unlist all nested list columns.
#'    Calls `purrr::map_dfr()` internally.
#'  - `FALSE`: Only unlists the top level of list, allowing for retention
#'    of nested list columns and/or complex S4 objects. Note that these
#'    elements are nested an extra level down as a `SimpleList`.
#' @param ... Additional arguments.
#'
#' @seealso
#' - `pipette::as.DataFrame()`.
#' - `purrr::map_dfr()`.
#' - `plyr::ldply()` (deprecated).
#'
#' @examples
#' x <- list(
#'     a = list(
#'         aa = seq(from = 1L, to = 3L),
#'         bb = seq(from = 4L, to = 6L)
#'     ),
#'     b = list(
#'         cc = seq(from = 7L, to = 9L),
#'         dd = seq(from = 10L, to = 12L)
#'     ),
#'     c = list(
#'         ee = seq(from = 13L, to = 15L),
#'         ff = seq(from = 16L, to = 18L)
#'     )
#' )
#' print(x)
#' y <- unlistToDataFrame(x, recursive = TRUE)
#' print(y)
#' y <- unlistToDataFrame(x, recursive = FALSE)
#' print(y)
#' ## Note that non-recursive elements must be nested down an extra level.
#' print(y[[1L]][[1L]])
NULL



## This step handles nested S4 List elements.
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
.unlistRecursive <- function(x) {
    x <- .decodeNestedList(x)
    y <- map_dfr(.x = x, .f = ~.x)
    y <- as(y, "DataFrame")
}



## Updated 2021-02-19.
.unlistNested <- function(x) {
    scalarList <- lapply(
        X = x,
        FUN = function(x) {
            assert(hasNames(x))
            bapply(
                X = x,
                FUN = function(x) {
                    isScalar(x)
                }
            )
        }
    )
    colnames <- unique(unlist(
        lapply(X = scalar, FUN = names),
        recursive = FALSE,
        use.names = FALSE
    ))
    assert(isCharacter(colnames))
    scalarMat <- matrix(
        data = TRUE,
        nrow = length(x),
        ncol = length(colnames),
        dimnames = list(names(x), colnames)
    )
    for (i in seq_along(scalarList)) {
        for (j in seq_along(scalarList[[i]])) {
            rowname <- names(scalarList)[[i]]
            colname <- names(scalarList[[i]])[[j]]
            if (isFALSE(scalarList[[i]][[j]])) {
                scalarMat[rowname, colname] <- FALSE
            }
        }
    }
    scalarCols <- apply(X = scalarMat, MARGIN = 2L, FUN = all)
    complexCols <- names(which(!scalarCols))
    ## FIXME NEED TO TEST THIS CASE.
    if (!hasLength(complexCols)) {
        df <- DataFrame(
            lapply(X = x, FUN = I),
            row.names = names(x)
        )
        return(df)
    }
    scalarCols <- names(which(scalarCols))




    ## FIXME RETHINKT THIS APPROACH?
    ## ASSIGN INTO EXPANDED MATRIX?
    df <- DataFrame(matrix(
        data = NA,
        nrow = nrow(scalarMat),
        ncol = ncol(scalarMat),
        dimnames = dimnames(scalarMat)
    ))


    ## UGH THIS CODE IS MESSY, NEED TO RETHINK...
    ## Collect the columns and selectively assign.
    cols <- mapply(
        colname = colnames(scalarMat),
        MoreArgs = list(
            rownames = rownames(scalarMat),
            input = x
        ),
        FUN = function(colname, rownames, input) {
            ok <- colname %in% scalarCols
            xxx <- list()
            for (i in seq_along(list)) {
                xxx[[i]] <- list[[i]][[col]]
            }
            if (isTRUE(ok)) {
                xxx <- unlist(xxx, recursive = FALSE, use.names = FALSE)
            }
            xxx
        },
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
    )
    names(cols) <- rownames(scalarMat)

    assert(
        hasLength(nrow(y), n = 1L),
        identical(length(x), ncol(y))
    )
}



## Recursive mode was previous default, so keeping that intact.
##
## The `purrr::map_dfr()` method doesn't expect complex S4 input and can do
## weird, unexpected things. The function will return `group`, `group_name`, and
## `value` columns in this case, which we don't want here. So we're hardening
## against unexpected return by not allowing complex S4 items in recursive mode.
##
## Updated 2021-02-19.
`unlistToDataFrame,list` <-  # nolint
    function(x, recursive = TRUE) {
        assert(
            hasLength(x),
            hasNames(x),
            isFlag(recursive)
        )
        if (isTRUE(recursive)) {
            y <- .unlistRecursive(x)
        } else {
            y <- .unlistNested(x)
        }
        y
    }



#' @rdname unlistToDataFrame
#' @export
setMethod(
    f = "unlistToDataFrame",
    signature = signature("list"),
    definition = `unlistToDataFrame,list`
)
