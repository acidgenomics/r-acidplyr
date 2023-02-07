## Look to data.table rbindlist C code for performance optimization.
## https://github.com/Rdatatable/data.table/blob/master/src/rbindlist.c

## https://stackoverflow.com/questions/45734380/transpose-nested-list
## https://github.com/tidyverse/purrr/blob/main/R/list-transpose.R
## https://github.com/tidyverse/purrr/blob/main/src/transpose.c



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
        ## Don't allow evaluation of top-level S4 elements (e.g. IntegerList).
        if (any(bapply(X = x, FUN = isS4))) {
            return(DataFrame("x1" = I(unname(x)), row.names = names(x)))
        }
        assert(
            hasLength(x),
            requireNamespaces("purrr")
        )
        dimnames <- list(
            names(x),
            unique(unlist(
                x = lapply(X = x, FUN = names),
                recursive = FALSE,
                use.names = FALSE
            ))
        )
        lt <- purrr::list_transpose(x)
        mat <- do.call(what = cbind, args = lt)
        df <- as(mat, "DataFrame")
        rownames(df) <- dimnames[[1L]]
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
