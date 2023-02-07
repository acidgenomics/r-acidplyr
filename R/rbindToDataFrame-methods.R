## See also:
## - https://stackoverflow.com/questions/45734380/transpose-nested-list
## - https://github.com/Rdatatable/data.table/blob/master/src/rbindlist.c
## - https://github.com/tidyverse/purrr/blob/main/R/list-transpose.R
## - https://github.com/tidyverse/purrr/blob/main/src/transpose.c



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
        dimnames <- list(
            names(x),
            unique(unlist(
                x = lapply(X = x, FUN = names),
                recursive = FALSE,
                use.names = FALSE
            ))
        )
        assert(
            !is.null(dimnames[[2L]]),
            msg = "Nested list elements are not named."
        )
        ## Transpose the list.
        xt <- Map(
            j = dimnames[[2L]],
            f = function(j, i, x) {
                Map(
                    i = i,
                    f = function(i, j, x) {
                        if (j %in% names(x[[i]])) {
                            value <- x[[i]][[j]]
                        } else {
                            value <- NA
                        }
                        value
                    },
                    MoreArgs = list(
                        "j" = j,
                        "x" = x
                    ),
                    USE.NAMES = TRUE
                )
            },
            MoreArgs = list(
                "i" = seq_along(x),
                "x" = x
            ),
            USE.NAMES = TRUE
        )
        isAtomic <- bapply(
            X = xt,
            FUN = function(x) {
                is.atomic(unlist(x, recursive = FALSE, use.names = FALSE))
            }
        )
        args <- Map(
            x = xt,
            isAtomic = isAtomic,
            f = function(x, isAtomic) {
                x <- unname(x)
                if (isTRUE(isAtomic)) {
                    do.call(what = c, args = x)
                } else {
                    # Replace any nested NAs with NULL for lists.
                    x <- lapply(
                        X = x,
                        FUN = function(x) {
                            if (identical(x, NA)) {
                                NULL
                            } else {
                                x
                            }
                        }
                    )
                    do.call(what = I, args = list(I(x)))
                }
            }
        )
        args <- append(x = args, values = list("row.names" = dimnames[[1L]]))
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
