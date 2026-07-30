#' @name rbindToDataFrame
#' @inherit AcidGenerics::rbindToDataFrame
#' @note Updated 2023-02-22.
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


## Updated 2023-09-25.
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
            !anyNA(dimnames[[2L]]),
            msg = "Nested list elements are not named."
        )
        ## Transpose the list.
        ## Align each row to the full column set with a single vectorized
        ## `row[idx]`, instead of one `match()` + `[[` call per cell -- for
        ## an atomic-vector row, `row[idx]` also coerces an unmatched
        ## position's value to that vector's type (e.g. `NA_integer_`), so
        ## it must be overwritten back to a plain (logical) `NA` explicitly
        ## to match the original per-cell fallback exactly. List rows are
        ## handled the same way for consistency, even though `row[idx]`
        ## already yields `NULL` (not a typed `NA`) for a list.
        perRow <- lapply(X = x, FUN = function(row) {
            idx <- match(dimnames[[2L]], names(row))
            miss <- is.na(idx)
            out <- if (is.list(row)) {
                unname(row[idx])
            } else {
                as.list(unname(row[idx]))
            }
            if (any(miss)) {
                out[miss] <- list(NA)
            }
            out
        })
        xt <- lapply(
            X = seq_along(dimnames[[2L]]),
            FUN = function(j) unname(lapply(perRow, `[[`, j))
        )
        names(xt) <- dimnames[[2L]]
        ## Refer to `pipette::as.DataFrame` for inspiration on this approach.
        df <- new(Class = "DFrame", listData = xt, nrows = length(xt[[1L]]))
        assert(identical(nrow(df), length(x)))
        dimnames(df) <- dimnames
        isScalarAtomic <- bapply(
            X = df,
            FUN = function(x) {
                all(bapply(
                    X = x,
                    FUN = function(x) {
                        is.atomic(x) && identical(length(x), 1L)
                    }
                ))
            }
        )
        for (pos in which(isScalarAtomic)) {
            df[[pos]] <- unlist(df[[pos]], recursive = FALSE, use.names = FALSE)
        }
        for (pos in which(!isScalarAtomic)) {
            df[[pos]] <- lapply(
                X = df[[pos]],
                FUN = function(x) {
                    if (identical(x, NA)) {
                        NULL
                    } else {
                        x
                    }
                }
            )
        }
        df
    }


#' @rdname rbindToDataFrame
#' @export
setMethod(
    f = "rbindToDataFrame",
    signature = signature(x = "list"),
    definition = `rbindToDataFrame,list`
)
