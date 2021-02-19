#' @name unlistToDataFrame
#' @inherit AcidGenerics::unlistToDataFrame
#' @note Updated 2021-02-19.
#'
#' @inheritParams AcidRoxygen::params
#' @param recursive `logical(1)`.
#'  - `TRUE`: Recursively unlist all nested list columns.
#'    Calls `purrr::map_dfr()` internally.
#'  - `FALSE`: Only unlists the top level of list, allowing for retention
#'    of nested list columns and/or complex S4 objects.
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



## Recursive mode was previous default, so keeping that intact.
##
## The `purrr::map_dfr()` method doesn't expect complex S4 input and can do
## weird, unexpected things with `CharacterList` and `IntegerList` input, for
## example. The function will return `group`, `group_name`, and `value` columns
## in this case, which we don't want here. So we're hardening against unexpected
## return by not allowing complex S4 items in recursive mode.
##
## Updated 2021-02-19.
`unlistToDataFrame,list` <-  # nolint
    function(x, recursive = TRUE) {
        assert(
            hasLength(x),
            isFlag(recursive)
        )
        if (isTRUE(recursive)) {
            s4 <- bapply(X = x, FUN = isS4)
            if (any(s4)) {
                stop(sprintf(
                    paste(
                        "S4 elements not allowed in recursive mode.",
                        "Detected at: %s.",
                        sep = "\n"
                    ),
                    toString(which(s4), width = 100L)
                ))
            }
            y <- map_dfr(.x = x, .f = data.frame)
            y <- as(y, "DataFrame")
        } else {
            y <- do.call(
                what = DataFrame,
                args = lapply(
                    X = x,
                    FUN = function(x) {
                        I(SimpleList(I(x)))
                    }
                )
            )
            assert(
                hasLength(nrow(y), n = 1L),
                identical(length(x), ncol(y))
            )
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
