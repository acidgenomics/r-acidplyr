#' @name unlistToDataFrame
#' @inherit AcidGenerics::unlistToDataFrame
#' @note Updated 2021-02-19.
#'
#' @inheritParams AcidRoxygen::params
#' @param recursive `logical(1)`.
#'  - `TRUE`: Recursively unlist all nested list columns.
#'    Calls `purrr::map_dfr` internally.
#'  - `FALSE`: Only unlists the top level of list, allowing for retention
#'    of nested list columns and/or complex S4 objects.
#' @param ... Additional arguments.
#'
#' @seealso
#' - `purrr::map_dfr`.
#' - `plyr::ldply` (deprecated).
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
#' y <- unlistToDataFrame(x, recursive = FALSE)
#' print(x)
NULL



## NOTE Recursive mode was previous default, so keeping that intact.
## Updated 2021-02-19.
`unlistToDataFrame,list` <-  # nolint
    function(x, recursive = TRUE) {
        if (isTRUE(recursive)) {
            x <- map_dfr(.x = x, .f = data.frame)
        } else {
            x <- rbind(lapply(X = x, FUN = I))
        }
        x <- as(x, "DataFrame")
        x
    }



#' @rdname unlistToDataFrame
#' @export
setMethod(
    f = "unlistToDataFrame",
    signature = signature("list"),
    definition = `unlistToDataFrame,list`
)
