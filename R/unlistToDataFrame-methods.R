#' @name unlistToDataFrame
#' @inherit AcidGenerics::unlistToDataFrame
#' @note Updated 2020-12-22.
#'
#' @inheritParams AcidRoxygen::params
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
#' x <- unlistToDataFrame(x)
#' print(x)
NULL



`unlistToDataFrame,list` <-  # nolint
    function(x) {
        x <- map_dfr(.x = x, .f = data.frame)
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
