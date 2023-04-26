#' Select multiple columns
#'
#' @name select
#' @note Updated 2019-08-26.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams mutate
#' @param ... Additional arguments.
#'
#' @return Modified object.
#'
#' @seealso
#' These functions are inspired by dplyr. However, they are designed to only
#' work on `DFrame` class, and use base R code internally.
#'
#' ```r
#' `help(topic = "select_all", package = "dplyr")`
#' ```
#'
#' @examples
#' data(iris, package = "datasets")
#'
#' ## DFrame ====
#' x <- as(iris, "DFrame")
#' selectIf(x, predicate = is.factor)
NULL



## Updated 2023-04-26.
`selectIf,DFrame` <- # nolint
    function(object, predicate) {
        keep <- bapply(X = object, FUN = predicate)
        object[, keep, drop = FALSE]
    }



#' @rdname select
#' @export
setMethod(
    f = "selectIf",
    signature = signature(
        object = "DFrame",
        predicate = "function"
    ),
    definition = `selectIf,DFrame`
)
