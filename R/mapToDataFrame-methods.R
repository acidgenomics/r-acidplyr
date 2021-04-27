#' @name mapToDataFrame
#' @inherit AcidGenerics::mapToDataFrame
#' @note Updated 2021-04-27.
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
#' y <- mapToDataFrame(x)
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



## Updated 2021-04-27.
`mapToDataFrame,list` <-  # nolint
    function(x) {
        requireNamespaces("purrr")
        assert(hasLength(x))
        x <- .decodeNestedList(x)
        idCol <- "name"
        assert(areDisjointSets(idCol, names(x)))
        y <- purrr::map_dfr(.x = x, .f = ~.x, .id = idCol)
        ## This doesn't get set when we bind a simple list of atomic vectors.
        if (isSubset(idCol, colnames(y))) {
            y[[idCol]] <- as.factor(y[[idCol]])
        }
        y <- as(y, "DataFrame")
        y
    }



#' @rdname mapToDataFrame
#' @export
setMethod(
    f = "mapToDataFrame",
    signature = signature("list"),
    definition = `mapToDataFrame,list`
)
