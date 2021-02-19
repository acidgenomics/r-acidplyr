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
#' - `plyr::ldply()` (*deprecated*).
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
#' y <- unlistToDataFrame(x, recursive = TRUE)
#' print(y)
#' y <- unlistToDataFrame(x, recursive = FALSE)
#' print(y)
#' ## Note that non-recursive elements must be nested down an extra level.
#' ## FIXME RETHINK THIS.
#' print(y[[1L]][[1L]])
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
`unlistToDataFrame,list` <-  # nolint
    function(x) {
        assert(hasLength(x), hasNames(x))
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



#' @rdname unlistToDataFrame
#' @export
setMethod(
    f = "unlistToDataFrame",
    signature = signature("list"),
    definition = `unlistToDataFrame,list`
)
