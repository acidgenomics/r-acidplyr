#' @name collapseToString
#' @inherit AcidGenerics::collapseToString
#' @note Updated 2021-09-03.
#'
#' @inheritParams AcidRoxygen::params
#' @param sep `character(1)`.
#'   Separator. Defaults to comma.
#' @param unique `logical(1)`.
#'   Unique values.
#' @param sort `logical(1)`.
#'   Sort values.
#' @param ... Additional arguments.
#'
#' @seealso `toString()`.
#'
#' @return
#' - `atomic`: `character(1)`.
#' - `dim`: Object of same class, collapsed to a single row.
#'
#' @examples
#' ## character ====
#' groceries <- c(NA, NA, "milk", "eggs", "eggs", "veggies")
#' collapseToString(
#'     groceries,
#'     unique = TRUE,
#'     sort = TRUE
#' )
#' collapseToString(
#'     groceries,
#'     unique = FALSE,
#'     sort = FALSE
#' )
#'
#' ## numeric ====
#' collapseToString(seq(1:5))
#'
#' ## logical ====
#' collapseToString(c(TRUE, FALSE))
#' collapseToString(c(NA, NaN))
#'
#' ## data.frame ====
#' df <- datasets::iris
#' df <- head(df)
#' df <- collapseToString(df, sort = TRUE, unique = TRUE)
#' t(df)
NULL



## Updated 2021-02-02.
`collapseToString,atomic` <-  # nolint
    function(
        object,
        sep = ", ",
        sort = FALSE,
        unique = FALSE
    ) {
        assert(
            isAny(object, classes = c("character", "factor", "vector")),
            isString(sep),
            isFlag(unique),
            isFlag(sort)
        )
        if (isScalar(object)) {
            return(object)
        }
        if (isTRUE(unique)) {
            object <- unique(object)
        }
        if (isTRUE(sort)) {
            object <- sort(object, na.last = TRUE)
        }
        out <- as.character(object)
        out <- paste(out, collapse = sep)
        out
    }



## Alternatively, can use `dplyr::summarise_all()` approach.
## Updated 2020-02-02.
`collapseToString,matrix` <-  # nolint
    function(
        object,
        sep = ", ",
        sort = FALSE,
        unique = FALSE
    ) {
        assert(hasLength(object))
        x <- object
        x <- as.data.frame(x, stringsAsFactors = FALSE)
        list <- lapply(
            X = x,
            FUN = function(x) {
                x <- collapseToString(
                    object = x,
                    sep = sep,
                    sort = sort,
                    unique = unique
                )
            }
        )
        x <- do.call(what = cbind, args = list)
        x <- as.data.frame(x, stringsAsFactors = FALSE)
        x <- as(object = x, Class = class(object)[[1L]])
        x
    }



## Updated 2019-07-22.
`collapseToString,data.frame` <-  # nolint
    `collapseToString,matrix`



## Updated 2019-08-18.
`collapseToString,DFrame` <-  # nolint
    `collapseToString,data.frame`



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature(object = "DFrame"),
    definition = `collapseToString,DFrame`
)

#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature(object = "atomic"),
    definition = `collapseToString,atomic`
)

#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature(object = "data.frame"),
    definition = `collapseToString,data.frame`
)

#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature(object = "matrix"),
    definition = `collapseToString,matrix`
)
