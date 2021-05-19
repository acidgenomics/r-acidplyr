#' @name mutate
#' @inherit AcidGenerics::mutate
#' @note Updated 2021-05-18.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' data(mtcars, package = "datasets")
#'
#' ## DataFrame ====
#' x <- as(mtcars, "DataFrame")
#' mutateAll(x, fun = log, base = 2L)
#' mutateAt(x, vars = c("mpg", "cyl"), fun = log, base = 2L)
#' mutateIf(x, predicate = is.double, fun = as.integer)
#' transmuteAt(x, vars = c("mpg", "cyl"), fun = log, base = 2L)
#' transmuteIf(x, predicate = is.double, fun = as.integer)
NULL



## Loop across the columns and then each row internally.
## Updated 2021-05-18.
`mutateAll,DataFrame` <-  # nolint
    function(object, fun, ...) {
        assert(is.function(fun))
        rn <- rownames(object)
        list <- lapply(X = object, FUN = fun, ...)
        out <- as.DataFrame(list)
        rownames(out) <- rn
        assert(
            identical(dim(out), dim(object)),
            identical(dimnames(out), dimnames(object))
        )
        metadata(out) <- metadata(object)
        out
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutateAll",
    signature = signature(
        object = "DataFrame",
        fun = "function"
    ),
    definition = `mutateAll,DataFrame`
)



## Updated 2020-10-12.
`mutateAt,DataFrame` <-  # nolint
    function(object, vars, fun, ...) {
        x <- transmuteAt(object, vars = vars, fun = fun, ...)
        y <- object[, setdiff(colnames(object), colnames(x)), drop = FALSE]
        out <- cbind(x, y)
        out <- out[, colnames(object), drop = FALSE]
        out
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutateAt",
    signature = signature(
        object = "DataFrame",
        vars = "character",
        fun = "function"
    ),
    definition = `mutateAt,DataFrame`
)



## Updated 2020-10-12.
`mutateIf,DataFrame` <-  # nolint
    function(object, predicate, fun, ...) {
        x <- transmuteIf(
            object = object,
            predicate = predicate,
            fun = fun,
            ...
        )
        y <- object[, setdiff(colnames(object), colnames(x)), drop = FALSE]
        out <- cbind(x, y)
        out <- out[, colnames(object), drop = FALSE]
        out
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutateIf",
    signature = signature(
        object = "DataFrame",
        predicate = "function",
        fun = "function"
    ),
    definition = `mutateIf,DataFrame`
)



## Updated 2020-10-12.
`transmuteAt,DataFrame` <-  # nolint
    function(object, vars, fun, ...) {
        x <- object[, vars, drop = FALSE]
        x <- mutateAll(x, fun = fun, ...)
        x
    }



#' @rdname mutate
#' @export
setMethod(
    f = "transmuteAt",
    signature = signature(
        object = "DataFrame",
        vars = "character",
        fun = "function"
    ),
    definition = `transmuteAt,DataFrame`
)



## Updated 2020-10-12.
`transmuteIf,DataFrame` <-  # nolint
    function(object, predicate, fun, ...) {
        x <- selectIf(object, predicate = predicate)
        x <- mutateAll(x, fun = fun, ...)
        x
    }



#' @rdname mutate
#' @export
setMethod(
    f = "transmuteIf",
    signature = signature(
        object = "DataFrame",
        predicate = "function",
        fun = "function"
    ),
    definition = `transmuteIf,DataFrame`
)
