#' @name mutate
#' @inherit AcidGenerics::mutate
#' @note Updated 2023-02-07.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' data(mtcars, package = "datasets")
#'
#' ## DFrame ====
#' x <- as(mtcars, "DFrame")
#' mutateAll(x, fun = log, base = 2L)
#' mutateAt(x, vars = c("mpg", "cyl"), fun = log, base = 2L)
#' mutateIf(x, predicate = is.double, fun = as.integer)
#' transmuteAt(x, vars = c("mpg", "cyl"), fun = log, base = 2L)
#' transmuteIf(x, predicate = is.double, fun = as.integer)
NULL



## Loop across the columns and then each row internally.
## Updated 2023-04-26.
`mutateAll,DFrame` <- # nolint
    function(object, fun, ...) {
        assert(
            requireNamespaces("pipette"),
            is.function(fun)
        )
        rn <- rownames(object)
        list <- lapply(X = object, FUN = fun, ...)
        out <- pipette::as.DataFrame(list)
        rownames(out) <- rn
        assert(
            identical(dim(out), dim(object)),
            identical(dimnames(out), dimnames(object))
        )
        metadata(out) <- metadata(object)
        out
    }



## Updated 2023-04-26.
`mutateAt,DFrame` <- # nolint
    function(object, vars, fun, ...) {
        x <- transmuteAt(object, vars = vars, fun = fun, ...)
        y <- object[, setdiff(colnames(object), colnames(x)), drop = FALSE]
        out <- cbind(x, y)
        out <- out[, colnames(object), drop = FALSE]
        out
    }



## Updated 2023-04-26.
`mutateIf,DFrame` <- # nolint
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



## Updated 2023-04-26.
`transmuteAt,DFrame` <- # nolint
    function(object, vars, fun, ...) {
        x <- object[, vars, drop = FALSE]
        x <- mutateAll(x, fun = fun, ...)
        x
    }



## Updated 2023-04-26.
`transmuteIf,DFrame` <- # nolint
    function(object, predicate, fun, ...) {
        x <- selectIf(object, predicate = predicate)
        x <- mutateAll(x, fun = fun, ...)
        x
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutateAll",
    signature = signature(
        object = "DFrame",
        fun = "function"
    ),
    definition = `mutateAll,DFrame`
)

#' @rdname mutate
#' @export
setMethod(
    f = "mutateAt",
    signature = signature(
        object = "DFrame",
        vars = "character",
        fun = "function"
    ),
    definition = `mutateAt,DFrame`
)

#' @rdname mutate
#' @export
setMethod(
    f = "mutateIf",
    signature = signature(
        object = "DFrame",
        predicate = "function",
        fun = "function"
    ),
    definition = `mutateIf,DFrame`
)


#' @rdname mutate
#' @export
setMethod(
    f = "transmuteAt",
    signature = signature(
        object = "DFrame",
        vars = "character",
        fun = "function"
    ),
    definition = `transmuteAt,DFrame`
)

#' @rdname mutate
#' @export
setMethod(
    f = "transmuteIf",
    signature = signature(
        object = "DFrame",
        predicate = "function",
        fun = "function"
    ),
    definition = `transmuteIf,DFrame`
)
