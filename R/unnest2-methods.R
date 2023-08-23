## FIXME Need to rework this to not depend on tidyr.



#' @name unnest2
#' @inherit AcidGenerics::unnest2
#' @note Updated 2023-08-23.
#'
#' @param col `character(1)`.
#' Name of the list-column to unnest into long format.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' ## DFrame ====
NULL



## Updated 2023-08-23.
`unnest2,DFrame` <-
    function(object, col) {
        assert(
            requireNamespaces("tidyr"),
            isString(col),
            isSubset(col, colnames(object))
        )
        rownames(object) <- NULL
        data <- as.data.frame(object)
        assert(
            is.list(object[[col]]),
            msg = sprintf("{.var %s} is not a list-column.", col)
        )
        out <- tidyr::unnest(data = data, col = {{ col }})
        out <- as(out, "DFrame")
        rownames(out) <- NULL
        out
    }



#' @rdname unnest2
#' @export
setMethod(
    f = "unnest2",
    signature = signature(object = "DFrame"),
    definition = `unnest2,DFrame`
)
