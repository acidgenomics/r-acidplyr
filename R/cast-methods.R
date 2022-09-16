#' @name cast
#' @inherit AcidGenerics::cast
#' @note Updated 2022-09-16.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(matrix, package = "AcidTest")
#'
#' ## DataFrame ====
#' long <- melt(matrix)
#' print(long)
#' wide <- cast(long)
#' print(wide)
NULL



## Updated 2022-09-16.
`cast,DataFrame` <- # nolint
    function(object,
             colnames = "colname",
             values = "value") {
        assert(
            requireNamespace("tidyr", quietly = TRUE),
            allAreAtomic(object),
            isString(colnames),
            isString(values),
            isSubset(c(colnames, values), colnames(object))
        )
        x <- as.data.frame(object)
        assert(identical(dim(x), dim(object)))
        x <- tidyr::pivot_wider(
            data = x,
            names_from = colnames,
            values_from = values,
            values_fill = NULL
        )
        x <- as(x, "DataFrame")
        if (isSubset("rowname", colnames(x))) {
            rownames(x) <- x[["rowname"]]
            x[["rowname"]] <- NULL
        }
        x
    }



#' @rdname cast
#' @export
setMethod(
    f = "cast",
    signature = signature(object = "DataFrame"),
    definition = `cast,DataFrame`
)
