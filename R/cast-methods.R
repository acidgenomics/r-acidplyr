#' @name cast
#' @inherit AcidGenerics::cast
#' @note Updated 2023-04-26.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param colnames `character(1)`.
#' Name of the column that contains the column name values in long format.
#'
#' @param values `character(1)`.
#' Name of the column that contains the values in long format.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(matrix, package = "AcidTest")
#'
#' ## DFrame ====
#' long <- melt(matrix)
#' print(long)
#' wide <- cast(long)
#' print(wide)
NULL



## Updated 2023-04-26.
`cast,DFrame` <- # nolint
    function(object,
             colnames = "colname",
             values = "value") {
        assert(
            requireNamespaces("tidyr"),
            allAreAtomic(object),
            isString(colnames),
            isString(values),
            isSubset(c(colnames, values), colnames(object))
        )
        x <- as.data.frame(object)
        assert(identical(dim(x), dim(object)))
        x <- tidyr::pivot_wider(
            data = x,
            ## `tidy-select` is required here.
            ## See `help("tidyr_tidy_select", "tidyr")` for details.
            names_from = {{ colnames }},
            values_from = {{ values }},
            values_fill = NULL
        )
        x <- as(x, "DFrame")
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
    signature = signature(object = "DFrame"),
    definition = `cast,DFrame`
)
