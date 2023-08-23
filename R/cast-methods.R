## FIXME Need to rework this to not depend on tidyr.
## FIXME Need to simulate extra columns to fill.
## > long[["extra"]] <- toupper(long[["rowname"]])
## FIXME Need to check fill order here, what about flipped factor levels?
## Consider coercing factor to character or something...



#' @name cast
#' @inherit AcidGenerics::cast
#' @note Updated 2023-08-23.
#'
#' @details
#' Requires the tidyr package to be installed.
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



## Updated 2023-08-23.
`cast,DFrame` <- # nolint
    function(object,
             colnames = "colname",
             values = "value") {
        assert(
            isString(colnames),
            isString(values),
            isSubset(c(colnames, values), colnames(object)),
            is.factor(object[[colnames]]),
            is.atomic(object[[values]])
        )
        spl <- split(x = object[[values]], f = object[[colnames]])
        df <- do.call(what = cbind, args = spl)
        df <- as(df, "DFrame")


        ## Need to handle any remaining columns.
        ## Ensure columns return sorted alphabetically.


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
