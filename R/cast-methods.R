#' @name cast
#' @inherit AcidGenerics::cast
#' @note Updated 2023-08-23.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param colnames `character(1)`.
#' Name of the column that contains the column name values in long format.
#' Must define an unordered `factor` column.
#'
#' @param values `character(1)`.
#' Name of the column that contains the values in long format.
#' Must define an `atomic` column that is not `factor`.
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
            isFALSE(is.ordered(object[[colnames]])),
            identical(
                x = levels(object[[colnames]]),
                y = sort(levels(object[[colnames]]))
            ),
            is.atomic(object[[values]]),
            !is.factor(object[[values]])
        )
        spl <- split(x = object[[values]], f = object[[colnames]])
        df <- do.call(what = cbind, args = spl)
        df <- as(df, "DFrame")
        extraCols <- setdiff(colnames(object), c(colnames, values))
        if (hasLength(extraCols)) {
            df2 <- object[
                seq_len(nlevels(object[[colnames]])),
                extraCols,
                drop = FALSE
            ]
            df <- cbind(df, df2)
        }
        if (isSubset("rowname", colnames(df))) {
            assert(hasNoDuplicates(df[["rowname"]]))
            rownames(df) <- df[["rowname"]]
            df[["rowname"]] <- NULL
        }
        df <- df[, sort(colnames(df)), drop = FALSE]
        df
    }



#' @rdname cast
#' @export
setMethod(
    f = "cast",
    signature = signature(object = "DFrame"),
    definition = `cast,DFrame`
)
