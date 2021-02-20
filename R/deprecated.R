## nocov start
## nolint start



#' @name deprecated
#' @inherit AcidRoxygen::deprecated description examples return seealso title
#' @inheritParams AcidRoxygen::params
#' @keywords internal
NULL



## v0.1.13 =====================================================================
#' @rdname deprecated
#' @name unlistToDataFrame
#' @importFrom AcidGenerics unlistToDataFrame
#' @usage unlistToDataFrame(x, ...)
#' @export
NULL

## Updated 2021-02-20.
`unlistToDataFrame,list` <-  # nolint
    function(x, ...) {
        mapToDataFrame(x, ...)
    }

#' @rdname deprecated
#' @export
setMethod(
    f = "unlistToDataFrame",
    signature = signature("list"),
    definition = `unlistToDataFrame,list`
)



## nolint end
## nocov end
