#' @name unnest2
#' @inherit AcidGenerics::unnest2 description title
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
        assert(isScalar(col))
    }



#' @rdname unnest2
#' @export
setMethod(
    f = "unnest2",
    signature = signature(object = "DFrame"),
    definition = `unnest2,DFrame`
)
