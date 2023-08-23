#' @name nest
#' @inherit AcidGenerics::nest description title
#' @note Updated 2023-08-23.
#'
#' @param col `character(1)`.
#' Name or position of the column to nest into a list.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' ## DFrame ====
#' long <- melt(matrix)
#' print(long)
#' wide <- cast(long)
#' print(wide)
NULL



## Updated 2023-08-23.
`nest,DFrame` <-
    function(object, col) {
    }
