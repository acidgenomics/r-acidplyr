#' @name filterNested
#' @inherit AcidGenerics::filterNested
#' @note Updated 2023-12-12.
#'
#' @param ... Additional arguments.
#'
#' @param ignoreCase `logical(1)`.
#' Perform case insensitive pattern matching.
#'
#' @examples
#' ## DFrame ====
#' pattern <- "^[A-Z]+$"
#' object <- S4Vectors::DataFrame(
#'     "V1" = c("a", "b", "c"),
#'     "V2" = I(list(
#'         c("aa", "bb", "CC"),
#'         c("dd", "ee", "ff"),
#'         c("gg", "hh", "ii")
#'     ))
#' )
#' object <- filterNested(object = object, pattern = pattern)
#' print(object)
NULL



## CharacterList method for `grepl` doesn't currently support `fixed`
## argument correctly, so disabling.
## https://github.com/Bioconductor/IRanges/issues/51

## Updated 2023-12-12.
`filterNested,DFrame` <- # nolint
    function(object, pattern, ignoreCase = FALSE) {
        assert(
            isString(pattern),
            isFlag(ignoreCase)
        )
        lst <- apply(
            X = object,
            MARGIN = 1L,
            FUN = function(x) {
                x <- tryCatch(
                    expr = {
                        unlist(x = x, recursive = TRUE, use.names = FALSE)
                    },
                    error = function(e) {
                        NULL
                    }
                )
                if (is.null(x)) {
                    return(x)
                }
                x <- as.character(x)
                x <- na.omit(x)
                x <- unique(x)
                x
            },
            simplify = FALSE
        )
        names(lst) <- NULL
        cl <- CharacterList(lst)
        ll <- grepl(
            pattern = pattern,
            x = cl,
            ignore.case = ignoreCase,
            fixed = FALSE
        )
        assert(is(ll, "LogicalList"))
        i <- any(ll)
        assert(
            any(i),
            msg = sprintf("Failed to match pattern: {.var %s}.", pattern)
        )
        out <- object[i, , drop = FALSE]
        out
    }



#' @rdname filterNested
#' @export
setMethod(
    f = "filterNested",
    signature = signature(
        object = "DFrame",
        pattern = "character"
    ),
    definition = `filterNested,DFrame`
)
