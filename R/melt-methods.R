#' @name melt
#' @inherit AcidGenerics::melt
#' @note Updated 2023-04-27.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param colnames `character(3)`.
#' Column name mappings for melted data frame return.
#'
#' @param min `numeric(1)` or `NULL`.
#' Minimum count threshold to apply. Filters using "greater than or equal to"
#' logic internally. Note that this threshold gets applied prior to
#' logarithmic transformation, when `trans` argument applies.
#' Use `-Inf` or `NULL` to disable.
#'
#' @param minMethod `character(1)`.
#' Only applies when `min` argument is numeric.
#' Uses `match.arg()`.
#'
#' - `absolute`: Applies hard cutoff to `counts` column after the melt
#' operation. This applies to all counts, not per feature.
#' - `perRow`: Applies cutoff per row (i.e. gene). Internally, `rowSums()`
#' values are checked against this cutoff threshold prior to the melt
#' operation.
#'
#' @param trans `character(1)`.
#' Apply a log transformation (e.g. `log2(x + 1L)`) to the count matrix prior
#' to melting, if desired. Use `"identity"` to return unmodified (default).
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(matrix, package = "AcidTest")
#'
#' ## matrix ====
#' dim(matrix)
#' x <- melt(matrix)
#' dim(x)
#' print(x)
NULL



## NOTE Matrix method is defined in AcidExperiment and inherits matrix method.

## Updated 2023-04-27.
`melt,matrix` <- # nolint
    function(object,
             colnames = c("rowname", "colname", "value"),
             min = -Inf,
             minMethod = c("absolute", "perRow"),
             trans = c("identity", "log2", "log10")) {
        if (!hasRownames(object)) {
            rownames(object) <- as.character(seq_len(nrow(object)))
        }
        if (!hasColnames(object)) {
            colnames(object) <- as.character(seq_len(ncol(object)))
        }
        assert(
            hasDims(object),
            isCharacter(colnames),
            hasLength(colnames, n = 3L),
            areDisjointSets(colnames, colnames(object)),
            isNumber(min, nullOK = TRUE)
        )
        hasCli <- isInstalled("AcidCLI")
        if (isTRUE(hasCli)) {
            assert(requireNamespaces("AcidCLI"))
        }
        minMethod <- match.arg(minMethod)
        trans <- match.arg(trans)
        if (
            identical(minMethod, "perRow") &&
                isTRUE(is.finite(min))
        ) {
            if (is(object, "Matrix")) {
                assert(requireNamespaces("Matrix"))
                rowSums <- Matrix::rowSums
            }
            keep <- rowSums(object) >= min
            if (isTRUE(hasCli)) {
                AcidCLI::alertInfo(sprintf(
                    "%d / %d %s passed {.arg %s} >= {.val %s} cutoff.",
                    sum(keep, na.rm = TRUE),
                    nrow(object),
                    ngettext(
                        n = nrow(object),
                        msg1 = "feature",
                        msg2 = "features"
                    ),
                    minMethod,
                    as.character(min)
                ))
            }
            object <- object[keep, , drop = FALSE]
        }
        valueCol <- colnames[[3L]]
        dn <- dimnames(object)
        names(dn) <- colnames[seq_len(2L)]
        df <- DataFrame(expand.grid(
            dn,
            KEEP.OUT.ATTRS = FALSE,
            stringsAsFactors = TRUE
        ))
        value <- DataFrame(as.vector(object))
        names(value) <- colnames[[3L]]
        df <- cbind(df, value)
        df <- decode(df)
        if (
            identical(minMethod, "absolute") &&
                isTRUE(is.finite(min))
        ) {
            nPrefilter <- nrow(df)
            keep <- df[[valueCol]] >= min
            df <- df[keep, , drop = FALSE]
            if (isTRUE(hasCli)) {
                AcidCLI::alertInfo(sprintf(
                    paste(
                        "%d / %d %s passed {.arg %s} >= {.val %s}",
                        "expression cutoff."
                    ),
                    nrow(df),
                    nPrefilter,
                    ngettext(
                        n = nPrefilter,
                        msg1 = "feature",
                        msg2 = "features"
                    ),
                    minMethod,
                    as.character(min)
                ))
            }
        }
        ## Log transform the value, if desired.
        if (!identical(trans, "identity")) {
            assert(isInt(min))
            if (isTRUE(hasCli)) {
                AcidCLI::alert(sprintf(
                    "Applying {.code %s(x + 1L)} transformation.", trans
                ))
            }
            fun <- get(
                x = trans,
                envir = asNamespace("base"),
                inherits = FALSE
            )
            assert(is.function(fun))
            df[[valueCol]] <- fun(df[[valueCol]] + 1L)
        }
        df
    }



## This is used in pointillism package.
## Updated 2020-10-12.
`melt,table` <- # nolint
    function(object, ...) {
        melt(object = as.matrix(unclass(object)), ...)
    }



## Updated 2023-04-26.
`melt,DFrame` <- # nolint
    function(object,
             colnames = c("rowname", "colname", "value")) {
        assert(
            hasColnames(object),
            all(bapply(object, is.atomic)),
            hasLength(unlist(unique(lapply(object, class))), n = 1L)
        )
        melt(object = as.matrix(object), colnames = colnames)
    }

formals(`melt,DFrame`)[["colnames"]] <- # nolint
    formals(`melt,matrix`)[["colnames"]]



#' @rdname melt
#' @export
setMethod(
    f = "melt",
    signature = signature(object = "DFrame"),
    definition = `melt,DFrame`
)

#' @rdname melt
#' @export
setMethod(
    f = "melt",
    signature = signature(object = "matrix"),
    definition = `melt,matrix`
)

#' @rdname melt
#' @export
setMethod(
    f = "melt",
    signature = signature(object = "table"),
    definition = `melt,table`
)
