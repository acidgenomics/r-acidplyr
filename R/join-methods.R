#' @name join
#' @inherit AcidGenerics::join
#' @note Updated 2021-01-29.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @section Ordering:
#'
#' The join functions never rearrange rows. To accomplish this, we're currently
#' setting an internal `.idx` column that we can use to reorder the rows after
#' [`merge()`][base::merge] operation.
#'
#' @section Row names:
#'
#' Unlike the S3 methods defined in dplyr, the join methods defined here for
#' `DataFrame` always preserve row names.
#'
#' @examples
#' data(band_members, band_instruments, package = "AcidTest")
#'
#' ## DataFrame ====
#' x <- as(band_members, "DataFrame")
#' y <- as(band_instruments, "DataFrame")
#' print(x)
#' print(y)
#' by <- "name"
#' innerJoin(x = x, y = y, by = by)
#' leftJoin(x = x, y = y, by = by)
#' rightJoin(x = x, y = y, by = by)
#' fullJoin(x = x, y = y, by = by)
#' semiJoin(x = x, y = y, by = by)
#' antiJoin(x = x, y = y, by = by)
NULL



## Updated 2021-01-24.
`innerJoin,DataFrame` <-  # nolint
    function(x, y, by) {
        assert(
            hasColnames(x),
            hasColnames(y),
            isSubset(x = by, y = colnames(x)),
            isSubset(x = by, y = colnames(y)),
            identical(
                x = intersect(x = colnames(x), y = colnames(y)),
                y = by
            ),
            areDisjointSets(
                x = setdiff(colnames(x), by),
                y = setdiff(colnames(y), by)
            ),
            hasNoDuplicates(by),
            areDisjointSets(
                x = c(".idx", ".idy"),
                y = colnames(x)
            ),
            areDisjointSets(
                x = c(".idx", ".idy"),
                y = colnames(y)
            )
        )
        x[[".idx"]] <- seq_len(nrow(x))
        out <- merge(x = x, y = y, by = by, all = FALSE, sort = FALSE)
        out <- out[order(out[[".idx"]]), , drop = FALSE]
        if (hasRownames(x)) {
            rownames(out) <- rownames(x)[out[[".idx"]]]
        }
        out <- out[, setdiff(colnames(out), ".idx"), drop = FALSE]
        out
    }



#' @rdname join
#' @export
setMethod(
    f = "innerJoin",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame",
        by = "character"
    ),
    definition = `innerJoin,DataFrame`
)



## Updated 2021-02-12.
`leftJoin,DataFrame` <-  # nolint
    function(x, y, by) {
        assert(
            hasColnames(x),
            hasColnames(y),
            isSubset(x = by, y = colnames(x)),
            isSubset(x = by, y = colnames(y)),
            identical(
                x = intersect(x = colnames(x), y = colnames(y)),
                y = by
            ),
            areDisjointSets(
                x = setdiff(x = colnames(x), y = by),
                y = setdiff(x = colnames(y), y = by)
            ),
            hasNoDuplicates(by),
            areDisjointSets(
                x = c(".idx", ".idy"),
                y = colnames(x)
            ),
            areDisjointSets(
                x = c(".idx", ".idy"),
                y = colnames(y)
            )
        )
        y <- unique(y)
        x[[".idx"]] <- seq_len(nrow(x))
        y[[".idy"]] <- seq_len(nrow(y))
        m <- merge(x = x, y = y, by = by, all.x = TRUE, sort = FALSE)
        assert(identical(nrow(x), nrow(m)))
        m <- m[, c(".idx", ".idy"), drop = FALSE]
        x <- x[m[[".idx"]], , drop = FALSE]
        y <- y[, setdiff(colnames(y), colnames(x)), drop = FALSE]
        ## S4Vectors doesn't support indices containing NAs.
        ## Will see: `Error: subscript contains NAs.` in this case.
        if (any(is.na(m[[".idy"]]))) {
            yy <- as.data.frame(y)
            assert(identical(colnames(yy), colnames(y)))
            y <- yy
        }
        y <- y[m[[".idy"]], , drop = FALSE]
        out <- cbind(x, y)
        out[[".idx"]] <- NULL
        out[[".idy"]] <- NULL
        rownames(out) <- rownames(x)
        out
    }



#' @rdname join
#' @export
setMethod(
    f = "leftJoin",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame",
        by = "character"
    ),
    definition = `leftJoin,DataFrame`
)



## Updated 2020-10-06.
`rightJoin,DataFrame` <-  # nolint
    function(x, y, by) {
        leftJoin(x = y, y = x, by = by)
    }



#' @rdname join
#' @export
setMethod(
    f = "rightJoin",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame",
        by = "character"
    ),
    definition = `rightJoin,DataFrame`
)



## Updated 2021-01-29.
`fullJoin,DataFrame` <-  # nolint
    function(x, y, by) {
        assert(
            hasColnames(x),
            hasColnames(y),
            isSubset(x = by, y = colnames(x)),
            isSubset(x = by, y = colnames(y)),
            identical(
                x = intersect(x = colnames(x), y = colnames(y)),
                y = by
            ),
            areDisjointSets(
                x = setdiff(x = colnames(x), y = by),
                y = setdiff(x = colnames(y), y = by)
            ),
            hasNoDuplicates(by),
            areDisjointSets(
                x = c(".idx", ".idy"),
                y = colnames(x)
            ),
            areDisjointSets(
                x = c(".idx", ".idy"),
                y = colnames(y)
            )
        )
        x[[".idx"]] <- seq_len(nrow(x))
        y[[".idy"]] <- seq_len(nrow(y))
        out <- merge(x = x, y = y, by = by, all = TRUE, sort = FALSE)
        out <- out[order(out[[".idx"]], out[[".idy"]]), , drop = FALSE]
        if (hasRownames(x) && hasRownames(y)) {
            rnx <- rownames(x)[na.omit(out[[".idx"]])]
            rny <- rownames(y)[na.omit(out[[".idy"]])]
            rn <- unique(c(rnx, rny))
            assert(hasLength(rn, n = nrow(out)))
            rownames(out) <- rn
        }
        out <- out[, setdiff(colnames(out), c(".idx", ".idy")), drop = FALSE]
        out
    }



#' @rdname join
#' @export
setMethod(
    f = "fullJoin",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame",
        by = "character"
    ),
    definition = `fullJoin,DataFrame`
)



## Updated 2021-01-29.
`semiJoin,DataFrame` <-  # nolint
    function(x, y, by) {
        assert(
            hasColnames(x),
            hasColnames(y),
            isSubset(x = by, y = colnames(x)),
            isSubset(x = by, y = colnames(y)),
            identical(
                x = intersect(x = colnames(x), y = colnames(y)),
                y = by
            ),
            areDisjointSets(
                x = setdiff(x = colnames(x), y = by),
                y = setdiff(x = colnames(y), y = by)
            ),
            hasNoDuplicates(by),
            areDisjointSets(
                x = c(".idx", ".idy"),
                y = colnames(x)
            ),
            areDisjointSets(
                x = c(".idx", ".idy"),
                y = colnames(y)
            )
        )
        x[[".idx"]] <- seq_len(nrow(x))
        m <- merge(x = x, y = y, by = by, all = FALSE, sort = FALSE)
        which <- m[[".idx"]]
        out <- x[which, setdiff(colnames(x), ".idx"), drop = FALSE]
        out
    }



#' @rdname join
#' @export
setMethod(
    f = "semiJoin",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame",
        by = "character"
    ),
    definition = `semiJoin,DataFrame`
)



## Updated 2021-01-29.
`antiJoin,DataFrame` <-  # nolint
    function(x, y, by) {
        assert(
            hasColnames(x),
            hasColnames(y),
            isSubset(x = by, y = colnames(x)),
            isSubset(x = by, y = colnames(y)),
            identical(
                x = intersect(x = colnames(x), y = colnames(y)),
                y = by
            ),
            areDisjointSets(
                x = setdiff(x = colnames(x), y = by),
                y = setdiff(x = colnames(y), y = by)
            ),
            hasNoDuplicates(by),
            areDisjointSets(
                x = c(".idx", ".idy"),
                y = colnames(x)
            ),
            areDisjointSets(
                x = c(".idx", ".idy"),
                y = colnames(y)
            )
        )
        x[[".idx"]] <- seq_len(nrow(x))
        m <- merge(x = x, y = y, by = by, all = FALSE, sort = FALSE)
        which <- order(setdiff(x[[".idx"]], m[[".idx"]]))
        out <- x[which, setdiff(colnames(x), ".idx"), drop = FALSE]
        out
    }



#' @rdname join
#' @export
setMethod(
    f = "antiJoin",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame",
        by = "character"
    ),
    definition = `antiJoin,DataFrame`
)
