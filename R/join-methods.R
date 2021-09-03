#' @name join
#' @inherit AcidGenerics::join
#' @note Updated 2021-09-03.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @section Ordering:
#'
#' The join functions never rearrange rows. To accomplish this, we're currently
#' setting an internal `.idx` column that we can use to reorder the rows after
#' `merge()` operation.
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
#' print(x)
#' y <- as(band_instruments, "DataFrame")
#' print(y)
#' by <- "name"
#' innerJoin(x = x, y = y, by = by)
#' leftJoin(x = x, y = y, by = by)
#' rightJoin(x = x, y = y, by = by)
#' fullJoin(x = x, y = y, by = by)
#' semiJoin(x = x, y = y, by = by)
#' antiJoin(x = x, y = y, by = by)
NULL



## Updated 2021-03-03.
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
        x <- as(x, "DataFrame")
        y <- as(y, "DataFrame")
        x[[".idx"]] <- seq_len(nrow(x))
        y[[".idy"]] <- seq_len(nrow(y))
        m <- merge(x = x, y = y, by = by, all = FALSE, sort = FALSE)
        assert(is(m, "DataFrame"))
        m <- m[, c(".idx", ".idy"), drop = FALSE]
        y <- y[, setdiff(colnames(y), colnames(x)), drop = FALSE]
        x <- x[m[[".idx"]], , drop = FALSE]
        y <- y[m[[".idy"]], , drop = FALSE]
        out <- cbind(x, y)
        out <- out[order(out[[".idx"]]), , drop = FALSE]
        out[[".idx"]] <- NULL
        out[[".idy"]] <- NULL
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



## Updated 2021-03-03.
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
        x <- as(x, "DataFrame")
        y <- as(y, "DataFrame")
        y <- unique(y)
        assert(
            identical(
                x = nrow(y),
                y = nrow(unique(y[, by, drop = FALSE]))
            ),
            msg = sprintf(
                "Columns defined in '%s' argument are not unique.", "by"
            )
        )
        x[[".idx"]] <- seq_len(nrow(x))
        y[[".idy"]] <- seq_len(nrow(y))
        m <- merge(x = x, y = y, by = by, all.x = TRUE, sort = FALSE)
        assert(
            is(m, "DataFrame"),
            identical(nrow(x), nrow(m))
        )
        m <- m[, c(".idx", ".idy"), drop = FALSE]
        y <- y[, setdiff(colnames(y), colnames(x)), drop = FALSE]
        x <- x[m[[".idx"]], , drop = FALSE]
        ## S4Vectors (i.e. DataFrame) doesn't support expansion via indices
        ## containing NAs. Will see: "Error: subscript contains NAs." in this
        ## case. Here we are coercing mismatched `y` to data.frame, which does
        ## allow expansion via indices containing NAs.
        containsNA <- c("y" = any(is.na(m[[".idy"]])))
        if (isTRUE(containsNA[["y"]])) {
            yy <- as.data.frame(y)
            assert(identical(colnames(yy), colnames(y)))
            yy <- yy[m[[".idy"]], , drop = FALSE]
            y <- as(yy, "DataFrame")
        } else {
            y <- y[m[[".idy"]], , drop = FALSE]
        }
        out <- cbind(x, y)
        out <- out[order(out[[".idx"]]), , drop = FALSE]
        out[[".idx"]] <- NULL
        out[[".idy"]] <- NULL
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



## Updated 2021-03-03.
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
        x <- as(x, "DataFrame")
        y <- as(y, "DataFrame")
        x[[".idx"]] <- seq_len(nrow(x))
        y[[".idy"]] <- seq_len(nrow(y))
        out <- merge(x = x, y = y, by = by, all = TRUE, sort = FALSE)
        assert(is(out, "DataFrame"))
        out <- out[order(out[[".idx"]], out[[".idy"]]), , drop = FALSE]
        if (hasRownames(x) && hasRownames(y)) {
            rnx <- rownames(x)[order(out[[".idx"]])]
            rnx <- gsub(pattern = "NA", replacement = NA, x = rnx)
            rny <- rownames(y)[order(out[[".idy"]])]
            rny <- gsub(pattern = "NA", replacement = NA, x = rny)
            rn <- unique(c(na.omit(rnx), na.omit(rny)))
            assert(hasLength(rn, n = nrow(out)))
            rownames(out) <- rn
        } else {
            rownames(out) <- NULL
        }
        out[[".idx"]] <- NULL
        out[[".idy"]] <- NULL
        metadata(out) <- metadata(x)
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



## Updated 2021-03-03.
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
        x <- as(x, "DataFrame")
        y <- as(y, "DataFrame")
        x[[".idx"]] <- seq_len(nrow(x))
        y[[".idy"]] <- seq_len(nrow(y))
        m <- merge(x = x, y = y, by = by, all = FALSE, sort = FALSE)
        assert(is(m, "DataFrame"))
        m <- m[, c(".idx", ".idy"), drop = FALSE]
        assert(is(m, "DataFrame"))
        rows <- m[[".idx"]]
        cols <- setdiff(colnames(x), ".idx")
        out <- x[rows, cols, drop = FALSE]
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



## Updated 2021-03-03.
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
        x <- as(x, "DataFrame")
        y <- as(y, "DataFrame")
        x[[".idx"]] <- seq_len(nrow(x))
        y[[".idy"]] <- seq_len(nrow(y))
        m <- merge(x = x, y = y, by = by, all = FALSE, sort = FALSE)
        assert(is(m, "DataFrame"))
        m <- m[, c(".idx", ".idy"), drop = FALSE]
        rows <- order(setdiff(x[[".idx"]], m[[".idx"]]))
        cols <- setdiff(colnames(x), ".idx")
        out <- x[rows, cols, drop = FALSE]
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
