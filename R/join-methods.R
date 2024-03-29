#' @name join
#' @inherit AcidGenerics::join
#' @note Updated 2024-01-04.
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
#' `DFrame` always preserve row names.
#'
#' @examples
#' data(join, package = "AcidTest")
#'
#' ## DFrame ====
#' x <- as(join[["members"]], "DFrame")
#' print(x)
#' y <- as(join[["instruments"]], "DFrame")
#' print(y)
#' by <- "name"
#' innerJoin(x = x, y = y, by = by)
#' leftJoin(x = x, y = y, by = by)
#' rightJoin(x = x, y = y, by = by)
#' fullJoin(x = x, y = y, by = by)
#' semiJoin(x = x, y = y, by = by)
#' antiJoin(x = x, y = y, by = by)
NULL



## Updated 2023-10-12.
`antiJoin,DFrame` <- # nolint
    function(x, y, by) {
        assert(
            hasColnames(x),
            hasColnames(y),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(setdiff(colnames(x), by), setdiff(colnames(y), by)),
            hasNoDuplicates(by),
            areDisjointSets(c(".idx", ".idy"), colnames(x)),
            areDisjointSets(c(".idx", ".idy"), colnames(y))
        )
        xBy <- x[, by, drop = FALSE]
        yBy <- y[, by, drop = FALSE]
        assert(
            identical(
                x = lapply(X = xBy, FUN = class),
                y = lapply(X = yBy, FUN = class)
            ),
            msg = sprintf(
                paste(
                    "Type mismatch of columns defined in {.var %s}",
                    "between {.var %s} and {.var %s}."
                ),
                "by", "x", "y"
            )
        )
        assert(
            allAreAtomic(xBy),
            allAreAtomic(yBy),
            msg = sprintf(
                paste(
                    "Columns defined in {.var %s} are not atomic",
                    "for both {.var %s} and {.var %s}."
                ),
                "by", "x", "y"
            )
        )
        assert(
            identical(nrow(x), nrow(unique(xBy))),
            identical(nrow(y), nrow(unique(yBy))),
            msg = sprintf(
                "Columns defined in {.var %s} argument are not unique.", "by"
            )
        )
        assert(
            all(complete.cases(xBy)),
            all(complete.cases(yBy)),
            msg = sprintf(
                "Columns defined in {.var %s} argument contain {.val %s}.",
                "by", "NA"
            )
        )
        x <- as(x, "DFrame")
        y <- as(y, "DFrame")
        x[[".idx"]] <- seq_len(nrow(x))
        y[[".idy"]] <- seq_len(nrow(y))
        m <- merge(x = x, y = y, by = by, all = FALSE, sort = FALSE)
        assert(is(m, "DFrame"))
        m <- m[, c(".idx", ".idy"), drop = FALSE]
        i <- order(setdiff(x[[".idx"]], m[[".idx"]]))
        j <- setdiff(colnames(x), ".idx")
        out <- x[i, j, drop = FALSE]
        out
    }



## Updated 2023-10-12.
`fullJoin,DFrame` <- # nolint
    function(x, y, by) {
        assert(
            hasColnames(x),
            hasColnames(y),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(setdiff(colnames(x), by), setdiff(colnames(y), by)),
            hasNoDuplicates(by),
            areDisjointSets(c(".idx", ".idy"), colnames(x)),
            areDisjointSets(c(".idx", ".idy"), colnames(y))
        )
        xBy <- x[, by, drop = FALSE]
        yBy <- y[, by, drop = FALSE]
        assert(
            identical(
                x = lapply(X = xBy, FUN = class),
                y = lapply(X = yBy, FUN = class)
            ),
            msg = sprintf(
                paste(
                    "Type mismatch of columns defined in {.var %s}",
                    "between {.var %s} and {.var %s}."
                ),
                "by", "x", "y"
            )
        )
        assert(
            allAreAtomic(xBy),
            allAreAtomic(yBy),
            msg = sprintf(
                paste(
                    "Columns defined in {.var %s} are not atomic",
                    "for both {.var %s} and {.var %s}."
                ),
                "by", "x", "y"
            )
        )
        assert(
            identical(nrow(x), nrow(unique(xBy))),
            identical(nrow(y), nrow(unique(yBy))),
            msg = sprintf(
                "Columns defined in {.var %s} argument are not unique.", "by"
            )
        )
        assert(
            all(complete.cases(xBy)),
            all(complete.cases(yBy)),
            msg = sprintf(
                "Columns defined in {.var %s} argument contain {.val %s}.",
                "by", "NA"
            )
        )
        x <- as(x, "DFrame")
        y <- as(y, "DFrame")
        x[[".idx"]] <- seq_len(nrow(x))
        y[[".idy"]] <- seq_len(nrow(y))
        out <- merge(x = x, y = y, by = by, all = TRUE, sort = FALSE)
        assert(is(out, "DFrame"))
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



## Updated 2023-10-12.
`innerJoin,DFrame` <- # nolint
    function(x, y, by) {
        assert(
            hasColnames(x),
            hasColnames(y),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(setdiff(colnames(x), by), setdiff(colnames(y), by)),
            hasNoDuplicates(by),
            areDisjointSets(c(".idx", ".idy"), colnames(x)),
            areDisjointSets(c(".idx", ".idy"), colnames(y))
        )
        xBy <- x[, by, drop = FALSE]
        yBy <- y[, by, drop = FALSE]
        assert(
            identical(
                x = lapply(X = xBy, FUN = class),
                y = lapply(X = yBy, FUN = class)
            ),
            msg = sprintf(
                paste(
                    "Type mismatch of columns defined in {.var %s}",
                    "between {.var %s} and {.var %s}."
                ),
                "by", "x", "y"
            )
        )
        assert(
            allAreAtomic(xBy),
            allAreAtomic(yBy),
            msg = sprintf(
                paste(
                    "Columns defined in {.var %s} are not atomic",
                    "for both {.var %s} and {.var %s}."
                ),
                "by", "x", "y"
            )
        )
        assert(
            identical(nrow(x), nrow(unique(xBy))),
            identical(nrow(y), nrow(unique(yBy))),
            msg = sprintf(
                "Columns defined in {.var %s} argument are not unique.", "by"
            )
        )
        assert(
            all(complete.cases(xBy)),
            all(complete.cases(yBy)),
            msg = sprintf(
                "Columns defined in {.var %s} argument contain {.val %s}.",
                "by", "NA"
            )
        )
        x <- as(x, "DFrame")
        y <- as(y, "DFrame")
        x[[".idx"]] <- seq_len(nrow(x))
        y[[".idy"]] <- seq_len(nrow(y))
        m <- merge(x = x, y = y, by = by, all = FALSE, sort = FALSE)
        assert(is(m, "DFrame"))
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



## S4Vectors (i.e. `DFrame`) doesn't support expansion via indices containing
## NAs. Will see: "Error: subscript contains NAs." in this case. Here we are
## coercing mismatched `y` to data.frame, which does allow expansion via indices
## containing NAs.

## Updated 2023-10-12.
`leftJoin,DFrame` <- # nolint
    function(x, y, by) {
        assert(
            hasColnames(x),
            hasColnames(y),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(setdiff(colnames(x), by), setdiff(colnames(y), by)),
            hasNoDuplicates(by),
            areDisjointSets(c(".idx", ".idy"), colnames(x)),
            areDisjointSets(c(".idx", ".idy"), colnames(y))
        )
        xBy <- x[, by, drop = FALSE]
        yBy <- y[, by, drop = FALSE]
        assert(
            identical(
                x = lapply(X = xBy, FUN = class),
                y = lapply(X = yBy, FUN = class)
            ),
            msg = sprintf(
                paste(
                    "Type mismatch of columns defined in {.var %s}",
                    "between {.var %s} and {.var %s}."
                ),
                "by", "x", "y"
            )
        )
        assert(
            allAreAtomic(xBy),
            allAreAtomic(yBy),
            msg = sprintf(
                paste(
                    "Columns defined in {.var %s} are not atomic",
                    "for both {.var %s} and {.var %s}."
                ),
                "by", "x", "y"
            )
        )
        ## We want to allow duplicates for xBy here.
        assert(
            identical(nrow(y), nrow(unique(yBy))),
            msg = sprintf(
                "Columns defined in {.var %s} argument are not unique.", "by"
            )
        )
        assert(
            any(complete.cases(xBy)),
            all(complete.cases(yBy)),
            msg = sprintf(
                "Columns defined in {.var %s} argument contain {.val %s}.",
                "by", "NA"
            )
        )
        x <- as(x, "DFrame")
        y <- as(y, "DFrame")
        y <- unique(y)
        x[[".idx"]] <- seq_len(nrow(x))
        y[[".idy"]] <- seq_len(nrow(y))
        m <- merge(x = x, y = y, by = by, all.x = TRUE, sort = FALSE)
        ## Handle case when x by column contains NA.
        if (!all(complete.cases(x[, by, drop = FALSE]))) {
            m <- m[!duplicated(m[[".idx"]]), , drop = FALSE]
        }
        assert(
            is(m, "DFrame"),
            identical(nrow(x), nrow(m))
        )
        m <- m[, c(".idx", ".idy"), drop = FALSE]
        y <- y[, setdiff(colnames(y), colnames(x)), drop = FALSE]
        x <- x[m[[".idx"]], , drop = FALSE]
        containsNA <- c("y" = anyNA(m[[".idy"]]))
        if (isTRUE(containsNA[["y"]])) {
            yy <- as.data.frame(y)
            assert(identical(colnames(yy), colnames(y)))
            yy <- yy[m[[".idy"]], , drop = FALSE]
            y <- as(yy, "DFrame")
        } else {
            y <- y[m[[".idy"]], , drop = FALSE]
        }
        out <- cbind(x, y)
        out <- out[order(out[[".idx"]]), , drop = FALSE]
        out[[".idx"]] <- NULL
        out[[".idy"]] <- NULL
        out
    }



## Updated 2023-04-26.
`rightJoin,DFrame` <- # nolint
    function(x, y, by) {
        leftJoin(x = y, y = x, by = by)
    }



## Updated 2023-08-25.
`semiJoin,DFrame` <- # nolint
    function(x, y, by) {
        assert(
            hasColnames(x),
            hasColnames(y),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(setdiff(colnames(x), by), setdiff(colnames(y), by)),
            hasNoDuplicates(by),
            areDisjointSets(c(".idx", ".idy"), colnames(x)),
            areDisjointSets(c(".idx", ".idy"), colnames(y))
        )
        xBy <- x[, by, drop = FALSE]
        yBy <- y[, by, drop = FALSE]
        assert(
            identical(
                x = lapply(X = xBy, FUN = class),
                y = lapply(X = yBy, FUN = class)
            ),
            msg = sprintf(
                paste(
                    "Type mismatch of columns defined in {.var %s}",
                    "between {.var %s} and {.var %s}."
                ),
                "by", "x", "y"
            )
        )
        assert(
            allAreAtomic(xBy),
            allAreAtomic(yBy),
            msg = sprintf(
                paste(
                    "Columns defined in {.var %s} are not atomic",
                    "for both {.var %s} and {.var %s}."
                ),
                "by", "x", "y"
            )
        )
        assert(
            identical(nrow(x), nrow(unique(xBy))),
            identical(nrow(y), nrow(unique(yBy))),
            msg = sprintf(
                "Columns defined in {.var %s} argument are not unique.", "by"
            )
        )
        assert(
            all(complete.cases(xBy)),
            all(complete.cases(yBy)),
            msg = sprintf(
                "Columns defined in {.var %s} argument contain {.val %s}.",
                "by", "NA"
            )
        )
        x <- as(x, "DFrame")
        y <- as(y, "DFrame")
        x[[".idx"]] <- seq_len(nrow(x))
        y[[".idy"]] <- seq_len(nrow(y))
        m <- merge(x = x, y = y, by = by, all = FALSE, sort = FALSE)
        assert(is(m, "DFrame"))
        m <- m[, c(".idx", ".idy"), drop = FALSE]
        assert(is(m, "DFrame"))
        i <- m[[".idx"]]
        j <- setdiff(colnames(x), ".idx")
        out <- x[i, j, drop = FALSE]
        out
    }



#' @rdname join
#' @export
setMethod(
    f = "antiJoin",
    signature = signature(
        x = "DFrame",
        y = "DFrame",
        by = "character"
    ),
    definition = `antiJoin,DFrame`
)

#' @rdname join
#' @export
setMethod(
    f = "fullJoin",
    signature = signature(
        x = "DFrame",
        y = "DFrame",
        by = "character"
    ),
    definition = `fullJoin,DFrame`
)

#' @rdname join
#' @export
setMethod(
    f = "innerJoin",
    signature = signature(
        x = "DFrame",
        y = "DFrame",
        by = "character"
    ),
    definition = `innerJoin,DFrame`
)

#' @rdname join
#' @export
setMethod(
    f = "leftJoin",
    signature = signature(
        x = "DFrame",
        y = "DFrame",
        by = "character"
    ),
    definition = `leftJoin,DFrame`
)

#' @rdname join
#' @export
setMethod(
    f = "rightJoin",
    signature = signature(
        x = "DFrame",
        y = "DFrame",
        by = "character"
    ),
    definition = `rightJoin,DFrame`
)

#' @rdname join
#' @export
setMethod(
    f = "semiJoin",
    signature = signature(
        x = "DFrame",
        y = "DFrame",
        by = "character"
    ),
    definition = `semiJoin,DFrame`
)
