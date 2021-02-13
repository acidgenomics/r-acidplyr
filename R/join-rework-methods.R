## Updated 2021-02-12.
leftJoin2 <-  # nolint
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
