# Release notes

## AcidPlyr 0.5.4 (2024-03-18)

Minor changes:

- `unlist2`: Added support for `NULL` definition in `nameCol` and `rownameCol`
  variables.

## AcidPlyr 0.5.3 (2023-12-12)

New functions:

- `filterNested`: Keep rows that match a nested condition. Performs recursive
  regular expression matching across all columns, including nested list columns.

## AcidPlyr 0.5.2 (2023-11-30)

New functions:

- `unlist2`: An alternative method to `unlist` for `DFrameList` that does a
  better job of keeping track of `names` and `rownames` in the original object,
  which makes conversion into long format simpler.

## AcidPlyr 0.5.1 (2023-10-12)

Minor changes:

- Tightened up assert checks in join functions for columns defined in `"by"`
  argument. Improved error messages on type mismatches and non-atomic input.

## AcidPlyr 0.5.0 (2023-10-03)

Major changes:

- New release series, indicating significant changes in Acid Genomics dependency
  packages.

Minor changes:

- `rbindToDataFrame`: Added an additional assert check.

## AcidPlyr 0.4.2 (2023-08-26)

Minor changes:

- `leftJoin` and `rightJoin` now allow for `NA` values defined in the `by`
  column for the `x` side of the join. This was required for OncoTree metadata
  additions in the pending Cellosaurus package update.
- All join operations now check to see if the `by` columns are atomic.
- `unnest2` now uses parallell `mclapply` for the main unnest step, which speeds
  up expansion of large data frames significantly. We have added an alert
  message for data frames with at least 1000 rows.

## AcidPlyr 0.4.1 (2023-08-24)

Minor changes:

- `unnest2`: Reworked internal code on `SplitDFrame` that now only attempts to
  recycle rows when length is greater than 1. Additionally, the function now
  consistently sets `NA` when length is 0. Removed unnecessary internal call
  to `unlist` -- we can just access the values directly.

## AcidPlyr 0.4.0 (2023-08-23)

New functions:

- `unnest2`: This is a simpler variant of tidyr `unnest` that is intended to
  work directly on S4 `DFrame` without coercion to `data.frame`. It supports
  unnesting of a single list-column by design, and is intended to be as
  simple as possible.

Major changes:

- `cast`: No longer requires tidyr internally. We reworked the internal code of
  this function to use base R syntax with full Bioconductor S4 class support.
  No internal coercion to `data.frame` is performed.

## AcidPlyr 0.3.11 (2023-08-10)

Major changes:

- Now requiring R 4.3.

Minor changes:

- `cast`: Now using tidy-select internally with `{{`. This is now required for
  breaking change update with tidyselect 1.2.0.
- Unit tests are now performed in parallel.

## AcidPlyr 0.3.10 (2023-04-27)

Minor changes:

- `melt`: Need to dynamically use Matrix `rowSums` generic internally when
  applicable, to avoid issues parsing `sparseMatrix` objects.

## AcidPlyr 0.3.9 (2023-04-26)

Minor changes:

- Dispatch on `DFrame` instead of `DataFrame` virtual class.

## AcidPlyr 0.3.8 (2023-04-25)

Minor changes:

- No longer importing `rowSums` into NAMESPACE, which has been removed from
  BiocGenerics in Bioconductor 3.17 update.

## AcidPlyr 0.3.7 (2023-02-23)

Minor changes:

- `rbindToDataFrame`: Simplified internal construction of `DataFrame` from
  `list`, using `new` instead of `matrix` coercion internally. This same
  approach is now used for `as.DataFrame` `list` method in pipette.
- `leftJoin` and other join functions: Improved error message handling when
  `x` and `y` contain overlapping columns not defined in `by` argument.

## AcidPlyr 0.3.6 (2023-02-10)

Major changes:

- `rbindToDataFrame`: Improved nesting of list columns.

## AcidPlyr 0.3.5 (2023-02-09)

Major changes:

- Removed `mapToDataFrame`, in favor of only supporting `rbindToDataFrame`
  function. This helps lighten the package a bit by removing suggested
  purrr and dplyr packages.

## AcidPlyr 0.3.4 (2023-02-08)

Minor changes:

- `rbindToDataFrame`: Simplified internal logic to speed up function quite a
  bit. Now supports processing of `"cellosaurus.txt"` file.

## AcidPlyr 0.3.3 (2023-02-07)

Major changes:

- `rbindToDataFrame`: Reworked internal code. For very large nested lists,
  consider using data.table `rbindlist` instead, which is very performant.

Minor changes:

- Hardened internal assert checks for join functions, to protect against
  unwanted duplicate or `NA` values defined in `"by"` argument.
- Now using `requireNamespaces` internally (from goalie) for optional package
  checks.

## AcidPlyr 0.3.2 (2022-09-16)

New functions:

- `cast`: Added support for casting `DataFrame` objects in long format back to
  wide format. Internally this calls `tidyr::pivot_wider` currently. The
  function is intentionally designed to be simple and provide easy
  interconversion support for `melt` return in long format back to wide format.
  For more advanced uses, call `pivot_wider` directly instead.

## AcidPlyr 0.3.1 (2022-05-23)

Minor changes:

- Updated lintr checks and testthat unit tests.

## AcidPlyr 0.3.0 (2022-04-29)

Major changes:

- Updated R dependency to 4.2.
- Reduced the number of strong dependencies in the package, migrating AcidCLI
  and pipette to `Suggests` instead of `Imports`.

## AcidPlyr 0.2.0 (2022-03-11)

Major changes:

- NAMESPACE updates, reverting back to importing BiocGenerics and S4Vectors.

Minor changes:

- Updated minimum dependencies to R 4.1 / Bioconductor 3.14.
- No longer reexporting `DataFrame` function.

## AcidPlyr 0.1.22 (2021-09-08)

Minor changes:

- Removed deprecated `unlistToDataFrame` function from NAMESPACE.
- Updated basejump dependency versions.

## AcidPlyr 0.1.21 (2021-09-03)

Minor changes:

- Now using `abort` function internally instead of `stop` for better stylized
  error messages.
- Miscellaneous documentation updates, reducing the number of links.

## AcidPlyr 0.1.20 (2021-05-18)

Minor changes:

- Need to import `as.DataFrame` from pipette to improve consistency of `list`
  to `DataFrame` coercion, which is currently tricky on Bioconductor 3.13.

## AcidPlyr 0.1.19 (2021-05-18)

Minor changes:

- `mapToDataFrame`: Hardened function in the case when dplyr is not installed.
  Internally the `purrr::map_df` call hands off to dplyr.

## AcidPlyr 0.1.18 (2021-04-27)

Minor changes:

- `mapToDataFrame`: Offloading purrr as a suggested package, since this is the
  only function that calls `map_dfr` internally.
- Removed `map_df` and `map_dfr` as reexported functions.

## AcidPlyr 0.1.17 (2021-03-10)

Minor changes:

- `leftJoin`: Improve error message when user attempts to pass in columns via
  "by" argument that are not unique.

## AcidPlyr 0.1.16 (2021-03-03)

Minor changes:

- Join methods now coerce objects to `DataFrame`, to avoid unwanted issues with
  classed objects.
- `fullJoin` return now retains metadata from `x`, as expected.

## AcidPlyr 0.1.15 (2021-03-02)

Minor changes:

- `mutateAll`: Attempting to remove dependency on internal `as_tibble` method,
  so we can preserve complex S4 columns.

## AcidPlyr 0.1.14 (2021-02-24)

Minor changes:

- NAMESPACE fixes for downstream packages that attempt to use melt method on
  sparse matrix. Now calling some functions from AcidGenerics that mask base
  variants.

## AcidPlyr 0.1.13 (2021-02-20)

New functions:

- Added `rbindToDataFrame`, which is an incredibly useful utility for ensuring
  that nested list elements return 1:1 per row. The function handles complex
  S4 classes such as `IntegerList` and nested ranges.

Minor changes:

- Renamed `unlistToDataFrame` to `mapToDataFrame`, which better matches the
  conventions used in other Acid Genomics packages, as well as purrr.

## AcidPlyr 0.1.12 (2021-02-19)

Minor changes:

- `unlistToDataFrame`: Improved internal code to handle input of `CharacterList`
  and `IntegerList` properly. Reworked internal call to `purrr::map_dfr`.

## AcidPlyr 0.1.11 (2021-02-13)

Minor changes:

- Improved internal code for join methods.

## AcidPlyr 0.1.10 (2021-02-12)

Minor changes:

- Tightened up dependency requirements.

## AcidPlyr 0.1.9 (2021-02-11)

Minor changes:

- Updated reexports to include some useful purrr map reexports.

## AcidPlyr 0.1.8 (2021-02-02)

New functions:

- Migrated `collapseToString` from basejump.

## AcidPlyr 0.1.7 (2021-01-31)

Minor changes:

- Tightened up internal code of join functions.

## AcidPlyr 0.1.6 (2021-01-06)

Minor changes:

- Migrated internal dependency from cli to AcidCLI.

## AcidPlyr 0.1.5 (2021-01-06)

New functions:

- Added new `unlistToDataFrame` utility, inspired by approach used in purrr
  package. Note that package now imports purrr.

## AcidPlyr 0.1.4 (2020-10-12)

Minor changes:

- `mutateAt`: Bug fix to improve internal `list` to `DataFrame` coercion.

## AcidPlyr 0.1.3 (2020-10-12)

Minor changes:

- `melt`: Improved internal code for contingency `table` method support.

## AcidPlyr 0.1.2 (2020-10-12)

Minor changes:

- `melt`: Added back `table` method support, which is used in pointillism.

## AcidPlyr 0.1.1 (2020-10-07)

New functions:

- Migrated functions previously defined in basejump: `melt`, `mutateAll`,
  `mutateIf`, `transmuteAt`, `transmuteIf`, `mutateAt`, `selectIf`.

Minor changes:

- `melt`: Restricted method to only support `matrix` and `DataFrame` here.
  Additional S4 methods that work on `SummarizedExperiment` and
  `SingleCellExperiment` are defined in basejump.

## AcidPlyr 0.1.0 (2020-10-06)

Initial release.

New functions:

- Migrated the join functions from basejump: `innerJoin`, `leftJoin`,
  `rightJoin`, `fullJoin`, `semiJoin`, `antiJoin`.
