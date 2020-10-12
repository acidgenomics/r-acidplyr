## AcidPlyr 0.1.3 (2020-10-12)

### Minor changes

- `melt`: Improved internal code for contingency `table` method support.

## AcidPlyr 0.1.2 (2020-10-12)

### Minor changes

- `melt`: Added back `table` method support, which is used in pointillism.

## AcidPlyr 0.1.1 (2020-10-07)

### New functions

- Migrated functions previously defined in basejump: `melt`, `mutateAll`,
  `mutateIf`, `transmuteAt`, `transmuteIf`, `mutateAt`, `selectIf`.

### Minor changes

- `melt`: Restricted method to only support `matrix` and `DataFrame` here.
  Additional S4 methods that work on `SummarizedExperiment` and
  `SingleCellExperiment` are defined in basejump.

## AcidPlyr 0.1.0 (2020-10-06)

Initial release.

### New functions

- Migrated the join functions from basejump: `innerJoin`, `leftJoin`,
  `rightJoin`, `fullJoin`, `semiJoin`, `antiJoin`.
