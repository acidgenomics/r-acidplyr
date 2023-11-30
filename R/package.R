#' AcidPlyr
#'
#' A dplyr-like grammar for manipulation of S4 rectangular data classes, such
#' as `DFrame`.
#'
#' @keywords internal
"_PACKAGE"



## Classes =====================================================================

#' @importClassesFrom IRanges DFrameList
#' @importClassesFrom S4Vectors DFrame
NULL



## S4 generics =================================================================

#' @importFrom AcidGenerics antiJoin cast collapseToString fullJoin innerJoin
#' leftJoin melt mutateAll mutateAt mutateIf rbindToDataFrame rightJoin selectIf
#' semiJoin splitByLevel transmuteAt transmuteIf unlist2 unnest2
#' @importFrom BiocGenerics as.data.frame cbind do.call lapply order rbind
#' setdiff unique unlist
#' @importFrom S4Vectors complete.cases decode expand.grid merge metadata
#' metadata<- na.omit split
NULL



## Standard functions ==========================================================

#' @importFrom S4Vectors DataFrame
#' @importFrom goalie allAreAtomic areDisjointSets assert bapply hasColnames
#' hasDims hasLength hasNames hasNoDuplicates hasRownames hasRows isAny
#' isCharacter isFlag isInstalled isInt isNumber isScalar isString isSubset
#' requireNamespaces
#' @importFrom methods as is new setMethod signature
#' @importFrom parallel mclapply
NULL
