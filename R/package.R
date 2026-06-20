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

#' @importFrom AcidGenerics antiJoin cast collapseToString filterNested
#' @importFrom AcidGenerics fullJoin innerJoin leftJoin melt mutateAll
#' @importFrom AcidGenerics mutateAt mutateIf rbindToDataFrame rightJoin
#' @importFrom AcidGenerics selectIf semiJoin splitByLevel transmuteAt
#' @importFrom AcidGenerics transmuteIf unlist2 unnest2
#' @importFrom BiocGenerics as.data.frame cbind do.call grepl lapply
#' @importFrom BiocGenerics order rbind setdiff unique unlist
#' @importFrom S4Vectors complete.cases decode expand.grid merge metadata
#' @importFrom S4Vectors "metadata<-" na.omit split
NULL


## Standard functions ==========================================================

#' @importFrom IRanges CharacterList
#' @importFrom S4Vectors DataFrame
#' @importFrom goalie allAreAtomic allAreMatchingRegex areDisjointSets assert
#' @importFrom goalie bapply hasColnames hasDims hasLength hasNames
#' @importFrom goalie hasNoDuplicates hasRownames hasRows isAny isCharacter
#' @importFrom goalie isFlag isInstalled isInt isNumber isScalar isString
#' @importFrom goalie isSubset requireNamespaces
#' @importFrom methods as is new setMethod signature
#' @importFrom parallel mclapply
NULL
