## Classes =====================================================================
#' @importClassesFrom BiocGenerics AsIs
#' @importClassesFrom S4Vectors DataFrame
NULL



## S4 generics =================================================================
#' @importFrom AcidGenerics antiJoin as.DataFrame collapseToString fullJoin
#'   innerJoin leftJoin mapToDataFrame melt mutateAll mutateAt mutateIf
#'   rbindToDataFrame rightJoin selectIf semiJoin splitByLevel transmuteAt
#'   transmuteIf
#' @importFrom BiocGenerics as.data.frame cbind do.call lapply order rbind
#'   rowSums setdiff unique
#' @importFrom S4Vectors decode expand.grid merge metadata metadata<- na.omit
#'
#' @importMethodsFrom pipette as.DataFrame
NULL



## Standard functions ==========================================================
#' @importFrom AcidBase requireNamespaces
#' @importFrom AcidCLI alert alertInfo
#' @importFrom S4Vectors DataFrame SimpleList
#' @importFrom goalie allAreAtomic areDisjointSets assert bapply hasColnames
#'   hasDims hasLength hasNames hasNoDuplicates hasRownames isAny isCharacter
#'   isFlag isInt isNumber isScalar isScalarAtomic isString isSubset
#' @importFrom methods as is setMethod signature
NULL
