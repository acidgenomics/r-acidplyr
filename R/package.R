#' AcidPlyr
#'
#' A dplyr-like grammar for manipulation of S4 rectangular data classes, such
#' as DataFrame.
#'
#' @keywords internal
#'
#' @importClassesFrom AcidGenerics AsIs DataFrame
#'
#' @importFrom AcidCLI alert alertInfo
#' @importFrom AcidGenerics DataFrame SimpleList as.data.frame cbind decode
#'   do.call expand.grid lapply merge metadata metadata<- na.omit order rbind
#'   rowSums setdiff unique
#' @importFrom goalie allAreAtomic areDisjointSets assert bapply hasColnames
#'   hasDims hasLength hasNames hasNoDuplicates hasRownames isAny isCharacter
#'   isFlag isInt isNumber isScalar isScalarAtomic isString isSubset
#' @importFrom methods as is setMethod signature
"_PACKAGE"
