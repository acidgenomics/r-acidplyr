#' AcidPlyr
#'
#' A dplyr-like grammar for manipulation of S4 rectangular data classes, such
#' as DataFrame.
#'
#' @keywords internal
#'
#' @importClassesFrom BiocGenerics AsIs
#' @importClassesFrom S4Vectors DataFrame
#'
#' @importMethodsFrom pipette as.DataFrame
#'
#' @importFrom AcidBase requireNamespaces
#' @importFrom AcidCLI alert alertInfo
#' @importFrom AcidGenerics as.DataFrame
#' @importFrom BiocGenerics as.data.frame cbind do.call lapply order rbind
#'   rowSums setdiff unique
#' @importFrom S4Vectors DataFrame SimpleList decode expand.grid merge metadata
#'   metadata<- na.omit
#' @importFrom goalie allAreAtomic areDisjointSets assert bapply hasColnames
#'   hasDims hasLength hasNames hasNoDuplicates hasRownames isAny isCharacter
#'   isFlag isInt isNumber isScalar isScalarAtomic isString isSubset
#' @importFrom methods as is setMethod signature
"_PACKAGE"
