#' AcidPlyr
#'
#' A dplyr-like grammar for manipulation of S4 rectangular data classes, such
#' as DataFrame.
#'
#' @keywords internal
#'
#' @importClassesFrom AcidGenerics DataFrame
#'
#' @importFrom AcidBase appendToBody
#' @importFrom AcidCLI alert alertInfo
#' @importFrom AcidGenerics DataFrame as.data.frame decode merge na.omit order
#'   setdiff unique
#' @importFrom goalie allAreAtomic areDisjointSets assert bapply hasColnames
#'   hasDims hasLength hasNoDuplicates hasRownames isAny isCharacter isFlag
#'   isInt isNumber isScalar isString isSubset
#' @importFrom methods as is setMethod signature
#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
"_PACKAGE"
