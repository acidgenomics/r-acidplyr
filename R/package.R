#' AcidPlyr
#'
#' A dplyr-like grammar for data manipulation of S4 classes defined
#' in S4Vectors.
#'
#' @keywords internal
"_PACKAGE"



#' @importClassesFrom S4Vectors DataFrame
#'
#' @importFrom AcidBase appendToBody
#' @importFrom AcidCLI alert alertInfo
#' @importFrom AcidGenerics decode merge na.omit order setdiff unique
#' @importFrom S4Vectors DataFrame
#' @importFrom goalie allAreAtomic areDisjointSets assert bapply hasColnames
#'   hasDims hasLength hasNoDuplicates hasRownames isAny isCharacter isFlag
#'   isInt isNumber isScalar isString isSubset
#' @importFrom methods as setMethod signature
#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
NULL
