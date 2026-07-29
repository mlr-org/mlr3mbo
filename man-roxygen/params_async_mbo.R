#' @section Parameters:
#' \describe{
#' \item{`initial_design`}{`data.table::data.table()`\cr
#'   Initial design of the optimization.
#'   If `NULL`, a design of size `design_size` is generated with the specified `design_function`.
#'   Default is `NULL`.}
#' \item{`design_size`}{`integer(1)`\cr
#'   Size of the initial design if it is to be generated.
#'   Default is `100`.}
#' \item{`design_function`}{`character(1)`\cr
#'   Sampling function to generate the initial design.
#'   Can be `random` [paradox::generate_design_random], `lhs` [paradox::generate_design_lhs],
#'   or `sobol` [paradox::generate_design_sobol].
#'   Default is `sobol`.}
#' \item{`n_workers`}{`integer(1)`\cr
#'   Number of parallel workers.
#'   If `NULL`, all rush workers specified via [rush::rush_plan()] are used.
#'   Default is `NULL`.}
#' \item{`profiles`}{named `integer()`\cr
#'   Number of parallel workers per \CRANpkg{mirai} compute profile, e.g. `c(cpu = 2, gpu = 2)`.
#'   The daemons of every profile must be created with [mirai::daemons()] beforehand.
#'   Cannot be combined with `n_workers`.
#'   If `NULL`, the profiles specified via [rush::rush_plan()] are used.
#'   Default is `NULL`.}
#' }
