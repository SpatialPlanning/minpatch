#' Calculate comprehensive cost summary for MinPatch solution
#'
#' Calculates various cost components using prioritizr functions where possible
#'
#' @param minpatch_data List containing all MinPatch data structures
#'
#' @return List containing detailed cost breakdown
#' @keywords internal
calculate_cost_summary <- function(minpatch_data) {

  # Create solution data for prioritizr functions using the minpatch column
  solution_data <- minpatch_data$prioritizr_solution %>%
    dplyr::select("minpatch")

  # Use prioritizr's eval_cost_summary for planning unit costs
  cost_summary <- prioritizr::eval_cost_summary(minpatch_data$prioritizr_problem, solution_data)

  # Use prioritizr's eval_n_summary for selected unit count
  n_summary <- prioritizr::eval_n_summary(minpatch_data$prioritizr_problem, solution_data)
  cost_summary$n <- n_summary$n

  # Use prioritizr's eval_boundary_summary for boundary costs if boundary penalty > 0
  if (minpatch_data$boundary_penalty > 0) {
    boundary_summary <- prioritizr::eval_boundary_summary(minpatch_data$prioritizr_problem, solution_data)
    cost_summary$boundary_length <- boundary_summary$boundary
    cost_summary$boundary_cost <- cost_summary$boundary_length * minpatch_data$boundary_penalty
  } else {
    cost_summary$boundary_length <- 0
    cost_summary$boundary_cost <- 0
  }

  cost_summary <- cost_summary %>%
    dplyr::mutate(total_cost = .data$cost + .data$boundary_cost)

  return(cost_summary)

}


#' Create solution vector from unit dictionary
#'
#' Converts the internal unit dictionary back to a binary solution vector
#'
#' @param unit_dict Named list containing cost and status for each planning unit
#'
#' @return Binary numeric vector indicating selected planning units
#' @keywords internal
create_solution_vector <- function(unit_dict) {

  n_units <- length(unit_dict)
  solution <- numeric(n_units)

  for (i in seq_len(n_units)) {
    unit_id <- as.character(i)
    if (unit_id %in% names(unit_dict)) {
      # Set to 1 if selected (status 1) or conserved (status 2)
      solution[i] <- as.numeric(unit_dict[[unit_id]]$status %in% c(1, 2))
    }
  }

  return(solution)
}

