#' Calculate comprehensive cost summary for MinPatch solution
#'
#' Calculates various cost components using prioritizr functions where possible
#'
#' @param prioritizr_problem A prioritizr problem object
#' @param solution_data sf object with solution data
#' @param unit_dict Named list containing cost and status for each planning unit (for compatibility)
#' @param boundary_penalty Boundary length modifier (BLM) value
#'
#' @return List containing detailed cost breakdown
#' @keywords internal
calculate_cost_summary <- function(prioritizr_problem = NULL, solution_data = NULL,
                                 unit_dict = NULL, boundary_penalty = 0) {
  
  # If prioritizr objects are available, use prioritizr functions
  if (!is.null(prioritizr_problem) && !is.null(solution_data)) {
    
    # Use prioritizr's eval_cost_summary for planning unit costs
    cost_summary <- prioritizr::eval_cost_summary(prioritizr_problem, solution_data)
    total_unit_cost <- cost_summary$cost
    
    # Use prioritizr's eval_n_summary for selected unit count
    n_summary <- prioritizr::eval_n_summary(prioritizr_problem, solution_data)
    selected_unit_count <- n_summary$n
    
    # Use prioritizr's eval_boundary_summary for boundary costs if boundary penalty > 0
    if (boundary_penalty > 0) {
      boundary_summary <- prioritizr::eval_boundary_summary(prioritizr_problem, solution_data)
      total_boundary_length <- boundary_summary$boundary
      total_boundary_cost <- total_boundary_length * boundary_penalty
    } else {
      total_boundary_length <- 0
      total_boundary_cost <- 0
    }
    
    total_cost <- total_unit_cost + total_boundary_cost
    
    return(list(
      total_unit_cost = total_unit_cost,
      selected_unit_count = selected_unit_count,
      total_boundary_length = total_boundary_length,
      total_boundary_cost = total_boundary_cost,
      total_cost = total_cost,
      boundary_penalty = boundary_penalty
    ))
    
  } else if (!is.null(unit_dict)) {
    # Fallback to original implementation for backward compatibility
    
    # Calculate total planning unit cost
    total_unit_cost <- 0
    selected_unit_count <- 0
    
    for (unit_id in names(unit_dict)) {
      if (unit_dict[[unit_id]]$status %in% c(1, 2)) {  # Selected or conserved
        total_unit_cost <- total_unit_cost + unit_dict[[unit_id]]$cost
        selected_unit_count <- selected_unit_count + 1
      }
    }
    
    # For boundary costs, we'd need the boundary_matrix - this is a limitation
    # of the fallback approach
    total_boundary_length <- 0
    total_boundary_cost <- 0
    total_cost <- total_unit_cost
    
    return(list(
      total_unit_cost = total_unit_cost,
      selected_unit_count = selected_unit_count,
      total_boundary_length = total_boundary_length,
      total_boundary_cost = total_boundary_cost,
      total_cost = total_cost,
      boundary_penalty = boundary_penalty
    ))
    
  } else {
    stop("Either prioritizr_problem and solution_data, or unit_dict must be provided")
  }
}

# Note: calculate_boundary_costs() function removed - now using prioritizr::eval_boundary_summary()
# This reduces code duplication and ensures consistency with prioritizr calculations

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

#' Calculate feature representation in solution
#'
#' Calculates how much of each conservation feature is represented
#' in the current solution using prioritizr functions where possible
#'
#' @param minpatch_data List containing all MinPatch data structures including prioritizr objects
#'
#' @return Data frame with feature representation statistics
#' @export
calculate_feature_representation <- function(minpatch_data) {
  
  # If prioritizr objects are available, use prioritizr functions
  if (!is.null(minpatch_data$prioritizr_problem) && !is.null(minpatch_data$solution_data)) {
    
    # Use prioritizr's eval_feature_representation_summary
    feature_rep <- prioritizr::eval_feature_representation_summary(minpatch_data$prioritizr_problem, minpatch_data$solution_data)
    
    # Use prioritizr's eval_target_coverage_summary for target information
    target_coverage <- prioritizr::eval_target_coverage_summary(minpatch_data$prioritizr_problem, minpatch_data$solution_data)
    
    # Combine the results to match the expected output format
    results <- data.frame(
      feature_id = seq_len(nrow(feature_rep)),
      target = target_coverage$target,
      conserved = feature_rep$absolute_held,
      proportion_met = feature_rep$relative_held,
      target_met = target_coverage$met,
      shortfall = pmax(0, target_coverage$target - feature_rep$absolute_held),
      stringsAsFactors = FALSE
    )
    
    return(results)
    
  } else {
    # Fallback to original implementation using minpatch_data components
    
    # Calculate current conservation amounts
    feature_amounts <- calculate_feature_conservation(minpatch_data)
    
    # Create results data frame
    results <- data.frame(
      feature_id = names(minpatch_data$target_dict),
      target = sapply(minpatch_data$target_dict, function(x) x$target),
      conserved = feature_amounts[names(minpatch_data$target_dict)],
      stringsAsFactors = FALSE
    )
    
    # Calculate proportion of target met
    results$proportion_met <- ifelse(results$target > 0,
                                    results$conserved / results$target,
                                    NA)
    
    # Identify if target is met
    results$target_met <- results$conserved >= results$target
    
    # Calculate shortfall
    results$shortfall <- pmax(0, results$target - results$conserved)
    
    return(results)
  }
}

#' Generate comprehensive MinPatch report
#'
#' Creates a detailed report of the MinPatch processing results
#'
#' @param minpatch_result Result object from run_minpatch function
#' @param prioritizr_problem A prioritizr problem object (optional, for enhanced reporting)
#' @param solution_data sf object with solution data (optional, for enhanced reporting)
#'
#' @return List containing formatted report components
#' @export
generate_minpatch_report <- function(minpatch_result, prioritizr_problem = NULL, solution_data = NULL) {
  
  # Extract components
  initial_stats <- minpatch_result$patch_stats$initial
  final_stats <- minpatch_result$patch_stats$final
  cost_summary <- minpatch_result$cost_summary
  
  # Calculate feature representation using prioritizr functions if available
  if (!is.null(prioritizr_problem) && !is.null(solution_data)) {
    # Create temporary minpatch_data with prioritizr objects
    temp_minpatch_data <- minpatch_result$minpatch_data
    temp_minpatch_data$prioritizr_problem <- prioritizr_problem
    temp_minpatch_data$solution_data <- solution_data
    feature_rep <- calculate_feature_representation(temp_minpatch_data)
  } else {
    # Fallback to original method
    feature_rep <- calculate_feature_representation(minpatch_result$minpatch_data)
  }
  
  # Summary statistics
  summary_stats <- list(
    initial_patches = initial_stats$all_patch_count,
    final_patches = final_stats$all_patch_count,
    initial_valid_patches = initial_stats$valid_patch_count,
    final_valid_patches = final_stats$valid_patch_count,
    initial_area = initial_stats$all_patch_area,
    final_area = final_stats$all_patch_area,
    area_change = final_stats$all_patch_area - initial_stats$all_patch_area,
    area_change_percent = ifelse(initial_stats$all_patch_area > 0,
                                ((final_stats$all_patch_area - initial_stats$all_patch_area) / 
                                 initial_stats$all_patch_area) * 100, 0),
    total_cost = cost_summary$total_cost,
    unit_cost = cost_summary$total_unit_cost,
    boundary_cost = cost_summary$total_boundary_cost,
    selected_units = cost_summary$selected_unit_count
  )
  
  # Feature summary
  feature_summary <- list(
    total_features = nrow(feature_rep),
    targets_met = sum(feature_rep$target_met, na.rm = TRUE),
    targets_unmet = sum(!feature_rep$target_met, na.rm = TRUE),
    mean_proportion_met = mean(feature_rep$proportion_met, na.rm = TRUE),
    total_shortfall = sum(feature_rep$shortfall, na.rm = TRUE)
  )
  
  return(list(
    summary_stats = summary_stats,
    feature_summary = feature_summary,
    feature_representation = feature_rep,
    patch_stats = list(
      initial = initial_stats,
      final = final_stats
    ),
    cost_breakdown = cost_summary
  ))
}

#' Print MinPatch results summary
#'
#' Prints a formatted summary of MinPatch processing results
#'
#' @param minpatch_result Result object from run_minpatch function
#'
#' @export
print_minpatch_summary <- function(minpatch_result) {
  
  report <- generate_minpatch_report(minpatch_result)
  
  cat("=== MinPatch Processing Summary ===\n\n")
  
  # Patch statistics
  cat("Patch Statistics:\n")
  cat(sprintf("  Initial patches: %d (valid: %d)\n", 
              report$summary_stats$initial_patches,
              report$summary_stats$initial_valid_patches))
  cat(sprintf("  Final patches: %d (valid: %d)\n", 
              report$summary_stats$final_patches,
              report$summary_stats$final_valid_patches))
  cat(sprintf("  Area change: %.2f (%.1f%%)\n", 
              report$summary_stats$area_change,
              report$summary_stats$area_change_percent))
  cat("\n")
  
  # Cost breakdown
  cat("Cost Breakdown:\n")
  cat(sprintf("  Planning unit cost: %.2f\n", report$summary_stats$unit_cost))
  cat(sprintf("  Boundary cost: %.2f\n", report$summary_stats$boundary_cost))
  cat(sprintf("  Total cost: %.2f\n", report$summary_stats$total_cost))
  cat(sprintf("  Selected units: %d\n", report$summary_stats$selected_units))
  cat("\n")
  
  # Feature representation
  cat("Feature Representation:\n")
  cat(sprintf("  Total features: %d\n", report$feature_summary$total_features))
  cat(sprintf("  Targets met: %d\n", report$feature_summary$targets_met))
  cat(sprintf("  Targets unmet: %d\n", report$feature_summary$targets_unmet))
  cat(sprintf("  Mean proportion met: %.3f\n", report$feature_summary$mean_proportion_met))
  cat(sprintf("  Total shortfall: %.2f\n", report$feature_summary$total_shortfall))
  cat("\n")
  
  if (report$feature_summary$targets_unmet > 0) {
    cat("Features with unmet targets:\n")
    unmet <- report$feature_representation[!report$feature_representation$target_met, ]
    for (i in seq_len(nrow(unmet))) {
      cat(sprintf("  Feature %s: %.2f/%.2f (%.1f%% met)\n",
                  unmet$feature_id[i],
                  unmet$conserved[i],
                  unmet$target[i],
                  unmet$proportion_met[i] * 100))
    }
  }
  
  cat("\n=== End Summary ===\n")
}