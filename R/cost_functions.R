#' Calculate comprehensive cost summary for MinPatch solution
#'
#' Calculates various cost components including planning unit costs,
#' boundary costs, and feature penalty costs
#'
#' @param unit_dict Named list containing cost and status for each planning unit
#' @param cost_dict Named vector of planning unit costs
#' @param boundary_matrix Named list containing adjacency and boundary lengths
#' @param boundary_penalty Boundary length modifier (BLM) value
#'
#' @return List containing detailed cost breakdown
#' @keywords internal
calculate_cost_summary <- function(unit_dict, cost_dict, boundary_matrix, boundary_penalty) {
  
  # Calculate total planning unit cost
  total_unit_cost <- 0
  selected_unit_count <- 0
  
  for (unit_id in names(unit_dict)) {
    if (unit_dict[[unit_id]]$status %in% c(1, 2)) {  # Selected or conserved
      total_unit_cost <- total_unit_cost + unit_dict[[unit_id]]$cost
      selected_unit_count <- selected_unit_count + 1
    }
  }
  
  # Calculate boundary costs
  boundary_results <- calculate_boundary_costs(unit_dict, boundary_matrix, boundary_penalty)
  
  # Calculate total cost
  total_cost <- total_unit_cost + boundary_results$total_boundary_cost
  
  return(list(
    total_unit_cost = total_unit_cost,
    selected_unit_count = selected_unit_count,
    total_boundary_length = boundary_results$total_boundary_length,
    total_boundary_cost = boundary_results$total_boundary_cost,
    total_cost = total_cost,
    boundary_penalty = boundary_penalty
  ))
}

#' Calculate boundary costs for current solution
#'
#' Calculates the total boundary length and associated costs based on
#' the Marxan boundary cost calculation method
#'
#' @param unit_dict Named list containing cost and status for each planning unit
#' @param boundary_matrix Named list containing adjacency and boundary lengths
#' @param boundary_penalty Boundary length modifier (BLM) value
#'
#' @return List containing boundary length and cost
#' @keywords internal
calculate_boundary_costs <- function(unit_dict, boundary_matrix, boundary_penalty) {
  
  total_boundary_length <- 0
  processed_pairs <- character(0)
  
  for (unit_id1 in names(unit_dict)) {
    unit1_status <- unit_dict[[unit_id1]]$status
    
    if (unit1_status %in% c(1, 2)) {  # Unit 1 is selected
      neighbors <- boundary_matrix[[unit_id1]]
      
      for (unit_id2 in names(neighbors)) {
        boundary_length <- neighbors[[unit_id2]]
        
        # Create a unique pair identifier to avoid double counting
        pair_id <- paste(sort(c(unit_id1, unit_id2)), collapse = "_")
        
        if (!pair_id %in% processed_pairs) {
          processed_pairs <- c(processed_pairs, pair_id)
          
          if (unit_id1 == unit_id2) {
            # Self-boundary (external edge)
            total_boundary_length <- total_boundary_length + boundary_length
          } else if (unit_id2 %in% names(unit_dict)) {
            unit2_status <- unit_dict[[unit_id2]]$status
            
            # Count boundary if exactly one unit is selected
            selected_count <- sum(c(unit1_status, unit2_status) %in% c(1, 2))
            if (selected_count == 1) {
              total_boundary_length <- total_boundary_length + boundary_length
            }
          }
        }
      }
    }
  }
  
  total_boundary_cost <- total_boundary_length * boundary_penalty
  
  return(list(
    total_boundary_length = total_boundary_length,
    total_boundary_cost = total_boundary_cost
  ))
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

#' Calculate feature representation in solution
#'
#' Calculates how much of each conservation feature is represented
#' in the current solution
#'
#' @param unit_dict Named list containing cost and status for each planning unit
#' @param abundance_matrix Named list containing feature abundances
#' @param target_dict Named list containing feature targets
#'
#' @return Data frame with feature representation statistics
#' @export
calculate_feature_representation <- function(unit_dict, abundance_matrix, target_dict) {
  
  # Calculate current conservation amounts
  feature_amounts <- calculate_feature_conservation(unit_dict, abundance_matrix, target_dict)
  
  # Create results data frame
  results <- data.frame(
    feature_id = names(target_dict),
    target = sapply(target_dict, function(x) x$target),
    conserved = feature_amounts[names(target_dict)],
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

#' Generate comprehensive MinPatch report
#'
#' Creates a detailed report of the MinPatch processing results
#'
#' @param minpatch_result Result object from run_minpatch function
#'
#' @return List containing formatted report components
#' @export
generate_minpatch_report <- function(minpatch_result) {
  
  # Extract components
  initial_stats <- minpatch_result$patch_stats$initial
  final_stats <- minpatch_result$patch_stats$final
  cost_summary <- minpatch_result$cost_summary
  
  # Calculate feature representation
  feature_rep <- calculate_feature_representation(
    minpatch_result$minpatch_data$unit_dict,
    minpatch_result$minpatch_data$abundance_matrix,
    minpatch_result$minpatch_data$target_dict
  )
  
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