#' Simulated whittling to remove unnecessary planning units
#'
#' Stage 3 of MinPatch: Remove planning units that are not needed to meet targets,
#' reduce fragmentation, or meet minimum patch size requirements
#'
#' @param minpatch_data List containing all MinPatch data structures (including prioritizr objects)
#' @param verbose Logical, whether to print progress
#'
#' @return Updated minpatch_data with unnecessary units removed
#' @keywords internal
simulated_whittling <- function(minpatch_data, verbose = TRUE) {

  unit_dict <- minpatch_data$unit_dict
  boundary_matrix <- minpatch_data$boundary_matrix
  abundance_matrix <- minpatch_data$abundance_matrix
  target_dict <- minpatch_data$target_dict
  area_dict <- minpatch_data$area_dict
  boundary_penalty <- minpatch_data$boundary_penalty
  min_patch_size <- minpatch_data$min_patch_size

  iteration <- 0
  max_iterations <- 10000  # Prevent infinite loops
  keystone_pu_id_set <- character(0)  # Initialize keystone set
  
  # OPTIMIZATION: Cache feature amounts and only update when unit is removed
  feature_amounts <- calculate_feature_conservation(minpatch_data)

  while (iteration < max_iterations) {
    iteration <- iteration + 1

    if (verbose && iteration %% 100 == 0) {
      cat("  Whittling iteration", iteration, "\n")
    }

    # Find edge planning units (units on the boundary of selected areas)
    edge_units <- find_edge_units(minpatch_data)

    # Remove keystone units from edge units
    edge_units <- setdiff(edge_units, keystone_pu_id_set)

    if (verbose && iteration <= 5) {
      cat("    Edge units found:", length(edge_units), "\n")
      cat("    Keystone units:", length(keystone_pu_id_set), "\n")
    }

    if (length(edge_units) == 0) {
      if (verbose) cat("  No more edge units to consider - terminating\n")
      break  # No more edge units to consider
    }

    # Calculate whittling scores for edge units (pass cached feature amounts)
    whittle_scores_raw <- calculate_whittle_scores(edge_units, minpatch_data, feature_amounts)

    # Separate keystone units (needed for targets) from scoreable units
    whittle_scores <- list()
    new_keystone_count <- 0
    for (unit_id in names(whittle_scores_raw)) {
      score <- whittle_scores_raw[[unit_id]]
      if (is.character(score) && score == "PU cannot be whittled, as needed to meet targets") {
        keystone_pu_id_set <- c(keystone_pu_id_set, unit_id)
        new_keystone_count <- new_keystone_count + 1
      } else {
        whittle_scores[[unit_id]] <- as.numeric(score)
      }
    }

    if (verbose && iteration <= 5) {
      cat("    New keystone units:", new_keystone_count, "\n")
      cat("    Scoreable units:", length(whittle_scores), "\n")
    }

    if (length(whittle_scores) == 0) {
      if (verbose) cat("  No units can be removed - all are keystone - terminating\n")
      break  # No units can be removed
    }

    # Find the unit with the lowest whittling score (most suitable for removal)
    candidate_unit <- names(whittle_scores)[which.min(unlist(whittle_scores))]

    # Check if removing this unit is acceptable (pass cached feature amounts)
    removal_result <- can_remove_unit(candidate_unit, minpatch_data, feature_amounts)
    if (removal_result) {
      # Remove the unit
      unit_dict[[candidate_unit]]$status <- 0
      
      # OPTIMIZATION: Update cached feature amounts incrementally instead of recalculating
      unit_abundances <- abundance_matrix[[candidate_unit]]
      for (feat_id in names(unit_abundances)) {
        if (feat_id %in% names(feature_amounts)) {
          feature_amounts[feat_id] <- feature_amounts[feat_id] - unit_abundances[[feat_id]]
        }
      }

      if (verbose && iteration <= 10) {
        cat("    Removed unit", candidate_unit, "at iteration", iteration, "\n")
      }
    } else {
      # Add to keystone set and remove from consideration
      keystone_pu_id_set <- c(keystone_pu_id_set, candidate_unit)

      if (verbose && iteration <= 10) {
        cat("    Unit", candidate_unit, "cannot be removed - adding to keystone set\n")
      }
    }
  }

  if (iteration >= max_iterations) {
    warning("Maximum iterations reached in simulated_whittling")
  }

  # Update minpatch_data with final unit_dict and minpatch column
  minpatch_data$unit_dict <- unit_dict
  minpatch_data$prioritizr_solution$minpatch <- create_solution_vector(unit_dict)

  return(minpatch_data)
}

#' Find edge planning units
#'
#' Identifies planning units that are on the edge of selected areas
#' (have at least one unselected neighbor). Optimized with vector pre-allocation.
#'
#' @param minpatch_data List containing all MinPatch data structures
#'
#' @return Character vector of edge unit IDs
#' @keywords internal
find_edge_units <- function(minpatch_data) {

  unit_dict <- minpatch_data$unit_dict
  boundary_matrix <- minpatch_data$boundary_matrix
  
  n_units <- length(unit_dict)
  
  # Pre-allocate for worst case (all selected units are edge units)
  edge_units <- character(n_units)
  edge_count <- 0

  for (unit_id in names(unit_dict)) {
    if (unit_dict[[unit_id]]$status %in% c(1, 2)) {  # Consider both selected (1) and conserved (2) units

      # Get neighbors from sparse matrix (units with non-zero boundary)
      unit_idx <- as.integer(unit_id)
      neighbor_indices <- which(boundary_matrix[unit_idx, ] > 0)
      neighbor_ids <- colnames(boundary_matrix)[neighbor_indices]

      is_edge <- FALSE
      for (neighbor_id in neighbor_ids) {
        if (neighbor_id != unit_id && neighbor_id %in% names(unit_dict)) {
          neighbor_status <- unit_dict[[neighbor_id]]$status

          # If neighbor is available (0) or excluded (3), this is an edge unit
          if (neighbor_status %in% c(0, 3)) {
            is_edge <- TRUE
            break
          }
        } else if (neighbor_id == unit_id) {
          # Self-reference (diagonal) indicates external boundary
          is_edge <- TRUE
          break
        }
      }
      
      if (is_edge) {
        edge_count <- edge_count + 1
        edge_units[edge_count] <- unit_id
      }
    }
  }
  
  # Trim to actual size and return unique values
  return(unique(edge_units[seq_len(edge_count)]))
}


#' Calculate whittling scores for edge units
#'
#' Calculates the "Low Relevance" score for each edge unit based on
#' feature importance (Equation A2 from the original paper).
#' Optimized to accept pre-computed feature amounts to avoid redundant calculations.
#'
#' @param edge_units Character vector of edge unit IDs
#' @param minpatch_data List containing all MinPatch data structures
#' @param feature_amounts Optional pre-computed feature conservation amounts
#'
#' @return Named vector of whittling scores
#' @keywords internal
calculate_whittle_scores <- function(edge_units, minpatch_data, feature_amounts = NULL) {

  unit_dict <- minpatch_data$unit_dict
  abundance_matrix <- minpatch_data$abundance_matrix
  target_dict <- minpatch_data$target_dict

  # Calculate current feature conservation amounts only if not provided
  if (is.null(feature_amounts)) {
    feature_amounts <- calculate_feature_conservation(minpatch_data)
  }

  scores <- list()

  for (unit_id in edge_units) {
    unit_abundances <- abundance_matrix[[unit_id]]

    if (length(unit_abundances) == 0) {
      # Unit has no features, can be removed easily
      scores[[unit_id]] <- 0
      next
    }

    # Check if this unit is needed to meet any targets
    is_needed <- FALSE
    feature_scores <- numeric(0)

    for (feat_id in names(unit_abundances)) {
      if (feat_id %in% names(target_dict)) {
        feat_amount <- unit_abundances[[feat_id]]
        target_amount <- target_dict[[feat_id]]$target
        current_amount <- feature_amounts[feat_id]

        # Check if removing this unit would cause target to be unmet
        if (target_amount > 0 && (current_amount - feat_amount) < target_amount) {
          is_needed <- TRUE
          break
        }

        # Calculate feature score for whittling
        if (current_amount > target_amount && target_amount > 0) {
          feat_score <- feat_amount / (current_amount - target_amount)
          feature_scores <- c(feature_scores, feat_score)
        }
      }
    }

    if (is_needed) {
      # This unit is needed to meet targets - return special value like Python
      scores[[unit_id]] <- "PU cannot be whittled, as needed to meet targets"
    } else if (length(feature_scores) > 0) {
      scores[[unit_id]] <- max(feature_scores)
    } else {
      scores[[unit_id]] <- 0
    }
  }

  return(scores)
}



#' Check if a planning unit can be removed
#'
#' Checks multiple criteria to determine if removing a unit is acceptable:
#' 1. Doesn't violate conservation targets
#' 2. Doesn't make any patch too small
#' 3. Doesn't increase total cost
#' 4. Doesn't split patches into non-viable pieces
#'
#' @param unit_id ID of unit to potentially remove
#' @param minpatch_data List containing all MinPatch data structures
#'
#' @return Logical indicating if unit can be removed
#' @keywords internal
can_remove_unit <- function(unit_id, minpatch_data, feature_amounts = NULL) {

  unit_dict <- minpatch_data$unit_dict
  min_patch_size <- minpatch_data$min_patch_size

  # Don't remove conserved units (status 2)
  if (unit_dict[[unit_id]]$status == 2) {
    return(FALSE)
  }

  # Check if removal would violate conservation targets (pass cached feature amounts)
  if (removal_violates_targets(unit_id, minpatch_data, feature_amounts)) {
    return(FALSE)
  }

  # Check if removal would make patch too small
  if (removal_makes_patch_too_small(unit_id, minpatch_data)) {
    return(FALSE)
  }

  # Check if removal would increase cost
  if (removal_increases_cost(unit_id, minpatch_data)) {
    return(FALSE)
  }

  # Check if removal would split patch into non-viable pieces
  if (removal_splits_patch_nonviably(unit_id, minpatch_data)) {
    return(FALSE)
  }

  return(TRUE)
}

#' Check if removing unit would violate conservation targets
#'
#' Optimized to accept pre-computed feature amounts to avoid redundant calculations.
#'
#' @param unit_id ID of unit to potentially remove
#' @param minpatch_data List containing all MinPatch data structures
#' @param feature_amounts Optional pre-computed feature conservation amounts
#'
#' @return Logical indicating if removal would violate targets
#' @keywords internal
removal_violates_targets <- function(unit_id, minpatch_data, feature_amounts = NULL) {

  unit_dict <- minpatch_data$unit_dict
  abundance_matrix <- minpatch_data$abundance_matrix
  target_dict <- minpatch_data$target_dict

  # Calculate current feature amounts only if not provided
  if (is.null(feature_amounts)) {
    feature_amounts <- calculate_feature_conservation(minpatch_data)
  }

  # Get features in this unit
  unit_abundances <- abundance_matrix[[unit_id]]

  for (feat_id in names(unit_abundances)) {
    if (feat_id %in% names(target_dict)) {
      feat_amount <- unit_abundances[[feat_id]]
      target_amount <- target_dict[[feat_id]]$target
      current_amount <- feature_amounts[feat_id]

      # Check if removing this unit would cause target to be unmet
      if (target_amount > 0 && (current_amount - feat_amount) < target_amount) {
        return(TRUE)
      }
    }
  }

  return(FALSE)
}

#' Check if removing unit would make its patch too small
#'
#' @param unit_id ID of unit to potentially remove
#' @param minpatch_data List containing all MinPatch data structures
#'
#' @return Logical indicating if removal would make patch too small
#' @keywords internal
removal_makes_patch_too_small <- function(unit_id, minpatch_data) {

  unit_dict <- minpatch_data$unit_dict
  area_dict <- minpatch_data$area_dict
  boundary_matrix <- minpatch_data$boundary_matrix
  min_patch_size <- minpatch_data$min_patch_size

  # Find the patch this unit belongs to
  patch_dict <- make_patch_dict(minpatch_data)

  # Find which patch contains this unit
  unit_patch_id <- NULL
  for (patch_id in names(patch_dict)) {
    if (unit_id %in% patch_dict[[patch_id]]$unit_ids) {
      unit_patch_id <- patch_id
      break
    }
  }

  if (is.null(unit_patch_id)) {
    return(FALSE)  # Unit not in any patch
  }

  # Calculate patch area without this unit
  patch_units <- patch_dict[[unit_patch_id]]$unit_ids
  remaining_area <- sum(area_dict[setdiff(patch_units, unit_id)])

  return(as.numeric(remaining_area) < as.numeric(min_patch_size))
}

#' Check if removing unit would increase cost
#'
#' @param unit_id ID of unit to potentially remove
#' @param minpatch_data List containing all MinPatch data structures
#'
#' @return Logical indicating if removal would increase cost
#' @keywords internal
removal_increases_cost <- function(unit_id, minpatch_data) {

  unit_dict <- minpatch_data$unit_dict
  boundary_matrix <- minpatch_data$boundary_matrix
  boundary_penalty <- minpatch_data$boundary_penalty

  if (boundary_penalty == 0) {
    return(FALSE)  # No boundary cost to consider
  }

  unit_cost <- unit_dict[[unit_id]]$cost

  # Get neighbors from sparse matrix
  unit_idx <- as.integer(unit_id)
  neighbor_indices <- which(boundary_matrix[unit_idx, ] > 0)
  neighbor_ids <- colnames(boundary_matrix)[neighbor_indices]
  
  # Calculate change in boundary cost
  boundary_change <- 0

  for (neighbor_id in neighbor_ids) {
    if (neighbor_id %in% names(unit_dict)) {
      neighbor_status <- unit_dict[[neighbor_id]]$status
      neighbor_idx <- as.integer(neighbor_id)
      boundary_length <- boundary_matrix[unit_idx, neighbor_idx]

      if (neighbor_status %in% c(1, 2)) {
        # Removing unit increases boundary (neighbor becomes edge)
        boundary_change <- boundary_change + boundary_length
      } else if (neighbor_status %in% c(0, 3)) {
        # Removing unit decreases boundary
        boundary_change <- boundary_change - boundary_length
      }
    }
  }

  boundary_cost_change <- boundary_change * boundary_penalty
  total_cost_change <- boundary_cost_change - unit_cost

  return(total_cost_change > 0)
}

#' Check if removing unit would split patch into non-viable pieces
#'
#' @param unit_id ID of unit to potentially remove
#' @param minpatch_data List containing all MinPatch data structures
#'
#' @return Logical indicating if removal would create non-viable patches
#' @keywords internal
removal_splits_patch_nonviably <- function(unit_id, minpatch_data) {

  unit_dict <- minpatch_data$unit_dict
  boundary_matrix <- minpatch_data$boundary_matrix
  area_dict <- minpatch_data$area_dict
  min_patch_size <- minpatch_data$min_patch_size

  # Create a temporary minpatch_data without this unit
  temp_minpatch_data <- minpatch_data
  temp_minpatch_data$unit_dict <- unit_dict
  temp_minpatch_data$unit_dict[[unit_id]]$status <- 0

  # Find patches in the modified solution
  new_patch_dict <- make_patch_dict(temp_minpatch_data)

  # Check if any new patches are too small
  for (patch_id in names(new_patch_dict)) {
    patch_area <- sum(area_dict[new_patch_dict[[patch_id]]$unit_ids])
    if (as.numeric(patch_area) < as.numeric(min_patch_size)) {
      return(TRUE)
    }
  }

  return(FALSE)
}
