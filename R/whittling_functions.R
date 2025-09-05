#' Simulated whittling to remove unnecessary planning units
#'
#' Stage 3 of MinPatch: Remove planning units that are not needed to meet targets,
#' reduce fragmentation, or meet minimum patch size requirements
#'
#' @param unit_dict Named list containing cost and status for each planning unit
#' @param minpatch_data List containing all MinPatch data structures
#' @param min_patch_size Minimum patch size threshold
#' @param verbose Logical, whether to print progress
#'
#' @return Updated unit_dict with unnecessary units removed
#' @keywords internal
simulated_whittling <- function(unit_dict, minpatch_data, min_patch_size, verbose = TRUE) {
  
  boundary_matrix <- minpatch_data$boundary_matrix
  abundance_matrix <- minpatch_data$abundance_matrix
  target_dict <- minpatch_data$target_dict
  area_dict <- minpatch_data$area_dict
  boundary_penalty <- minpatch_data$boundary_penalty
  
  iteration <- 0
  max_iterations <- 10000  # Prevent infinite loops
  keystone_pu_id_set <- character(0)  # Initialize keystone set
  
  while (iteration < max_iterations) {
    iteration <- iteration + 1
    
    if (verbose && iteration %% 100 == 0) {
      cat("  Whittling iteration", iteration, "\n")
    }
    
    # Find edge planning units (units on the boundary of selected areas)
    edge_units <- find_edge_units(unit_dict, boundary_matrix)
    
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
    
    # Calculate whittling scores for edge units
    whittle_scores_raw <- calculate_whittle_scores(
      edge_units, unit_dict, abundance_matrix, target_dict, minpatch_data
    )
    
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
    
    # Check if removing this unit is acceptable
    removal_result <- can_remove_unit(candidate_unit, unit_dict, minpatch_data, min_patch_size)
    if (removal_result) {
      # Remove the unit
      unit_dict[[candidate_unit]]$status <- 0
      
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
  
  return(unit_dict)
}

#' Find edge planning units
#'
#' Identifies planning units that are on the edge of selected areas
#' (have at least one unselected neighbor)
#'
#' @param unit_dict Named list containing cost and status for each planning unit
#' @param boundary_matrix Named list containing adjacency information
#'
#' @return Character vector of edge unit IDs
#' @keywords internal
find_edge_units <- function(unit_dict, boundary_matrix) {
  
  edge_units <- character(0)
  
  for (unit_id in names(unit_dict)) {
    if (unit_dict[[unit_id]]$status == 1) {  # Only consider selected units
      
      # Check if this unit has any unselected neighbors
      neighbors <- names(boundary_matrix[[unit_id]])
      
      for (neighbor_id in neighbors) {
        if (neighbor_id != unit_id && neighbor_id %in% names(unit_dict)) {
          neighbor_status <- unit_dict[[neighbor_id]]$status
          
          # If neighbor is available (0) or excluded (3), this is an edge unit
          if (neighbor_status %in% c(0, 3)) {
            edge_units <- c(edge_units, unit_id)
            break
          }
        } else if (neighbor_id == unit_id) {
          # Self-reference indicates external boundary
          edge_units <- c(edge_units, unit_id)
          break
        }
      }
    }
  }
  
  return(unique(edge_units))
}

#' Calculate whittling scores for edge units
#'
#' Calculates the "Low Relevance" score for each edge unit based on
#' feature importance (Equation A2 from the original paper)
#'
#' @param edge_units Character vector of edge unit IDs
#' @param unit_dict Named list containing cost and status for each planning unit
#' @param abundance_matrix Named list containing feature abundances
#' @param target_dict Named list containing feature targets
#' @param minpatch_data List containing all MinPatch data structures
#'
#' @return Named vector of whittling scores
#' @keywords internal
calculate_whittle_scores <- function(edge_units, unit_dict, abundance_matrix,
                                   target_dict, minpatch_data) {
  
  # Calculate current feature conservation amounts
  feature_amounts <- calculate_feature_conservation(unit_dict, abundance_matrix, target_dict)
  
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
#' 3. Doesn't increase total Marxan cost
#' 4. Doesn't split patches into non-viable pieces
#'
#' @param unit_id ID of unit to potentially remove
#' @param unit_dict Named list containing cost and status for each planning unit
#' @param minpatch_data List containing all MinPatch data structures
#' @param min_patch_size Minimum patch size threshold
#'
#' @return Logical indicating if unit can be removed
#' @keywords internal
can_remove_unit <- function(unit_id, unit_dict, minpatch_data, min_patch_size) {
  
  # Don't remove conserved units (status 2)
  if (unit_dict[[unit_id]]$status == 2) {
    return(FALSE)
  }
  
  # Check if removal would violate conservation targets
  if (removal_violates_targets(unit_id, unit_dict, minpatch_data)) {
    return(FALSE)
  }
  
  # Check if removal would make patch too small
  if (removal_makes_patch_too_small(unit_id, unit_dict, minpatch_data, min_patch_size)) {
    return(FALSE)
  }
  
  # Check if removal would increase Marxan cost
  if (removal_increases_marxan_cost(unit_id, unit_dict, minpatch_data)) {
    return(FALSE)
  }
  
  # Check if removal would split patch into non-viable pieces
  if (removal_splits_patch_nonviably(unit_id, unit_dict, minpatch_data, min_patch_size)) {
    return(FALSE)
  }
  
  return(TRUE)
}

#' Check if removing unit would violate conservation targets
#'
#' @param unit_id ID of unit to potentially remove
#' @param unit_dict Named list containing cost and status for each planning unit
#' @param minpatch_data List containing all MinPatch data structures
#'
#' @return Logical indicating if removal would violate targets
#' @keywords internal
removal_violates_targets <- function(unit_id, unit_dict, minpatch_data) {
  
  abundance_matrix <- minpatch_data$abundance_matrix
  target_dict <- minpatch_data$target_dict
  
  # Calculate current feature amounts
  feature_amounts <- calculate_feature_conservation(unit_dict, abundance_matrix, target_dict)
  
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
#' @param unit_dict Named list containing cost and status for each planning unit
#' @param minpatch_data List containing all MinPatch data structures
#' @param min_patch_size Minimum patch size threshold
#'
#' @return Logical indicating if removal would make patch too small
#' @keywords internal
removal_makes_patch_too_small <- function(unit_id, unit_dict, minpatch_data, min_patch_size) {
  
  area_dict <- minpatch_data$area_dict
  boundary_matrix <- minpatch_data$boundary_matrix
  
  # Find the patch this unit belongs to
  patch_dict <- make_patch_dict(unit_dict, boundary_matrix)
  
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

#' Check if removing unit would increase Marxan cost
#'
#' @param unit_id ID of unit to potentially remove
#' @param unit_dict Named list containing cost and status for each planning unit
#' @param minpatch_data List containing all MinPatch data structures
#'
#' @return Logical indicating if removal would increase cost
#' @keywords internal
removal_increases_marxan_cost <- function(unit_id, unit_dict, minpatch_data) {
  
  boundary_matrix <- minpatch_data$boundary_matrix
  boundary_penalty <- minpatch_data$boundary_penalty
  
  if (boundary_penalty == 0) {
    return(FALSE)  # No boundary cost to consider
  }
  
  unit_cost <- unit_dict[[unit_id]]$cost
  
  # Calculate change in boundary cost
  neighbors <- names(boundary_matrix[[unit_id]])
  boundary_change <- 0
  
  for (neighbor_id in neighbors) {
    if (neighbor_id %in% names(unit_dict)) {
      neighbor_status <- unit_dict[[neighbor_id]]$status
      boundary_length <- boundary_matrix[[unit_id]][[neighbor_id]]
      
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
#' @param unit_dict Named list containing cost and status for each planning unit
#' @param minpatch_data List containing all MinPatch data structures
#' @param min_patch_size Minimum patch size threshold
#'
#' @return Logical indicating if removal would create non-viable patches
#' @keywords internal
removal_splits_patch_nonviably <- function(unit_id, unit_dict, minpatch_data, min_patch_size) {
  
  boundary_matrix <- minpatch_data$boundary_matrix
  area_dict <- minpatch_data$area_dict
  
  # Create a temporary unit_dict without this unit
  temp_unit_dict <- unit_dict
  temp_unit_dict[[unit_id]]$status <- 0
  
  # Find patches in the modified solution
  new_patch_dict <- make_patch_dict(temp_unit_dict, boundary_matrix)
  
  # Check if any new patches are too small
  for (patch_id in names(new_patch_dict)) {
    patch_area <- sum(area_dict[new_patch_dict[[patch_id]]$unit_ids])
    if (as.numeric(patch_area) < as.numeric(min_patch_size)) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}