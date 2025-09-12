#' Validate MinPatch inputs
#'
#' Internal function to validate all inputs to the MinPatch algorithm
#'
#' @param solution Binary solution vector
#' @param planning_units sf object with planning units
#' @param features sf object or data.frame with features
#' @param targets data.frame with targets
#' @param costs numeric vector of costs
#' @param min_patch_size minimum patch size
#' @param patch_radius patch radius for adding patches
#' @param boundary_penalty boundary length modifier
#'
#' @return NULL (throws errors if validation fails)
#' @keywords internal
validate_inputs <- function(solution, planning_units, features, targets, costs,
                           min_patch_size, patch_radius, boundary_penalty) {
  
  # Check solution
  if (!is.numeric(solution) || !all(solution %in% c(0, 1))) {
    stop("Solution must be a binary numeric vector (0s and 1s)")
  }
  
  # Check planning units
  if (!inherits(planning_units, "sf")) {
    stop("planning_units must be an sf object")
  }
  
  if (nrow(planning_units) != length(solution)) {
    stop("Number of planning units must match length of solution vector")
  }
  
  # Check for required columns in planning units
  required_cols <- c("geometry")
  if (!all(required_cols %in% names(planning_units))) {
    stop("planning_units must contain geometry column")
  }
  
  # Check targets
  if (!is.data.frame(targets)) {
    stop("targets must be a data.frame")
  }
  
  required_target_cols <- c("feature_id", "target")
  if (!all(required_target_cols %in% names(targets))) {
    stop("targets must contain 'feature_id' and 'target' columns")
  }
  
  # Check numeric parameters (handle units objects from sf)
  min_patch_size_numeric <- as.numeric(min_patch_size)
  if (!is.numeric(min_patch_size_numeric) || min_patch_size_numeric <= 0) {
    stop("min_patch_size must be a positive number")
  }
  
  patch_radius_numeric <- as.numeric(patch_radius)
  if (!is.numeric(patch_radius_numeric) || patch_radius_numeric <= 0) {
    stop("patch_radius must be a positive number")
  }
  
  if (!is.numeric(boundary_penalty) || boundary_penalty < 0) {
    stop("boundary_penalty must be a non-negative number")
  }
  
  # Check costs if provided
  if (!is.null(costs)) {
    if (!is.numeric(costs) || length(costs) != length(solution)) {
      stop("costs must be a numeric vector with same length as solution")
    }
    if (any(costs < 0)) {
      stop("costs must be non-negative")
    }
  }
}

#' Initialize MinPatch data structures
#'
#' Creates the internal data structures needed for MinPatch processing
#'
#' @param solution Binary solution vector
#' @param planning_units sf object with planning units
#' @param features sf object or data.frame with features
#' @param targets data.frame with targets
#' @param costs numeric vector of costs
#' @param min_patch_size minimum patch size
#' @param patch_radius patch radius
#' @param boundary_penalty boundary length modifier
#' @param prioritizr_problem A prioritizr problem object
#' @param prioritizr_solution A solved prioritizr solution object
#'
#' @return List containing all necessary data structures
#' @keywords internal
initialize_minpatch_data <- function(solution, planning_units, features, targets, costs,
                                    min_patch_size, patch_radius, boundary_penalty,
                                    prioritizr_problem, prioritizr_solution) {
  
  n_units <- length(solution)
  
  # Create unit dictionary: list with cost and status for each planning unit
  if (is.null(costs)) {
    costs <- rep(1, n_units)  # Default unit costs
  }
  
  # Status codes: 0 = available, 1 = selected, 2 = conserved, 3 = excluded
  # Convert solution to status (1 = selected, 0 = available)
  unit_dict <- vector("list", n_units)
  names(unit_dict) <- as.character(seq_len(n_units))
  
  for (i in seq_len(n_units)) {
    unit_dict[[i]] <- list(
      cost = costs[i],
      status = as.integer(solution[i])
    )
  }
  
  # Calculate planning unit areas
  area_dict <- as.numeric(sf::st_area(planning_units))
  names(area_dict) <- as.character(seq_len(n_units))
  
  # Create cost dictionary
  cost_dict <- costs
  names(cost_dict) <- as.character(seq_len(n_units))
  
  # Create boundary matrix (adjacency with shared boundary lengths)
  boundary_matrix <- create_boundary_matrix(planning_units)
  
  # Create abundance matrix (features in each planning unit)
  abundance_matrix <- create_abundance_matrix(planning_units, features)
  
  # Create target dictionary
  target_dict <- vector("list", nrow(targets))
  names(target_dict) <- as.character(targets$feature_id)
  
  for (i in seq_len(nrow(targets))) {
    target_dict[[as.character(targets$feature_id[i])]] <- list(
      name = paste0("feature_", targets$feature_id[i]),
      target = targets$target[i],
      penalty = 1.0,  # Default penalty
      type = 1        # Default type
    )
  }
  
  # Create patch radius dictionary for adding patches
  patch_radius_dict <- create_patch_radius_dict(planning_units, patch_radius)
  
  return(list(
    unit_dict = unit_dict,
    area_dict = area_dict,
    cost_dict = cost_dict,
    boundary_matrix = boundary_matrix,
    abundance_matrix = abundance_matrix,
    target_dict = target_dict,
    patch_radius_dict = patch_radius_dict,
    min_patch_size = min_patch_size,
    patch_radius = patch_radius,
    boundary_penalty = boundary_penalty,
    prioritizr_problem = prioritizr_problem,
    prioritizr_solution = prioritizr_solution
  ))
}

#' Create boundary matrix from planning units
#'
#' Creates a matrix of shared boundary lengths between adjacent planning units
#'
#' @param planning_units sf object with planning unit geometries
#'
#' @return Named list where each element contains neighbors and shared boundary lengths
#' @keywords internal
create_boundary_matrix <- function(planning_units) {
  
  n_units <- nrow(planning_units)
  boundary_matrix <- vector("list", n_units)
  names(boundary_matrix) <- as.character(seq_len(n_units))
  
  # Initialize empty lists for each planning unit
  for (i in seq_len(n_units)) {
    boundary_matrix[[i]] <- list()
  }
  
  # Find adjacent planning units and calculate shared boundary lengths
  # This is computationally intensive, so we'll use sf::st_touches for adjacency
  # and sf::st_intersection for boundary lengths
  
  cat("Calculating boundary matrix (this may take a while)...\n")
  
  # Check for invalid geometries and repair if needed
  if (any(!sf::st_is_valid(planning_units))) {
    cat("Warning: Invalid geometries detected, attempting to repair...\n")
    planning_units <- sf::st_make_valid(planning_units)
  }
  
  # Get adjacency matrix using a more robust method
  # sf::st_touches() can be unreliable due to precision issues
  # Use st_intersects() with boundaries instead
  boundaries <- sf::st_boundary(planning_units)
  touches <- sf::st_intersects(boundaries, boundaries, sparse = FALSE)
  
  # Remove self-intersections (diagonal)
  diag(touches) <- FALSE
  
  
  # Calculate shared boundaries
  for (i in seq_len(n_units)) {
    for (j in seq_len(n_units)) {
      if (i != j && touches[i, j]) {
        # Calculate shared boundary length (suppress sf warnings)
        intersection <- suppressWarnings(sf::st_intersection(
          sf::st_boundary(planning_units[i, ]),
          sf::st_boundary(planning_units[j, ])
        ))
        
        if (nrow(intersection) > 0) {
          shared_length <- sum(as.numeric(sf::st_length(intersection)))
          # Use tolerance for very small shared lengths (floating-point precision issues)
          if (shared_length > 1e-10) {
            boundary_matrix[[i]][[as.character(j)]] <- shared_length
          } else if (shared_length > 0) {
            # For very small but non-zero lengths, use a minimal positive value
            boundary_matrix[[i]][[as.character(j)]] <- 1e-6
          }
        }
      }
    }
    
    # Add self-boundary (external edge) - approximate as perimeter
    perimeter <- as.numeric(sf::st_length(sf::st_boundary(planning_units[i, ])))
    boundary_matrix[[i]][[as.character(i)]] <- perimeter
    
    if (i %% 100 == 0) {
      cat("Processed", i, "of", n_units, "planning units\n")
    }
  }
  
  return(boundary_matrix)
}

#' Create abundance matrix from planning units and features
#'
#' Creates a matrix showing the amount of each feature in each planning unit
#'
#' @param planning_units sf object with planning unit geometries
#' @param features sf object or data.frame with conservation features
#'
#' @return Named list where each planning unit contains feature abundances
#' @keywords internal
create_abundance_matrix <- function(planning_units, features) {
  
  n_units <- nrow(planning_units)
  abundance_matrix <- vector("list", n_units)
  names(abundance_matrix) <- as.character(seq_len(n_units))
  
  # Initialize empty lists
  for (i in seq_len(n_units)) {
    abundance_matrix[[i]] <- list()
  }
  
  # If features is an sf object, calculate intersections
  if (inherits(features, "sf")) {
    cat("Calculating feature abundances from spatial data...\n")
    
    for (feat_id in seq_len(nrow(features))) {
      # Calculate intersection areas between each planning unit and this feature
      intersections <- sf::st_intersection(planning_units, features[feat_id, ])
      
      if (nrow(intersections) > 0) {
        intersection_areas <- as.numeric(sf::st_area(intersections))
        
        # Get the planning unit IDs from the intersection
        pu_ids <- intersections[[names(planning_units)[1]]]  # Assuming first column is ID
        
        for (i in seq_along(pu_ids)) {
          if (intersection_areas[i] > 0) {
            abundance_matrix[[as.character(pu_ids[i])]][[as.character(feat_id)]] <- intersection_areas[i]
          }
        }
      }
      
      if (feat_id %% 10 == 0) {
        cat("Processed", feat_id, "of", nrow(features), "features\n")
      }
    }
  } else {
    # If features is a data.frame, assume it contains pu_id, feature_id, amount columns
    if (!all(c("pu_id", "feature_id", "amount") %in% names(features))) {
      stop("If features is a data.frame, it must contain 'pu_id', 'feature_id', and 'amount' columns")
    }
    
    for (i in seq_len(nrow(features))) {
      pu_id <- as.character(features$pu_id[i])
      feat_id <- as.character(features$feature_id[i])
      amount <- features$amount[i]
      
      if (amount > 0) {
        abundance_matrix[[pu_id]][[feat_id]] <- amount
      }
    }
  }
  
  return(abundance_matrix)
}

#' Create patch radius dictionary
#'
#' For each planning unit, find all units within the specified patch radius
#'
#' @param planning_units sf object with planning unit geometries
#' @param patch_radius radius for patch creation
#'
#' @return Named list where each planning unit contains list of units within radius
#' @keywords internal
create_patch_radius_dict <- function(planning_units, patch_radius) {
  
  n_units <- nrow(planning_units)
  patch_radius_dict <- vector("list", n_units)
  names(patch_radius_dict) <- as.character(seq_len(n_units))
  
  # Get centroids for distance calculations
  centroids <- sf::st_centroid(planning_units)
  
  cat("Creating patch radius dictionary...\n")
  
  for (i in seq_len(n_units)) {
    # Find all units within patch_radius of unit i
    distances <- sf::st_distance(centroids[i, ], centroids)
    # Convert both to numeric to avoid units mismatch
    distances_numeric <- as.numeric(distances)
    patch_radius_numeric <- as.numeric(patch_radius)
    within_radius <- which(distances_numeric <= patch_radius_numeric & seq_len(n_units) != i)
    
    patch_radius_dict[[i]] <- as.character(within_radius)
    
    if (i %% 100 == 0) {
      cat("Processed", i, "of", n_units, "planning units\n")
    }
  }
  
  return(patch_radius_dict)
}