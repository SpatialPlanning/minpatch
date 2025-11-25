#' Validate MinPatch inputs
#'
#' Internal function to validate all inputs to the MinPatch algorithm
#'
#' @param solution Binary solution vector
#' @param planning_units sf object with planning units
#' @param targets data.frame with targets
#' @param costs numeric vector of costs
#' @param min_patch_size minimum patch size
#' @param patch_radius patch radius for adding patches
#' @param boundary_penalty Boundary penalty value
#'
#' @return NULL (throws errors if validation fails)
#' @keywords internal
validate_inputs <- function(solution, planning_units, targets, costs,
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
#' @param targets data.frame with targets
#' @param costs numeric vector of costs
#' @param min_patch_size minimum patch size
#' @param patch_radius patch radius
#' @param boundary_penalty Boundary penalty value
#' @param prioritizr_problem A prioritizr problem object
#' @param prioritizr_solution A solved prioritizr solution object
#'
#' @return List containing all necessary data structures
#' @keywords internal
initialize_minpatch_data <- function(solution, planning_units, targets, costs,
                                    min_patch_size, patch_radius, boundary_penalty,
                                    prioritizr_problem, prioritizr_solution, verbose = TRUE) {

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
  boundary_matrix <- create_boundary_matrix(planning_units, verbose)

  # Create abundance matrix (features in each planning unit)
  # Extract directly from planning_units feature columns using prioritizr problem
  abundance_matrix <- create_abundance_matrix(planning_units, prioritizr_problem)

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
  patch_radius_dict <- create_patch_radius_dict(planning_units, patch_radius, verbose)

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
#' Creates a sparse matrix of shared boundary lengths between adjacent planning units.
#' Returns a Matrix::sparseMatrix for efficient storage and operations.
#' This optimized version supports parallel processing via the parallelly package.
#' When n_cores = 1, runs sequentially with no parallel overhead.
#'
#' @param planning_units sf object with planning unit geometries
#' @param verbose Logical, whether to print progress
#' @param n_cores Integer, number of cores to use. If NULL, uses availableCores(omit=1).
#'               Set to 1 for sequential processing.
#'
#' @return Matrix::dgCMatrix sparse matrix where [i,j] is the shared boundary length
#' @keywords internal
create_boundary_matrix <- function(planning_units, verbose = TRUE, n_cores = NULL) {

  n_units <- nrow(planning_units)

  # Determine number of cores
  if (is.null(n_cores)) {
    if (requireNamespace("parallelly", quietly = TRUE)) {
      n_cores <- parallelly::availableCores(omit = 2)
    } else {
      n_cores <- 1
    }
  }
  # Only use parallel for larger datasets (overhead not worth it for small ones)
  if (n_units < 500) {
    n_cores <- 1
  } else {
    n_cores <- min(n_cores, n_units)
  }

  # Final safety check: ensure n_cores is always between 1 and n_units
  n_cores <- max(1, min(n_cores, n_units))

  if (verbose) {
    if (n_cores > 1) {
      cat("Calculating boundary matrix using", n_cores, "cores...\n")
    } else {
      cat("Calculating boundary matrix (optimized version)...\n")
    }
  }

  # Check for invalid geometries and repair if needed
  if (any(!sf::st_is_valid(planning_units))) {
    cat("Warning: Invalid geometries detected, attempting to repair...\n")
    planning_units <- sf::st_make_valid(planning_units)
  }

  # Pre-compute all boundaries once (major optimization)
  boundaries <- sf::st_boundary(planning_units)

  # Pre-compute all perimeters once for diagonal
  perimeters <- as.numeric(sf::st_length(boundaries))

  # Get sparse adjacency list (much more efficient than dense matrix)
  touches_sparse <- sf::st_intersects(boundaries, boundaries)

  # Split work into chunks - handle edge cases properly
  if (n_cores == 1) {
    # Single core: all units in one chunk
    chunks <- list(seq_len(n_units))
  } else {
    # Multiple cores: split evenly
    # Ensure we don't try to create more chunks than units
    actual_cores <- min(n_cores, n_units)
    if (actual_cores >= n_units) {
      # If cores >= units, each unit gets its own chunk
      chunks <- as.list(seq_len(n_units))
    } else {
      # Normal case: split into chunks
      chunks <- split(seq_len(n_units), cut(seq_len(n_units), actual_cores, labels = FALSE))
    }
  }

  # Function to process a chunk of units
  process_chunk <- function(unit_indices) {
    local_i <- integer()
    local_j <- integer()
    local_lengths <- numeric()

    for (i in unit_indices) {
      neighbors <- touches_sparse[[i]]
      neighbors <- neighbors[neighbors != i]

      if (length(neighbors) > 0) {
        for (j in neighbors) {
          if (i < j) {  # Only process each pair once
            intersection <- suppressWarnings(sf::st_intersection(
              boundaries[i, ],
              boundaries[j, ]
            ))

            if (nrow(intersection) > 0) {
              shared_length <- sum(as.numeric(sf::st_length(intersection)))
              if (shared_length > 1e-10) {
                local_i <- c(local_i, i, j)
                local_j <- c(local_j, j, i)
                local_lengths <- c(local_lengths, shared_length, shared_length)
              } else if (shared_length > 0) {
                local_i <- c(local_i, i, j)
                local_j <- c(local_j, j, i)
                local_lengths <- c(local_lengths, 1e-6, 1e-6)
              }
            }
          }
        }
      }
    }

    list(i = local_i, j = local_j, x = local_lengths)
  }

  # Process chunks (parallel if n_cores > 1, sequential if n_cores = 1)
  if (n_cores > 1 && requireNamespace("parallelly", quietly = TRUE)) {
    # Parallel processing
    cl <- parallelly::makeClusterPSOCK(n_cores, autoStop = TRUE, verbose = FALSE)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterExport(cl, c("boundaries", "touches_sparse"),
                           envir = environment())
    parallel::clusterEvalQ(cl, library(sf))

    if (verbose) cat("Processing chunks in parallel...\n")
    results <- parallel::parLapply(cl, chunks, process_chunk)
  } else {
    # Sequential processing
    results <- lapply(chunks, function(chunk) {
      result <- process_chunk(chunk)
      if (verbose && max(chunk) %% 100 == 0) {
        cat("Processed", max(chunk), "of", n_units, "planning units\n")
      }
      result
    })
  }

  # Combine results
  if (verbose && n_cores > 1) cat("Combining results...\n")
  i_indices <- unlist(lapply(results, function(r) r$i))
  j_indices <- unlist(lapply(results, function(r) r$j))
  boundary_lengths <- unlist(lapply(results, function(r) r$x))

  # Add perimeters on diagonal
  i_indices <- c(i_indices, seq_len(n_units))
  j_indices <- c(j_indices, seq_len(n_units))
  boundary_lengths <- c(boundary_lengths, perimeters)

  # Create sparse matrix
  Matrix::sparseMatrix(
    i = i_indices,
    j = j_indices,
    x = boundary_lengths,
    dims = c(n_units, n_units),
    dimnames = list(as.character(seq_len(n_units)),
                   as.character(seq_len(n_units)))
  )
}

#' Create abundance matrix from planning units
#'
#' Creates a matrix showing the amount of each feature in each planning unit
#' by extracting feature columns directly from planning_units using prioritizr problem
#'
#' @param planning_units sf object with planning unit geometries and feature columns
#' @param prioritizr_problem A prioritizr problem object to get feature names
#'
#' @return Named list where each planning unit contains feature abundances
#' @keywords internal
create_abundance_matrix <- function(planning_units, prioritizr_problem) {

  n_units <- nrow(planning_units)
  abundance_matrix <- vector("list", n_units)
  names(abundance_matrix) <- as.character(seq_len(n_units))

  # Initialize empty lists
  for (i in seq_len(n_units)) {
    abundance_matrix[[i]] <- list()
  }

  # Get feature names from prioritizr problem
  feature_names <- prioritizr::feature_names(prioritizr_problem)

  if (length(feature_names) > 0) {
    for (i in seq_along(feature_names)) {
      col_name <- feature_names[i]
      feature_id <- as.character(i)  # Use sequential numbering for feature IDs

      # Check if this feature column exists in planning_units
      if (col_name %in% names(planning_units)) {
        abundances <- planning_units[[col_name]]

        for (pu_id in seq_len(n_units)) {
          if (abundances[pu_id] > 0) {
            abundance_matrix[[as.character(pu_id)]][[feature_id]] <- abundances[pu_id]
          }
        }
      } else {
        warning(paste("Feature column", col_name, "not found in planning units"))
      }
    }
  }

  return(abundance_matrix)
}

#' Create patch radius dictionary
#'
#' For each planning unit, find all units within the specified patch radius.
#' Optimized version computes full distance matrix once instead of n times.
#'
#' @param planning_units sf object with planning unit geometries
#' @param patch_radius radius for patch creation
#'
#' @return Named list where each planning unit contains list of units within radius
#' @keywords internal
create_patch_radius_dict <- function(planning_units, patch_radius, verbose = TRUE) {

  n_units <- nrow(planning_units)
  patch_radius_dict <- vector("list", n_units)
  names(patch_radius_dict) <- as.character(seq_len(n_units))

  # Get centroids for distance calculations
  centroids <- sf::st_centroid(planning_units %>%
                                 dplyr::select("geometry"))

  if (verbose) cat("Creating patch radius dictionary (optimized)...\n")

  # OPTIMIZATION: Compute full distance matrix ONCE instead of n times
  # This changes from O(n²) distance calculations to O(n²/2) calculations
  dist_matrix <- sf::st_distance(centroids, centroids)
  dist_matrix_numeric <- as.numeric(dist_matrix)
  patch_radius_numeric <- as.numeric(patch_radius)

  # Create matrix of dimensions n x n
  dist_mat <- matrix(dist_matrix_numeric, nrow = n_units, ncol = n_units)

  # For each unit, find neighbors within radius
  for (i in seq_len(n_units)) {
    # Use vectorized comparison on pre-computed distances
    within_radius <- which(dist_mat[i, ] <= patch_radius_numeric & seq_len(n_units) != i)
    patch_radius_dict[[i]] <- as.character(within_radius)

    if (verbose && i %% 100 == 0) {
      cat("Processed", i, "of", n_units, "planning units\n")
    }
  }

  return(patch_radius_dict)
}
