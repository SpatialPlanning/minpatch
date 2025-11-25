#' MinPatch for R: Post-processing prioritizr solutions to ensure minimum patch sizes
#'
#' This package provides functions to post-process conservation planning solutions
#' from prioritizr to ensure all protected areas meet user-defined minimum size
#' thresholds, following the methodology described in Smith et al. (2010).
#'
"_PACKAGE"

#' Run MinPatch algorithm on prioritizr solution
#'
#' This is the main function that applies the MinPatch algorithm to a prioritizr
#' solution to ensure all protected areas meet minimum size thresholds.
#' The function uses prioritizr summary functions where possible to reduce code
#' duplication and ensure consistency with prioritizr calculations.
#'
#' @param prioritizr_problem A prioritizr problem object
#' @param prioritizr_solution A solved prioritizr solution object
#' @param min_patch_size Minimum patch size threshold
#' @param patch_radius Radius for adding new patches
#' @param boundary_penalty Boundary penalty value (default = 0)
#' @param remove_small_patches Logical, whether to remove small patches (Stage 1, default = TRUE)
#' @param add_patches Logical, whether to add new patches to meet targets (Stage 2, default = TRUE)
#' @param whittle_patches Logical, whether to remove unnecessary units (Stage 3, default = TRUE)
#' @param solution_column Name of solution column (default = "solution_1")
#' @param verbose Logical, whether to print progress (default = TRUE)
#'
#' @details
#' The MinPatch algorithm consists of three stages:
#' \enumerate{
#'   \item Remove small patches: Removes patches smaller than min_patch_size
#'   \item Add new patches: Adds patches to meet conservation targets
#'   \item Whittle patches: Removes unnecessary planning units
#' }
#'
#' **Important**: If you set \code{remove_small_patches = TRUE} but
#' \code{add_patches = FALSE}, the algorithm may remove patches without
#' compensating, potentially violating conservation targets. In such cases,
#' a warning will be issued. Consider using \code{add_patches = TRUE} or
#' a smaller \code{min_patch_size} to maintain target achievement.
#'
#' @return MinPatch result object with enhanced reporting using prioritizr functions
#' @export
#'
#' @examples
#'
#' library(prioritizr)
#' library(sf)
#' library(terra)
#'
#' # Get example data from prioritizr
#' dat <- c(get_sim_pu_raster(), get_sim_features()) %>%
#'   as.polygons(dissolve = FALSE, values = TRUE) %>%
#'   sf::st_as_sf() %>%
#'   dplyr::rename(cost = layer)
#'
#' st_crs(dat) <- NA
#'
#' features = colnames(dat) %>%
#'   stringr::str_subset("feature_")
#'
#' # Create prioritizr problem
#' p <- problem(dat, features, cost_column = "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.17) %>%  # 17% of each feature
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # Solve problem
#' s <- solve(p)
#'
#' # Apply MinPatch with all stages
#' result <- run_minpatch(
#'   prioritizr_problem = p,
#'   prioritizr_solution = s,
#'   min_patch_size = 0.05,
#'   patch_radius = 0.3,
#' )

#'
#' # Apply MinPatch with only Stage 1 and 3 (skip adding patches)
#' result2 <- run_minpatch(
#'   prioritizr_problem = p,
#'   prioritizr_solution = s,
#'   min_patch_size = 0.05,
#'   patch_radius = 0.3,
#'   add_patches = FALSE
#' )
#'
#' print_minpatch_summary(result)
#'
run_minpatch <- function(prioritizr_problem,
                         prioritizr_solution,
                         min_patch_size,
                         patch_radius,
                         boundary_penalty = 0,
                         remove_small_patches = TRUE,
                         add_patches = TRUE,
                         whittle_patches = TRUE,
                         solution_column = "solution_1",
                         verbose = TRUE) {

  # Stage 0: Checks and data preparation -----

  # Check if prioritizr is available
  if (!requireNamespace("prioritizr", quietly = TRUE)) {
    stop("prioritizr package is required for this function")
  }

  # Prioritizr problem is required
  if (missing(prioritizr_problem)) {
    stop("prioritizr_problem is required and must be provided")
  }

  # Extract solution vector
  if (!solution_column %in% names(prioritizr_solution)) {
    stop(paste("Solution column", solution_column, "not found"))
  }

  solution <- prioritizr_solution[[solution_column]]

  # Extract planning units and rename the selected solution column to "prioritizr"
  planning_units <- prioritizr_solution %>%
    dplyr::rename(prioritizr = !!solution_column)

  # Extract data from prioritizr problem object
  targets_raw <- prioritizr_problem$targets$data
  costs <- prioritizr_problem$planning_unit_costs()

  # Convert targets to data.frame format required by downstream functions
  if (is.data.frame(targets_raw)) {
    targets <- targets_raw
    # Ensure required columns exist
    if (!"feature_id" %in% names(targets)) {
      targets$feature_id <- seq_len(nrow(targets))
    }
  } else if (is.list(targets_raw)) {
    # Handle prioritizr targets list format - check if it contains a targets tibble
    if ("targets" %in% names(targets_raw) && is.data.frame(targets_raw$targets)) {
      # Extract the targets tibble
      targets_df <- targets_raw$targets

      # Convert relative targets to absolute targets
      targets <- data.frame(
        feature_id = seq_len(nrow(targets_df)),
        target = numeric(nrow(targets_df))
      )

      # Get feature names from prioritizr problem
      feature_names <- prioritizr::feature_names(prioritizr_problem)

      for (i in seq_len(nrow(targets_df))) {
        if (targets_df$type[i] == "relative") {
          # Calculate total amount of this feature using actual feature name
          feature_col <- feature_names[i]
          if (feature_col %in% names(planning_units)) {
            total_amount <- sum(planning_units[[feature_col]], na.rm = TRUE)
            targets$target[i] <- targets_df$target[i] * total_amount
          } else {
            stop(paste("Feature column", feature_col, "not found in planning units"))
          }
        } else {
          # Absolute target
          targets$target[i] <- targets_df$target[i]
        }
      }
    } else {
      # Handle other list formats - extract target values
      target_values <- numeric(length(targets_raw))
      for (i in seq_along(targets_raw)) {
        if (is.list(targets_raw[[i]])) {
          target_values[i] <- targets_raw[[i]]$target
        } else {
          target_values[i] <- as.numeric(targets_raw[[i]])
        }
      }
      targets <- data.frame(
        feature_id = seq_along(targets_raw),
        target = target_values
      )
    }
  } else {
    # Handle vector format
    targets <- data.frame(
      feature_id = seq_along(targets_raw),
      target = as.numeric(targets_raw)
    )
  }


  # Input validation
  if (verbose) cat("Validating inputs...\n")
  validate_inputs(solution, planning_units, targets, costs,
                  min_patch_size, patch_radius, boundary_penalty)

  # Initialize data structures
  if (verbose) cat("Initializing data structures...\n")
  minpatch_data <- initialize_minpatch_data(
    solution, planning_units, targets, costs,
    min_patch_size, patch_radius, boundary_penalty,
    prioritizr_problem, prioritizr_solution, verbose
  )

  # Create initial minpatch column in prioritizr_solution
  minpatch_data$prioritizr_solution$minpatch <- create_solution_vector(minpatch_data$unit_dict)

  # Store initial patch statistics
  if (verbose) cat("Calculating initial patch statistics...\n")
  minpatch_data <- calculate_patch_stats(minpatch_data)
  initial_patch_stats <- minpatch_data$patch_stats



  # Stage 1: Remove small patches (conditional) -----
  if (remove_small_patches) {
    if (verbose) cat("Stage 1: Removing small patches...\n")
    minpatch_data <- remove_small_patches_from_solution(minpatch_data)

    # Check if targets are still met after removing small patches
    if (!add_patches) {
      unmet_after_removal <- identify_unmet_targets(minpatch_data)

      if (length(unmet_after_removal) > 0) {
        warning(paste("After removing small patches,", length(unmet_after_removal),
                      "conservation targets are no longer met. Consider setting add_patches = TRUE",
                      "to automatically add patches to meet targets, or use a smaller min_patch_size."))
        if (verbose) {
          cat("  Warning:", length(unmet_after_removal), "targets are no longer met after removing small patches\n")
          cat("  Unmet feature IDs:", paste(unmet_after_removal, collapse = ", "), "\n")
        }
      }
    }
  } else {
    if (verbose) cat("Stage 1: Skipping removal of small patches...\n")
    # Still need to create the initial minpatch column
    minpatch_data$prioritizr_solution$minpatch <- create_solution_vector(minpatch_data$unit_dict)
  }

  # Stage 2: Add new patches to meet targets (conditional) ----
  unmet_targets <- character(0)
  if (add_patches) {
    if (verbose) cat("Stage 2: Adding new patches...\n")

    minpatch_data <- add_new_patches(minpatch_data, verbose)

    # Check final unmet targets
    unmet_targets <- identify_unmet_targets(minpatch_data)

    if (length(unmet_targets) > 0) {
      warning(paste("Could not meet targets for", length(unmet_targets), "features"))
      if (verbose) cat("  Unmet targets:", paste(unmet_targets, collapse = ", "), "\n")
    } else {
      if (verbose) cat("  All conservation targets are now met!\n")
    }
  } else {
    if (verbose) cat("Stage 2: Skipping addition of new patches...\n")
  }

  # Stage 3: Simulated whittling (conditional) ----
  if (whittle_patches) {
    if (verbose) cat("Stage 3: Removing unnecessary planning units...\n")
    minpatch_data <- simulated_whittling(minpatch_data, verbose)
  } else {
    if (verbose) cat("Stage 3: Skipping simulated whittling...\n")
  }

  # Calculate final statistics
  if (verbose) cat("Calculating final statistics...\n")
  minpatch_data <- calculate_patch_stats(minpatch_data)
  final_patch_stats <- minpatch_data$patch_stats

  # Create output solution vector
  solution_vector <- create_solution_vector(minpatch_data$unit_dict)

  # Create the output sf object with both prioritizr and minpatch columns
  result_sf <- planning_units  # This already has the "prioritizr" column
  result_sf$minpatch <- solution_vector

  # Create solution data for prioritizr functions using the minpatch column
  solution_data_for_prioritizr <- result_sf[c("minpatch")]
  names(solution_data_for_prioritizr)[names(solution_data_for_prioritizr) == "minpatch"] <- solution_column

  # Use prioritizr functions for cost summary
  cost_summary <- calculate_cost_summary(minpatch_data)

  if (verbose) cat("MinPatch processing complete!\n")

  return(list(
    solution = result_sf,  # Return the sf object with minpatch column
    patch_stats = list(
      initial = initial_patch_stats,
      final = final_patch_stats
    ),
    cost_summary = cost_summary,
    planning_units = planning_units,
    minpatch_data = minpatch_data
  ))
}
