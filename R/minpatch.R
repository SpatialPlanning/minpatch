#' MinPatch for R: Post-processing prioritizr solutions to ensure minimum patch sizes
#'
#' This package provides functions to post-process conservation planning solutions
#' from prioritizr to ensure all protected areas meet user-defined minimum size
#' thresholds, following the methodology described in Smith et al. (2010).
#'
#' @docType package
#' @name minpatch
#' @author Your Name
#' @references Smith, R.J., Di Minin, E., Linke, S., Segan, D.B., Possingham, H.P. (2010).
#'   An approach for ensuring minimum protected area size in systematic conservation planning.
#'   Biological Conservation, 143(10), 2525-2531.
NULL

#' Run MinPatch algorithm on prioritizr solution
#'
#' This is the main function that applies the MinPatch algorithm to a prioritizr
#' solution to ensure all protected areas meet minimum size thresholds.
#' The function automatically extracts all necessary data from the prioritizr solution object.
#'
#' @param prioritizr_solution A solved prioritizr problem object
#' @param min_patch_size Minimum patch size threshold
#' @param patch_radius Radius for adding new patches
#' @param boundary_penalty Boundary length modifier (default = 0)
#' @param remove_small_patches Logical, whether to remove small patches (Stage 1, default = TRUE)
#' @param add_patches Logical, whether to add new patches to meet targets (Stage 2, default = TRUE)
#' @param whittle_patches Logical, whether to remove unnecessary units (Stage 3, default = TRUE)
#' @param solution_column Name of solution column (default = "solution_1")
#' @param verbose Logical, whether to print progress (default = TRUE)
#'
#' @return MinPatch result object
#' @export
#'
#' @examples
#' \dontrun{
#' library(prioritizr)
#'
#' # Create example data
#' example_data <- create_example_data()
#'
#' # Create prioritizr problem
#' p <- problem(example_data$planning_units,
#'              cost_column = "cost") %>%
#'   add_min_set_objective() %>%
#'   add_manual_targets(example_data$targets) %>%
#'   add_binary_decisions()
#'
#' # Solve problem
#' s <- solve(p)
#'
#' # Apply MinPatch with all stages
#' result <- run_minpatch(
#'   s,
#'   min_patch_size = 2.0,
#'   patch_radius = 1.5
#' )
#'
#' # Apply MinPatch with only Stage 1 and 3 (skip adding patches)
#' result2 <- run_minpatch(
#'   s,
#'   min_patch_size = 2.0,
#'   patch_radius = 1.5,
#'   add_patches = FALSE
#' )
#'
#' print_minpatch_summary(result)
#' }
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

  # Extract planning units (remove solution columns)
  planning_units <- prioritizr_solution
  solution_cols <- grep("^solution_", names(planning_units))
  if (length(solution_cols) > 0) {
    planning_units <- planning_units[, -solution_cols]
  }

  # Extract feature data from planning units
  feature_cols <- grep("^feature_", names(planning_units))
  if (length(feature_cols) == 0) {
    stop("No feature columns found in planning units. Feature columns should be named 'feature_1', 'feature_2', etc.")
  }

  # Create features data frame
  features_data <- data.frame()
  for (i in seq_along(feature_cols)) {
    col_name <- names(planning_units)[feature_cols[i]]
    feature_id <- as.numeric(gsub("feature_", "", col_name))

    # Get non-zero abundances
    abundances <- planning_units[[col_name]]
    non_zero <- which(abundances > 0)

    if (length(non_zero) > 0) {
      feat_data <- data.frame(
        pu_id = non_zero,
        feature_id = feature_id,
        amount = abundances[non_zero]
      )
      features_data <- rbind(features_data, feat_data)
    }
  }

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
      
      for (i in seq_len(nrow(targets_df))) {
        if (targets_df$type[i] == "relative") {
          # Calculate total amount of this feature
          feature_col <- paste0("feature_", i)
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
  
  # Debug: print targets and features structure
  if (verbose) {
    cat("Targets structure:\n")
    cat("  Class:", class(targets), "\n")
    cat("  Columns:", paste(names(targets), collapse = ", "), "\n")
    cat("  Dimensions:", nrow(targets), "x", ncol(targets), "\n")
    cat("  Target feature IDs:", paste(targets$feature_id, collapse = ", "), "\n")
    cat("Features data structure:\n")
    cat("  Unique feature IDs:", paste(sort(unique(features_data$feature_id)), collapse = ", "), "\n")
    cat("  Total feature records:", nrow(features_data), "\n")
  }

  # Input validation
  if (verbose) cat("Validating inputs...\n")
  validate_inputs(solution, planning_units, features_data, targets, costs,
                  min_patch_size, patch_radius, boundary_penalty)

  # Initialize data structures
  if (verbose) cat("Initializing data structures...\n")
  minpatch_data <- initialize_minpatch_data(
    solution, planning_units, features_data, targets, costs,
    min_patch_size, patch_radius, boundary_penalty
  )

  # Create initial patch dictionary
  if (verbose) cat("Identifying initial patches...\n")
  patch_dict <- make_patch_dict(minpatch_data$unit_dict, minpatch_data$boundary_matrix)

  # Store initial patch statistics
  initial_patch_stats <- NULL
  if (length(patch_dict) > 0) {
    initial_patch_stats <- calculate_patch_stats(patch_dict, minpatch_data$area_dict, min_patch_size)
  }

  # Stage 1: Remove small patches (conditional)
  if (remove_small_patches) {
    if (verbose) cat("Stage 1: Removing small patches...\n")
    minpatch_data <- remove_small_patches_from_solution(
      minpatch_data, patch_dict, min_patch_size
    )
  } else {
    if (verbose) cat("Stage 1: Skipping removal of small patches...\n")
  }

  # Stage 2: Add new patches to meet targets (conditional)
  unmet_targets <- character(0)
  if (add_patches) {
    if (verbose) cat("Stage 2: Adding new patches...\n")
    result <- add_new_patches(minpatch_data, patch_radius, verbose)
    minpatch_data$unit_dict <- result$unit_dict
    unmet_targets <- result$unmet_targets

    if (length(unmet_targets) > 0) {
      warning(paste("Could not meet targets for", length(unmet_targets), "features"))
    }
  } else {
    if (verbose) cat("Stage 2: Skipping addition of new patches...\n")
  }

  # Stage 3: Simulated whittling (conditional)
  if (whittle_patches) {
    if (verbose) cat("Stage 3: Removing unnecessary planning units...\n")
    minpatch_data$unit_dict <- simulated_whittling(
      minpatch_data$unit_dict, minpatch_data, min_patch_size, verbose
    )
  } else {
    if (verbose) cat("Stage 3: Skipping simulated whittling...\n")
  }

  # Calculate final statistics
  if (verbose) cat("Calculating final statistics...\n")
  final_patch_dict <- make_patch_dict(minpatch_data$unit_dict, minpatch_data$boundary_matrix)
  final_patch_stats <- calculate_patch_stats(final_patch_dict, minpatch_data$area_dict, min_patch_size)
  cost_summary <- calculate_cost_summary(minpatch_data$unit_dict, minpatch_data$cost_dict,
                                         minpatch_data$boundary_matrix, boundary_penalty)

  # Create output solution vector
  solution_vector <- create_solution_vector(minpatch_data$unit_dict)

  if (verbose) cat("MinPatch processing complete!\n")

  return(list(
    solution = solution_vector,
    patch_stats = list(
      initial = initial_patch_stats,
      final = final_patch_stats
    ),
    cost_summary = cost_summary,
    planning_units = planning_units,
    minpatch_data = minpatch_data
  ))
}
