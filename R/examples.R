






#' Compare solutions before and after MinPatch
#'
#' Creates a comprehensive comparison of key metrics between original and MinPatch solutions,
#' including overall statistics and detailed feature-level analysis
#'
#' @param original_solution Original binary solution vector
#' @param minpatch_result Result from run_minpatch function
#'
#' @return List containing:
#'   \itemize{
#'     \item overall: Data frame with overall solution comparison
#'     \item features: Data frame with feature-level area comparisons
#'     \item summary: List with summary statistics of feature changes
#'   }
#' @export
#'
#' @examples
#' # Create example data
#' example_data <- create_example_data(n_units = 25, n_features = 3)
#'
#' # Create original solution
#' set.seed(123)
#' original_solution <- sample(c(0, 1), 25, replace = TRUE, prob = c(0.7, 0.3))
#'
#' # Run MinPatch
#' result <- run_minpatch(
#'   solution = original_solution,
#'   planning_units = example_data$planning_units,
#'   features = example_data$features,
#'   targets = example_data$targets,
#'   min_patch_size = 2.0,
#'   patch_radius = 1.5
#' )
#'
#' # Compare solutions
#' comparison <- compare_solutions(original_solution, result)
#'
#' # Print overall comparison
#' print(comparison$overall)
#'
#' # Print feature-level comparison
#' print(comparison$features)
#'
#' # Print summary statistics
#' cat("Features improved:", comparison$summary$features_improved, "\n")
#' cat("Targets gained:", comparison$summary$targets_gained, "\n")
compare_solutions <- function(original_solution, minpatch_result) {

  # Extract data
  minpatch_solution <- minpatch_result$solution
  initial_stats <- minpatch_result$patch_stats$initial
  final_stats <- minpatch_result$patch_stats$final
  cost_summary <- minpatch_result$cost_summary

  # Calculate TRUE original solution costs and statistics
  original_unit_dict <- minpatch_result$minpatch_data$unit_dict

  # Reset to original solution
  for (i in seq_along(original_solution)) {
    original_unit_dict[[as.character(i)]]$status <- original_solution[i]
  }

  # Calculate original solution patch statistics
  original_patch_dict <- make_patch_dict(original_unit_dict, minpatch_result$minpatch_data$boundary_matrix)
  original_patch_stats <- calculate_patch_stats(original_patch_dict,
                                               minpatch_result$minpatch_data$area_dict,
                                               minpatch_result$minpatch_data$min_patch_size)

  original_cost <- calculate_cost_summary(
    original_unit_dict,
    minpatch_result$minpatch_data$cost_dict,
    minpatch_result$minpatch_data$boundary_matrix,
    minpatch_result$minpatch_data$boundary_penalty
  )

  # Create overall comparison data frame
  overall_comparison <- data.frame(
    Metric = c(
      "Selected Planning Units",
      "Total Area",
      "Number of Patches",
      "Valid Patches (>= min size)",
      "Median Patch Size",
      "Planning Unit Cost",
      "Boundary Cost",
      "Total Cost"
    ),
    Original = c(
      sum(original_solution),
      original_patch_stats$all_patch_area,
      original_patch_stats$all_patch_count,
      original_patch_stats$valid_patch_count,
      original_patch_stats$median_all_patch,
      original_cost$total_unit_cost,
      original_cost$total_boundary_cost,
      original_cost$total_cost
    ),
    MinPatch = c(
      sum(minpatch_solution),
      final_stats$all_patch_area,
      final_stats$all_patch_count,
      final_stats$valid_patch_count,
      final_stats$median_all_patch,
      cost_summary$total_unit_cost,
      cost_summary$total_boundary_cost,
      cost_summary$total_cost
    ),
    stringsAsFactors = FALSE
  )

  # Calculate changes
  overall_comparison$Change <- overall_comparison$MinPatch - overall_comparison$Original
  overall_comparison$Percent_Change <- ifelse(overall_comparison$Original != 0,
                                             (overall_comparison$Change / overall_comparison$Original) * 100,
                                             NA)

  # Calculate feature-level comparisons
  abundance_matrix <- minpatch_result$minpatch_data$abundance_matrix
  target_dict <- minpatch_result$minpatch_data$target_dict
  
  # Calculate original feature conservation
  original_feature_amounts <- calculate_feature_conservation(original_unit_dict, abundance_matrix, target_dict)
  
  # Calculate MinPatch feature conservation
  minpatch_unit_dict <- minpatch_result$minpatch_data$unit_dict
  minpatch_feature_amounts <- calculate_feature_conservation(minpatch_unit_dict, abundance_matrix, target_dict)
  
  # Create feature comparison data frame
  feature_ids <- names(target_dict)
  feature_comparison <- data.frame(
    Feature_ID = feature_ids,
    Target = sapply(target_dict, function(x) x$target),
    Original_Area = original_feature_amounts[feature_ids],
    MinPatch_Area = minpatch_feature_amounts[feature_ids],
    stringsAsFactors = FALSE
  )
  
  # Calculate feature changes
  feature_comparison$Area_Change <- feature_comparison$MinPatch_Area - feature_comparison$Original_Area
  feature_comparison$Percent_Change <- ifelse(feature_comparison$Original_Area != 0,
                                             (feature_comparison$Area_Change / feature_comparison$Original_Area) * 100,
                                             NA)
  
  # Calculate target achievement
  feature_comparison$Original_Target_Met <- feature_comparison$Original_Area >= feature_comparison$Target
  feature_comparison$MinPatch_Target_Met <- feature_comparison$MinPatch_Area >= feature_comparison$Target
  feature_comparison$Original_Proportion <- ifelse(feature_comparison$Target > 0,
                                                   feature_comparison$Original_Area / feature_comparison$Target,
                                                   NA)
  feature_comparison$MinPatch_Proportion <- ifelse(feature_comparison$Target > 0,
                                                  feature_comparison$MinPatch_Area / feature_comparison$Target,
                                                  NA)

  # Return both comparisons
  return(list(
    overall = overall_comparison,
    features = feature_comparison,
    summary = list(
      features_improved = sum(feature_comparison$Area_Change > 0, na.rm = TRUE),
      features_reduced = sum(feature_comparison$Area_Change < 0, na.rm = TRUE),
      features_unchanged = sum(feature_comparison$Area_Change == 0, na.rm = TRUE),
      targets_gained = sum(feature_comparison$MinPatch_Target_Met & !feature_comparison$Original_Target_Met, na.rm = TRUE),
      targets_lost = sum(!feature_comparison$MinPatch_Target_Met & feature_comparison$Original_Target_Met, na.rm = TRUE)
    )
  ))
}

#' Visualize MinPatch results
#'
#' Creates a simple visualization of the MinPatch results showing
#' original vs. modified solutions
#'
#' @param original_solution Original binary solution vector
#' @param minpatch_result Result from run_minpatch function
#' @param title Plot title (optional)
#'
#' @return ggplot object (if ggplot2 is available)
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires ggplot2
#' library(ggplot2)
#'
#' # Create example data
#' example_data <- create_example_data(n_units = 25, n_features = 3)
#'
#' # Create original solution
#' set.seed(123)
#' original_solution <- sample(c(0, 1), 25, replace = TRUE, prob = c(0.7, 0.3))
#'
#' # Run MinPatch
#' result <- run_minpatch(
#'   solution = original_solution,
#'   planning_units = example_data$planning_units,
#'   features = example_data$features,
#'   targets = example_data$targets,
#'   min_patch_size = 2.0,
#'   patch_radius = 1.5
#' )
#'
#' # Visualize results
#' plot <- visualize_minpatch_results(original_solution, result)
#' print(plot)
#' }
visualize_minpatch_results <- function(original_solution, minpatch_result, title = "MinPatch Results") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for visualization")
  }

  # Extract planning units and solutions
  planning_units <- minpatch_result$planning_units
  minpatch_solution <- minpatch_result$solution

  # Create plotting data
  plot_data <- planning_units
  plot_data$original <- factor(original_solution, levels = c(0, 1), labels = c("Not Selected", "Selected"))
  plot_data$minpatch <- factor(minpatch_solution, levels = c(0, 1), labels = c("Not Selected", "Selected"))

  # Create change category
  plot_data$change <- "No Change"
  plot_data$change[original_solution == 0 & minpatch_solution == 1] <- "Added"
  plot_data$change[original_solution == 1 & minpatch_solution == 0] <- "Removed"
  plot_data$change[original_solution == 1 & minpatch_solution == 1] <- "Retained"

  plot_data$change <- factor(plot_data$change,
                            levels = c("No Change", "Added", "Removed", "Retained"))

  # Create the plot
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = change), color = "white", size = 0.2) +
    ggplot2::scale_fill_manual(
      values = c("No Change" = "lightgray",
                "Added" = "darkgreen",
                "Removed" = "red",
                "Retained" = "lightgreen"),
      name = "Change"
    ) +
    ggplot2::theme_void() +
    ggplot2::labs(title = title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom"
    )

  return(p)
}

