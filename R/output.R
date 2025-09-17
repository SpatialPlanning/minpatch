#' Compare solutions before and after MinPatch
#'
#' Creates a comprehensive comparison of key metrics between original and MinPatch solutions,
#' including overall statistics and detailed feature-level analysis
#'
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
#' # Run MinPatch
#' result <- run_minpatch(
#'   prioritizr_problem = p,
#'   prioritizr_solution = s,
#'   min_patch_size = 0.05,
#'   patch_radius = 0.3,
#'   verbose = FALSE
#' )
#'
#' # Compare solutions
#' comparison <- compare_solutions(result)
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
#'
compare_solutions <- function(minpatch_result) {

  # Extract data
  minpatch_solution <- minpatch_result$solution$minpatch
  original_solution <- minpatch_result$solution$prioritizr
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
  temp_minpatch_data <- minpatch_result$minpatch_data
  temp_minpatch_data$unit_dict <- original_unit_dict
  temp_minpatch_data <- calculate_patch_stats(temp_minpatch_data)
  original_patch_stats <- temp_minpatch_data$patch_stats

  # Calculate original cost using the consistent interface
  temp_cost_data <- temp_minpatch_data
  original_cost <- calculate_cost_summary(temp_cost_data)

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
      original_cost$cost,
      original_cost$boundary_cost,
      original_cost$total_cost
    ),
    MinPatch = c(
      sum(minpatch_solution),
      final_stats$all_patch_area,
      final_stats$all_patch_count,
      final_stats$valid_patch_count,
      final_stats$median_all_patch,
      cost_summary$cost,
      cost_summary$boundary_cost,
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
  temp_original_data <- minpatch_result$minpatch_data
  temp_original_data$unit_dict <- original_unit_dict
  original_feature_amounts <- calculate_feature_conservation(temp_original_data)

  # Calculate MinPatch feature conservation
  minpatch_feature_amounts <- calculate_feature_conservation(minpatch_result$minpatch_data)

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
    summary = data.frame(
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
#' @param minpatch_result Result from run_minpatch function
#' @param title Plot title (optional)
#'
#' @return ggplot object (if ggplot2 is available)
#' @export
#'
#' @examples
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
#' # Run MinPatch
#' result <- run_minpatch(
#'   prioritizr_problem = p,
#'   prioritizr_solution = s,
#'   min_patch_size = 0.05,
#'   patch_radius = 0.3,
#'   verbose = FALSE
#' )
#'
#' # Visualize results
#' plot_minpatch(result)
plot_minpatch <- function(minpatch_result, title = "MinPatch Results") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for visualization")
  }

  # Extract planning units and solutions
  planning_units <- minpatch_result$planning_units
  original_solution <- minpatch_result$solution$prioritizr
  minpatch_solution <- minpatch_result$solution$minpatch

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
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$change), color = "white", size = 0.2) +
    ggplot2::scale_fill_manual(
      values = c("No Change" = "lightgray",
                 "Added" = "darkgreen",
                 "Removed" = "red",
                 "Retained" = "lightgreen"),
      name = "Change",
      drop = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::labs(title = title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom"
    )

}


#' Visualize prioritizr solutions
#'
#' Creates a simple visualization of prioritizr solutions showing selected
#' and unselected planning units using ggplot2
#'
#' @param s sf object containing planning units with solution data
#' @param col Column name containing the solution values (default = "solution_1")
#' @param title Plot title (default = "prioritizr Solution")
#'
#' @return ggplot object showing the spatial solution
#' @export
#'
#' @examples
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
#' # Plot the solution
#' plot_prioritizr(s)
#'
#' # Plot with custom title and column
#' plot_prioritizr(s, col = "solution_1", title = "My Conservation Plan")
plot_prioritizr <- function(s, col = "solution_1", title = "prioritizr Solution") {

ggplot2::ggplot(data = s) +
  ggplot2::geom_sf(ggplot2::aes(fill = as.logical(.data[[col]])), color = "white", size = 0.2) +
  ggplot2::scale_fill_manual(
    values = c("TRUE" = "blue",
               "FALSE" = "grey80"),
    name = "PU's Selected",
    drop = FALSE) +
  ggplot2::theme_void() +
  ggplot2::labs(title = title) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    legend.position = "bottom"
  )
}



#' Generate comprehensive MinPatch report
#'
#' Creates a detailed report of the MinPatch processing results
#'
#' @param minpatch_result Result object from run_minpatch function
#'
#' @return List containing formatted report components
#' @export
#' @examples
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
#' # Run MinPatch
#' result <- run_minpatch(
#'   prioritizr_problem = p,
#'   prioritizr_solution = s,
#'   min_patch_size = 0.05,
#'   patch_radius = 0.3,
#'   verbose = FALSE
#' )
#'
#' generate_minpatch_report(result)
#'
generate_minpatch_report <- function(minpatch_result) {

  # Combine patch stats
  patch_stats <- rbind(minpatch_result$patch_stats$initial,
                       minpatch_result$patch_stats$final)
  patch_stats <- cbind(data.frame(time = c("initial", "final")), patch_stats)


  # Calculate feature representation using prioritizr objects from minpatch_data
  feature_rep <- prioritizr::eval_target_coverage_summary(x = minpatch_result$minpatch_data$prioritizr_problem,
                                                          solution = minpatch_result$minpatch_data$prioritizr_solution %>%
                                                            dplyr::select("minpatch"))

  return(list(
    features = feature_rep,
    patch_stats = patch_stats,
    cost = minpatch_result$cost_summary
  ))

}

#' Print MinPatch results summary
#'
#' Prints a formatted summary of MinPatch processing results
#'
#' @param minpatch_result Result object from run_minpatch function
#'
#' @export
#'
#' @examples
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
#' # Run MinPatch
#' result <- run_minpatch(
#'   prioritizr_problem = p,
#'   prioritizr_solution = s,
#'   min_patch_size = 0.05,
#'   patch_radius = 0.3,
#' )
#'
#' print_minpatch_summary(result)
#'
print_minpatch_summary <- function(minpatch_result) {

  report <- generate_minpatch_report(minpatch_result)

  cat("=== MinPatch Processing Summary ===\n\n")

  # Patch statistics
  cat("Patch Statistics:\n")
  cat(sprintf("  Initial patches: %d (valid: %d)\n",
              report$patch_stats$all_patch_count[1],
              report$patch_stats$valid_patch_count[1]))
  cat(sprintf("  Final patches: %d (valid: %d)\n",
              report$patch_stats$all_patch_count[2],
              report$patch_stats$valid_patch_count[2]))
  cat(sprintf("  Area change: %.2f (%.1f%%)\n",
              (report$patch_stats$all_patch_area[2] - report$patch_stats$all_patch_area[1]),
              ((report$patch_stats$all_patch_area[2] - report$patch_stats$all_patch_area[1]) / report$patch_stats$all_patch_area[1]) * 100))
  cat("\n")

  # Cost breakdown
  cat("Cost Breakdown:\n")
  cat(sprintf("  Planning unit cost: %.2f\n", report$cost$cost))
  cat(sprintf("  Boundary cost: %.2f\n", report$cost$boundary_cost))
  cat(sprintf("  Total cost: %.2f\n", report$cost$total_cost))
  cat(sprintf("  Selected units: %d\n", report$cost$n))
  cat("\n")

  # Feature representation
  cat("Feature Representation:\n")
  cat(sprintf("  Total features: %d\n", nrow(report$features)))
  cat(sprintf("  Targets met: %d\n", sum(report$features$met, na.rm = TRUE)))
  cat(sprintf("  Targets unmet: %d\n", sum(!report$features$met, na.rm = TRUE)))
  cat(sprintf("  Mean proportion: %.3f\n", mean(report$features$relative_held, na.rm = TRUE)))
  cat(sprintf("  Total shortfall: %.2f\n", sum(report$features$absolute_shortfall, na.rm = TRUE)))
  cat("\n")

  # if (report$feature_summary$targets_unmet > 0) {
  #   cat("Features with unmet targets:\n")
  #   unmet <- report$feature_representation[!report$feature_representation$target_met, ]
  #   for (i in seq_len(nrow(unmet))) {
  #     cat(sprintf("  Feature %s: %.2f/%.2f (%.1f%% met)\n",
  #                 unmet$feature_id[i],
  #                 unmet$conserved[i],
  #                 unmet$target[i],
  #                 unmet$proportion_met[i] * 100))
  #   }
  # }

  cat("\n=== End Summary ===\n")
}
