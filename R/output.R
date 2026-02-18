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


#' Plot solution grid across multiple results
#'
#' Creates a grid of prioritizr-style selection maps with a shared legend.
#' Useful for comparing solutions across different boundary penalties or
#' MinPatch multipliers.
#'
#' @param results List of MinPatch result objects to plot
#' @param get_solution_fun Function to extract solution sf object from each result
#' @param ncol Number of columns in the grid (default = 3)
#' @param legend_position Position of shared legend (default = "bottom")
#' @param title_size Font size for plot titles (default = 13)
#' @param titles Character vector of titles (one per result). Takes priority over other title options.
#' @param title_fun Function(i, results) returning title string. Used if titles is NULL.
#' @param subtitle_values Numeric vector of values (e.g., penalties or multipliers) for titles
#' @param title_prefix Prefix string for subtitle_values titles (e.g., "Boundary penalty = ")
#' @param value_label_fun Function to format subtitle_values (default formats scientifically)
#' @param include_baseline Logical, whether to include a baseline plot (default = FALSE)
#' @param baseline_solution sf object for baseline solution (required if include_baseline = TRUE)
#' @param baseline_title Title for baseline plot
#'
#' @return A patchwork object combining all plots
#' @export
plot_solution_grid <- function(
    results,
    get_solution_fun,
    ncol = 3,
    legend_position = "bottom",
    title_size = 13,

    # --- Title options (pick ONE) ---
    titles = NULL,            # e.g., c("MinPatch: 5x median PU area", ...)
    title_fun = NULL,         # function(i, results) -> "..."
    subtitle_values = NULL,   # e.g., penalties or multipliers
    title_prefix = "",        # e.g., "Boundary penalty = " or "MinPatch: "
    value_label_fun = function(x) format(x, scientific = TRUE, trim = TRUE),

    # --- Optional baseline ---
    include_baseline = FALSE,
    baseline_solution = NULL,
    baseline_title = NULL
) {

  stopifnot(length(results) >= 1)

  # Extract solution objects for plotting
  sol_list <- purrr::map(results, get_solution_fun)

  # ---- Build titles (priority order) ----
  if (!is.null(titles)) {
    stopifnot(length(titles) == length(sol_list))
    plot_titles <- titles

  } else if (!is.null(title_fun)) {
    plot_titles <- purrr::map_chr(seq_along(sol_list), ~ title_fun(.x, results))

  } else if (!is.null(subtitle_values)) {
    stopifnot(length(subtitle_values) == length(sol_list))
    plot_titles <- paste0(title_prefix, vapply(subtitle_values, value_label_fun, character(1)))

  } else {
    plot_titles <- paste("Run", seq_along(sol_list))
  }

  # ---- Make plots ----
  p_list <- purrr::map2(
    sol_list,
    plot_titles,
    ~ plot_prioritizr(.x) +
      ggplot2::ggtitle(.y) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = title_size),
        legend.position = "none"
      )
  )

  # ---- Optional baseline (prepended) ----
  if (isTRUE(include_baseline)) {
    if (is.null(baseline_solution)) {
      stop("include_baseline = TRUE, but baseline_solution is NULL.")
    }

    p_base <- plot_prioritizr(baseline_solution) +
      ggplot2::ggtitle(baseline_title) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = title_size),
        legend.position = "none"
      )

    p_list <- c(list(p_base), p_list)
  }

  # ---- Wrap and collect legend once ----
  patchwork::wrap_plots(p_list, ncol = ncol, guides = "collect") &
    ggplot2::theme(legend.position = legend_position)
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

#' Make MinPatch summary table
#'
#' Builds a single table summarising a baseline solution and multiple MinPatch solutions.
#' Baseline patch metrics come from `compare_solutions(... )$overall[,"Original"]`, while
#' MinPatch metrics come from `compare_solutions(... )$overall[,"MinPatch"]`.
#'
#' @param region_sf sf of all planning units (used to compute median PU area for min patch size).
#' @param baseline_solution_sf sf containing the baseline solution selection column and PU costs.
#' @param minpatch_results List of MinPatch results; each must contain `$solution` (sf).
#' @param multipliers Numeric vector; same length/order as `minpatch_results`.
#' @param compare_solutions_fun Function that returns a list with `$overall`.
#'   If NULL, the function will try to find `compare_solutions()` in your session.
#' @param baseline_compare_obj Object to pass into compare_solutions() for baseline "Original" metrics.
#'   If NULL, defaults to `minpatch_results[[1]]`.
#' @param baseline_elapsed Baseline runtime (seconds). Optional.
#' @param minpatch_elapsed MinPatch runtimes (seconds). Optional; must match `multipliers`.
#' @param projected_epsg EPSG for perimeter calcs if inputs are lon/lat (default 32740).
#' @param baseline_solution_col Baseline selection column name (1/0). If NULL, auto-detects.
#' @param minpatch_solution_col MinPatch selection column in `$solution` (default "minpatch").
#' @param cost_col PU cost column in `baseline_solution_sf` (default "cost").
#' @param baseline_boundary_cost Baseline boundary cost (default 0).
#' @param make_kable If TRUE, also returns an HTML kable.
#'
#' @return list(data = tibble, kable = html or NULL)
#' @export
make_minpatch_summary_table <- function(
    region_sf,
    baseline_solution_sf,
    minpatch_results,
    multipliers,
    compare_solutions_fun = NULL,
    baseline_compare_obj = NULL,
    baseline_elapsed = NA_real_,
    minpatch_elapsed = NULL,
    projected_epsg = 32740,
    baseline_solution_col = NULL,
    minpatch_solution_col = "minpatch",
    cost_col = "cost",
    baseline_boundary_cost = 0,
    make_kable = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("purrr", quietly = TRUE)) stop("Package 'purrr' is required.")
  if (isTRUE(make_kable)) {
    if (!requireNamespace("knitr", quietly = TRUE)) stop("Package 'knitr' is required for make_kable=TRUE.")
    if (!requireNamespace("kableExtra", quietly = TRUE)) stop("Package 'kableExtra' is required for make_kable=TRUE.")
  }

  # --- Resolve compare_solutions() robustly ---
  if (is.null(compare_solutions_fun)) {
    if (exists("compare_solutions", mode = "function", inherits = TRUE)) {
      compare_solutions_fun <- get("compare_solutions", mode = "function")
    } else {
      ga <- utils::getAnywhere("compare_solutions")
      if (length(ga$objs) > 0) {
        compare_solutions_fun <- ga$objs[[1]]
      } else {
        stop(
          "compare_solutions() was not found.\n",
          "Fix: load the package/script that defines compare_solutions(), or pass it in as compare_solutions_fun=."
        )
      }
    }
  }

  # --- helper to extract a metric ---
  get_metric <- function(overall, column, metric) {
    out <- overall[[column]][overall$Metric == metric]
    if (length(out) == 0) NA_real_ else out
  }

  # --- total OUTER perimeter (km) of selected PUs ---
  total_outer_perimeter_km <- function(solution_sf, solution_col) {
    sel <- solution_sf %>% dplyr::filter(.data[[solution_col]] == 1)
    if (nrow(sel) == 0) return(NA_real_)
    u <- sf::st_union(sf::st_geometry(sel))
    per_m <- as.numeric(sf::st_length(sf::st_cast(u, "MULTILINESTRING")))
    per_m / 1000
  }

  # --- sum of PU costs over selected PUs ---
  pu_cost <- function(solution_sf, solution_col, cost_col = "cost") {
    if (!cost_col %in% names(solution_sf)) {
      stop("Cost column '", cost_col, "' not found in baseline_solution_sf.")
    }
    sel <- solution_sf %>% dplyr::filter(.data[[solution_col]] == 1)
    sum(sel[[cost_col]], na.rm = TRUE)
  }

  # --- checks ---
  if (length(minpatch_results) != length(multipliers)) {
    stop("minpatch_results and multipliers must have the same length.")
  }
  if (!is.null(minpatch_elapsed) && length(minpatch_elapsed) != length(multipliers)) {
    stop("minpatch_elapsed must be NULL or same length as multipliers.")
  }

  # --- auto-detect baseline selection column if not provided ---
  if (is.null(baseline_solution_col)) {
    baseline_solution_col <- if ("solution_1" %in% names(baseline_solution_sf)) "solution_1" else
      if ("prioritizr" %in% names(baseline_solution_sf)) "prioritizr" else
        if ("solution" %in% names(baseline_solution_sf)) "solution" else
          stop("Could not auto-detect baseline_solution_col; please pass it explicitly.")
  }

  # --- project to metres for perimeter if inputs are lon/lat ---
  to_projected_if_longlat <- function(x) {
    if (inherits(x, "sf") && sf::st_is_longlat(x)) sf::st_transform(x, projected_epsg) else x
  }
  region_sf_proj <- to_projected_if_longlat(region_sf)
  baseline_sf_proj <- to_projected_if_longlat(baseline_solution_sf)
  minpatch_results_proj <- purrr::map(minpatch_results, function(res) {
    res$solution <- to_projected_if_longlat(res$solution)
    res
  })

  # --- min patch size lookup (km^2) from median PU area ---
  median_pu_area_m2  <- stats::median(sf::st_area(region_sf_proj))
  min_patch_lookup <- tibble::tibble(
    multiplier = multipliers,
    min_patch_km2 = as.numeric(multipliers * median_pu_area_m2) / 1e6
  )

  # --- runtime lookup (seconds) ---
  runtime_lookup <- tibble::tibble(
    scenario    = c("Baseline", paste0("MinPatch \u00d7", multipliers)),
    runtime_sec = c(as.numeric(baseline_elapsed),
                    if (is.null(minpatch_elapsed)) rep(NA_real_, length(multipliers)) else as.numeric(minpatch_elapsed))
  )

  # --- baseline overall metrics (Original) ---
  if (is.null(baseline_compare_obj)) baseline_compare_obj <- minpatch_results[[1]]
  overall0 <- compare_solutions_fun(baseline_compare_obj)$overall

  baseline_pu_cost_val <- pu_cost(baseline_sf_proj, baseline_solution_col, cost_col = cost_col)
  baseline_overall_cost_val <- baseline_pu_cost_val + baseline_boundary_cost

  baseline_row <- tibble::tibble(
    scenario                 = "Baseline",
    multiplier               = NA_real_,
    min_patch_size_km2       = NA_real_,
    selected_pu              = get_metric(overall0, "Original", "Selected Planning Units"),
    total_area_km2           = get_metric(overall0, "Original", "Total Area") / 1e6,
    total_perimeter_km       = total_outer_perimeter_km(baseline_sf_proj, baseline_solution_col),
    n_patches                = get_metric(overall0, "Original", "Number of Patches"),
    valid_patches_ge_min     = get_metric(overall0, "Original", "Valid Patches (>= min size)"),
    median_patch_km2         = get_metric(overall0, "Original", "Median Patch Size") / 1e6,
    total_pu_cost            = baseline_pu_cost_val,
    boundary_cost            = baseline_boundary_cost,
    overall_cost             = baseline_overall_cost_val
  )

  # --- MinPatch rows ---
  minpatch_rows <- purrr::map2_dfr(minpatch_results_proj, multipliers, function(res, mult) {
    overall <- compare_solutions_fun(res)$overall
    tibble::tibble(
      scenario                 = paste0("MinPatch \u00d7", mult),
      multiplier               = mult,
      min_patch_size_km2       = min_patch_lookup$min_patch_km2[min_patch_lookup$multiplier == mult],
      selected_pu              = get_metric(overall, "MinPatch", "Selected Planning Units"),
      total_area_km2           = get_metric(overall, "MinPatch", "Total Area") / 1e6,
      total_perimeter_km       = total_outer_perimeter_km(res$solution, minpatch_solution_col),
      n_patches                = get_metric(overall, "MinPatch", "Number of Patches"),
      valid_patches_ge_min     = get_metric(overall, "MinPatch", "Valid Patches (>= min size)"),
      median_patch_km2         = get_metric(overall, "MinPatch", "Median Patch Size") / 1e6,
      total_pu_cost            = get_metric(overall, "MinPatch", "Planning Unit Cost"),
      boundary_cost            = get_metric(overall, "MinPatch", "Boundary Cost"),
      overall_cost             = get_metric(overall, "MinPatch", "Total Cost")
    )
  })

  # --- combine + format ---
  solution_summary <- dplyr::bind_rows(baseline_row, minpatch_rows) %>%
    dplyr::left_join(runtime_lookup, by = "scenario") %>%
    dplyr::mutate(
      multiplier = dplyr::if_else(is.na(.data$multiplier), "-", as.character(.data$multiplier)),
      dplyr::across(tidyselect::all_of(c("selected_pu", "n_patches", "valid_patches_ge_min")),
                    ~ as.integer(round(.x))),
      dplyr::across(tidyselect::all_of(c("min_patch_size_km2", "total_area_km2", "median_patch_km2",
                      "total_perimeter_km","total_pu_cost", "boundary_cost",
                      "overall_cost")),
                    ~ round(.x, 2)),
      runtime_sec = round(.data$runtime_sec, 1)
    ) %>%
    dplyr::select(
      .data$scenario,
      `Multiplier`                  = .data$multiplier,
      `Minimum patch size (km^2)`   = .data$min_patch_size_km2,
      `Selected PUs`                = .data$selected_pu,
      `Total area (km^2)`           = .data$total_area_km2,
      `Total perimeter (km)`        = .data$total_perimeter_km,
      `# patches`                   = .data$n_patches,
      `Valid patches (>= min size)` = .data$valid_patches_ge_min,
      `Median patch size (km^2)`    = .data$median_patch_km2,
      `Total PU cost`               = .data$total_pu_cost,
      `Boundary cost`               = .data$boundary_cost,
      `Overall cost`                = .data$overall_cost,
      `Runtime (s)`                 = .data$runtime_sec
    )

  out <- list(data = solution_summary, kable = NULL)

  if (isTRUE(make_kable)) {
    out$kable <- knitr::kable(solution_summary, format = "html", align = "l") |>
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        full_width = FALSE,
        position = "left"
      ) |>
      kableExtra::row_spec(0, bold = TRUE)
  }

  out
}


#' Plot rook-adjacency patches for a MinPatch solution and flag valid/invalid patches
#'
#' A patch is a rook-connected cluster of selected planning units (edge-sharing).
#' A patch is "valid" if its area >= (multiplier x median PU area), with a small
#' numeric tolerance to avoid floating-point edge cases.
#'
#' @param multiplier Numeric. The MinPatch multiplier to plot (must exist in `multipliers`).
#' @param multipliers Numeric vector of multipliers (same order as `minpatch_results`).
#' @param minpatch_results List. Each element contains a `$solution` sf object with column `minpatch`.
#' @param pu_sf sf. Planning-unit sf used to compute median PU area (defaults to Seychelles_sf if present).
#' @param return Character. "plot" (ggplot), "counts" (tibble), or "patches" (sf patch polygons).
#' @param do_snap Logical. If TRUE, snap geometries before adjacency for robustness.
#' @param snap_size Numeric. Grid size passed to `lwgeom::st_snap_to_grid()` (CRS units).
#' @param eps_rel Numeric. Relative tolerance applied to the area threshold (default 1e-9).
#' @param debug Logical. If TRUE, prints patch table (including diff vs threshold).
#'
#' @return ggplot object, or tibble, or sf patch polygons (see `return`).
#' @export
plot_patch_validity <- function(
    multiplier,
    multipliers,
    minpatch_results,
    pu_sf = NULL,
    return = c("plot", "counts", "patches"),
    do_snap = TRUE,
    snap_size = 1,
    eps_rel = 1e-9,
    debug = FALSE
) {

  return <- match.arg(return)

  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("igraph", quietly = TRUE)) stop("Package 'igraph' is required.")
  if (!requireNamespace("shadowtext", quietly = TRUE)) stop("Package 'shadowtext' is required.")
  if (!requireNamespace("lwgeom", quietly = TRUE)) stop("Package 'lwgeom' is required.")

  # fallback PU sf for threshold
  if (is.null(pu_sf)) {
    if (exists("Seychelles_sf", inherits = TRUE)) {
      pu_sf <- get("Seychelles_sf", inherits = TRUE)
    } else {
      stop("Provide `pu_sf` (planning-unit sf) or ensure `Seychelles_sf` exists.")
    }
  }

  # locate run
  i_run <- which(multipliers == multiplier)
  stopifnot(length(i_run) == 1)

  sol <- minpatch_results[[i_run]]$solution
  stopifnot("minpatch" %in% names(sol))

  # consistent PU id for joins/labels
  sol <- sol %>%
    dplyr::mutate(
      pu_id    = dplyr::row_number(),
      selected = (.data$minpatch == 1)
    )

  # ---- threshold area (m^2): multiplier x median PU area (numeric) ----
  median_pu_area_m2 <- stats::median(as.numeric(sf::st_area(pu_sf)), na.rm = TRUE)
  min_patch_area_m2 <- as.numeric(multiplier) * as.numeric(median_pu_area_m2)

  # selected units only (for patch detection)
  sel <- sol %>% dplyr::filter(.data$selected)
  stopifnot(nrow(sel) > 0)

  # optionally snap + validate (only for patch counting/unioning)
  sel2 <- sel
  if (isTRUE(do_snap)) {
    sel2 <- sel2 %>% lwgeom::st_snap_to_grid(size = snap_size)
  }
  sel2 <- sel2 %>% sf::st_make_valid()

  # rook adjacency = edge sharing only
  nb_list <- sf::st_relate(sel2, sel2, pattern = "F***1****")

  edges <- do.call(
    rbind,
    lapply(seq_along(nb_list), function(i) {
      if (length(nb_list[[i]]) == 0) return(NULL)
      cbind(from = i, to = nb_list[[i]])
    })
  )

  g <- igraph::make_empty_graph(n = nrow(sel2), directed = FALSE)
  if (!is.null(edges) && nrow(edges) > 0) {
    g <- igraph::add_edges(g, as.vector(t(edges)))
  }

  comp <- igraph::components(g)
  sel2$patch_id <- comp$membership

  # numeric tolerance to avoid floating-point edge cases
  eps <- eps_rel * min_patch_area_m2

  # ---- patch polygons + patch size (area) + validity ----
  patch_info <- sel2 %>%
    dplyr::group_by(.data$patch_id) %>%
    dplyr::summarise(
      n_pu     = dplyr::n(),
      geometry = sf::st_union(.data$geometry),
      .groups  = "drop"
    ) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(
      patch_area = as.numeric(sf::st_area(.data$geometry)),         # m^2
      is_valid   = (.data$patch_area + eps) >= min_patch_area_m2,
      label_pt   = sf::st_point_on_surface(.data$geometry)
    )

  # optional debug table
  if (isTRUE(debug)) {
    print(
      patch_info %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(
          patch_km2  = .data$patch_area / 1e6,
          thresh_km2 = min_patch_area_m2 / 1e6,
          diff_km2   = .data$patch_km2 - .data$thresh_km2
        ) %>%
        dplyr::arrange(.data$is_valid, .data$patch_area)
    )
  }

  # counts
  n_patches         <- nrow(patch_info)
  n_valid_patches   <- sum(patch_info$is_valid)
  n_invalid_patches <- n_patches - n_valid_patches

  if (return == "counts") {
    return(tibble::tibble(
      scenario          = paste0("MinPatch \u00d7", multiplier),
      new_n_patches     = as.integer(n_patches),
      new_valid_patches = as.integer(n_valid_patches),
      new_invalid_patches = as.integer(n_invalid_patches),
      threshold_km2     = min_patch_area_m2 / 1e6,
      snapped           = isTRUE(do_snap)
    ))
  }

  if (return == "patches") {
    return(patch_info)
  }

  # label coords
  xy <- sf::st_coordinates(patch_info$label_pt)
  patch_info$x <- xy[, 1]
  patch_info$y <- xy[, 2]

  # join validity back to PUs
  sel_class <- sel2 %>%
    sf::st_drop_geometry() %>%
    dplyr::select(.data$pu_id, .data$patch_id) %>%
    dplyr::left_join(
      patch_info %>% sf::st_drop_geometry() %>% dplyr::select(.data$patch_id, .data$is_valid),
      by = "patch_id"
    ) %>%
    dplyr::mutate(status = ifelse(.data$is_valid, "Valid patch", "Invalid patch"))

  sol_plot <- sol %>%
    dplyr::left_join(sel_class %>% dplyr::select(.data$pu_id, .data$status), by = "pu_id") %>%
    dplyr::mutate(
      status = ifelse(is.na(.data$status), "Not selected", .data$status),
      status = factor(.data$status, levels = c("Not selected", "Valid patch", "Invalid patch"))
    )

  cat(
    "MinPatch ", multiplier, "x: ",
    n_patches, " patches | ",
    n_valid_patches, " valid | ",
    n_invalid_patches, " invalid",
    " (threshold = ", round(min_patch_area_m2 / 1e6, 3), " km\u00b2) | ",
    "snap = ", isTRUE(do_snap),
    "\n",
    sep = ""
  )

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = sol_plot,
      ggplot2::aes(fill = .data$status),
      colour = "white",
      linewidth = 0.15
    ) +
    shadowtext::geom_shadowtext(
      data = patch_info,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$patch_id),
      size = 4,
      fontface = "bold",
      colour = "black",
      bg.colour = "white",
      bg.r = 0.12
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Not selected"  = "grey92",
        "Valid patch"   = "#2ca25f",
        "Invalid patch" = "red"
      )
    ) +
    ggplot2::labs(
      title = paste0("MinPatch: ", multiplier, "\u00d7 median PU area"),
      subtitle = paste0(
        "Patches (rook adjacency): ",
        n_patches, " total | ",
        n_valid_patches, " valid | ",
        n_invalid_patches, " invalid",
        " | valid if patch area \u2265 ",
        multiplier, "\u00d7 median PU area",
        " | snap = ", isTRUE(do_snap)
      ),
      fill = NULL
    ) +
    ggplot2::theme_minimal()
}
