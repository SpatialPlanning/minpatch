# ============================================================
# Using oceandatr + minpatch in a marine planning region
# Seychelles example (plain R script)
# ============================================================

# ----------------------------
# 0. Setup
# ----------------------------
options(scipen = 999)
set.seed(123)

library(oceandatr)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(prioritizr)
#library(minpatch)
devtools::load_all()
library(purrr)
library(tibble)

# optional (only if you actually use it later)
# install.packages("kableExtra")
# library(kableExtra)

# ----------------------------
# 1. Study region: Seychelles EEZ
# ----------------------------
Seychelles_eez <- get_boundary(name = "Seychelles")

# quick visual check
plot(Seychelles_eez[1],
     col = "lightgreen",
     main = "Seychelles EEZ",
     axes = TRUE)

# ----------------------------
# 2. Choose equal-area projection
# ----------------------------
projection_Seychelles <- "+proj=laea +lon_0=55 +lat_0=-4.5 +datum=WGS84 +units=m +no_defs"

# ----------------------------
# 3. Create planning-unit grid
# ----------------------------
Seychelles_grid <- get_grid(
  boundary   = Seychelles_eez,
  resolution = 30000,   # 30 km grid for runtime; reduce for finer resolution
  crs        = projection_Seychelles
)

# project EEZ for plotting
Seychelles_eez_proj <- Seychelles_eez %>%
  st_transform(crs = projection_Seychelles) %>%
  st_geometry()

# plot grid
terra::plot(Seychelles_grid,
            col = "gold3",
            axes = FALSE,
            legend = FALSE,
            main = "Seychelles spatial grid (30 km)")
plot(Seychelles_eez_proj,
     add = TRUE,
     border = "black",
     lwd = 1)

# convert grid to sf polygons (minpatch needs sf)
Seychelles_pu <- Seychelles_grid %>%
  stars::st_as_stars() %>%
  sf::st_as_sf() %>%
  dplyr::mutate(
    id   = dplyr::row_number(),
    cost = as.numeric(sf::st_area(.)) / 1e6  # cost = area in km^2
  ) %>%
  dplyr::select(-layer)

# ----------------------------
# 4. Build marine features with oceandatr
# ----------------------------
set.seed(500)  # reproducibility for enviro zones sampling

feature_stack <- get_features(spatial_grid = Seychelles_grid) %>%
  remove_empty_layers()

# tidy names
names(feature_stack) <- gsub("_", " ", names(feature_stack)) %>%
  stringr::str_to_sentence()

# (optional) quick look: plot first few layers
# terra::plot(feature_stack[[1:4]])

# convert features to sf table (drop geometry)
features_df <- feature_stack %>%
  stars::st_as_stars() %>%
  sf::st_as_sf() %>%
  dplyr::mutate(id = dplyr::row_number()) %>%
  sf::st_drop_geometry()

# combine features + PU polygons
Seychelles_sf <- Seychelles_pu %>%
  dplyr::left_join(
    as.data.frame(features_df),
    by = "id"
  )

# feature column names (excluding id)
feature_names <- names(features_df) %>%
  dplyr::setdiff("id")

# ----------------------------
# 5. Build + solve prioritizr baseline
# ----------------------------
p_base <- prioritizr::problem(
  x           = Seychelles_sf,
  features    = feature_names,
  cost_column = "cost"
) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.30) %>%
  add_binary_decisions() %>%
  add_rsymphony_solver(verbose = FALSE)

t_base <- system.time({
  s_base <- solve(p_base)
})

p_base_plot <- plot_prioritizr(s_base) +
  ggtitle("Baseline (no MinPatch)")

print(p_base_plot)

# ----------------------------
# 6. MinPatch parameters
# ----------------------------
median_pu_area_m2  <- median(st_area(Seychelles_sf))
median_pu_area_km2 <- median_pu_area_m2 / 1e6

multipliers <- c(5, 10, 20)

patch_sizes_m2  <- multipliers * median_pu_area_m2
patch_sizes_km2 <- patch_sizes_m2 / 1e6

patch_radius <- sqrt(median_pu_area_m2 * 10 / pi)

minpatch_param_summary <- tibble::tibble(
  multiplier         = multipliers,
  min_patch_m2       = patch_sizes_m2,
  min_patch_km2      = patch_sizes_km2,
  median_pu_m2       = median_pu_area_m2,
  median_pu_area_km2 = median_pu_area_km2
)

cat("\nMinPatch parameters:\n")
cat("- Median planning unit area:", round(median_pu_area_km2, 3), "km^2\n\n")

for (i in seq_along(multipliers)) {
  cat("Multiplier:", multipliers[i], "x median PU area\n")
  cat("  - Minimum patch size:", round(patch_sizes_km2[i], 2), "km^2\n")
  cat("  - Corresponds to ≈", round(patch_sizes_km2[i] / median_pu_area_km2, 2), "planning units\n\n")
}

median_pu_length <- sqrt(median_pu_area_m2)
patch_radius_cells <- patch_radius / median_pu_length

cat("Patch radius used:\n")
cat("  -", round(patch_radius, 1), "m (≈", round(patch_radius / 1000, 2), "km)\n")
cat("  - Corresponds to ≈", round(patch_radius_cells, 2), "median planning-unit lengths\n")

# ----------------------------
# 7. Run MinPatch across patch-size multipliers
# ----------------------------
minpatch_results <- vector("list", length(patch_sizes_m2))
minpatch_times <- numeric(length(patch_sizes_m2))

for (i in seq_along(patch_sizes_m2)) {

  cat("\n============================================\n")
  cat("Running MinPatch with min patch area ~",
      round(patch_sizes_km2[i], 2), "km^2 (",
      multipliers[i], "x median PU)\n")
  cat("============================================\n")

  t_mp <- system.time({
    minpatch_results[[i]] <- run_minpatch(
      prioritizr_problem   = p_base,
      prioritizr_solution  = s_base,
      min_patch_size       = patch_sizes_m2[i],
      patch_radius         = patch_radius,
      boundary_penalty     = 0,
      remove_small_patches = TRUE,
      add_patches          = TRUE,
      whittle_patches      = TRUE,
      verbose              = TRUE
    )
  })

  minpatch_times[i] <- t_mp[["elapsed"]]
}

names(minpatch_results) <- paste0("minpatch_", multipliers, "x")
names(minpatch_times)   <- paste0("minpatch_", multipliers, "x")

# ----------------------------
# 8. Plot baseline vs MinPatch solutions (2x2)
# ----------------------------
get_solution_with_solution1 <- function(x) {
  sol <- x$solution
  extra_cols <- setdiff(names(sol), names(x$planning_units))
  if (!"solution_1" %in% names(sol) && length(extra_cols) >= 1) {
    sol$solution_1 <- sol[[extra_cols[1]]]
  }
  sol
}

sol_list <- purrr::map(minpatch_results, get_solution_with_solution1)

p_list <- purrr::map2(
  sol_list,
  multipliers,
  ~ plot_prioritizr(.x) +
    ggtitle(paste0("MinPatch: ", .y, "× median PU area"))
)

p_1 <- p_list[[1]]
p_2 <- p_list[[2]]
p_3 <- p_list[[3]]

p_base_plot <- plot_prioritizr(s_base) +
  ggtitle("Baseline (no MinPatch)")

print(
  (p_base_plot | p_1) /
    (p_2        | p_3)
)








# ----------------------------
# 9. Boundary penalty experiments (result3, result4, result5)
# ----------------------------
median_area <- median(st_area(Seychelles_sf))
min_patch_size <- median_area * 5
patch_radius_bp <- sqrt(median_area * 10)

t3 <- system.time({
  result3 <- run_minpatch(
    prioritizr_problem   = p_base,
    prioritizr_solution  = s_base,
    min_patch_size       = min_patch_size,
    patch_radius         = patch_radius_bp,
    boundary_penalty     = 0,
    remove_small_patches = TRUE,
    add_patches          = TRUE,
    whittle_patches      = TRUE,
    verbose              = TRUE,
    debug_boundary = TRUE,
    debug_boundary_every = 50
  )
})
cat("result3 runtime (sec):", t3[["elapsed"]], "\n")

t4 <- system.time({
  result4 <- run_minpatch(
    prioritizr_problem   = p_base,
    prioritizr_solution  = s_base,
    min_patch_size       = min_patch_size,
    patch_radius         = patch_radius_bp,
    boundary_penalty     = 1e-5,
    remove_small_patches = TRUE,
    add_patches          = TRUE,
    whittle_patches      = TRUE,
    verbose              = TRUE,
    debug_boundary = TRUE,
    debug_boundary_every = 50
  )
})
cat("result4 runtime (sec):", t4[["elapsed"]], "\n")

t5 <- system.time({
  result5 <- run_minpatch(
    prioritizr_problem   = p_base,
    prioritizr_solution  = s_base,
    min_patch_size       = min_patch_size,
    patch_radius         = patch_radius_bp,
    boundary_penalty     = 10,
    remove_small_patches = TRUE,
    add_patches          = TRUE,
    whittle_patches      = TRUE,
    verbose              = TRUE,
    debug_boundary = TRUE,
    debug_boundary_every = 50
  )
})
cat("result5 runtime (sec):", t5[["elapsed"]], "\n")

# ----------------------------
# 10. Plot baseline vs boundary-penalty MinPatch runs
# ----------------------------
boundary_penalties <- c(0, 1e-5, 10)
boundary_results <- list(result3, result4, result5)

sol_list_bp <- purrr::map(boundary_results, get_solution_with_solution1)

p_base_plot <- plot_prioritizr(s_base) +
  ggtitle("Baseline (no MinPatch)")

p_list_bp <- purrr::map2(
  sol_list_bp,
  boundary_penalties,
  ~ plot_prioritizr(.x) +
    ggtitle(paste0("MinPatch: boundary penalty = ", .y))
)

p_bp1 <- p_list_bp[[1]]
p_bp2 <- p_list_bp[[2]]
p_bp3 <- p_list_bp[[3]]

print(
  (p_base_plot | p_bp1) /
    (p_bp2      | p_bp3)
)

# ----------------------------
# 11. Compare solutions table helper + summary for boundary penalty runs
# ----------------------------
get_metric <- function(overall, column, metric) {
  out <- overall[[column]][overall$Metric == metric]
  if (length(out) == 0) NA_real_ else out
}

minpatch_results_bp <- list(result3, result4, result5)
minpatch_runtimes <- c(t3[["elapsed"]], t4[["elapsed"]], t5[["elapsed"]])

overall0 <- compare_solutions(minpatch_results_bp[[1]])$overall

baseline_row <- tibble::tibble(
  scenario           = "baseline",
  boundary_penalty   = NA_real_,
  selected_pu        = get_metric(overall0, "Original", "Selected Planning Units"),
  total_area         = get_metric(overall0, "Original", "Total Area"),
  n_patches          = get_metric(overall0, "Original", "Number of Patches"),
  valid_patches      = get_metric(overall0, "Original", "Valid Patches (>= min size)"),
  median_patch_size  = get_metric(overall0, "Original", "Median Patch Size"),
  planning_unit_cost = get_metric(overall0, "Original", "Planning Unit Cost"),
  boundary_cost      = get_metric(overall0, "Original", "Boundary Cost"),
  total_cost         = get_metric(overall0, "Original", "Total Cost"),
  runtime_sec        = t_base[["elapsed"]]
)

minpatch_rows <- purrr::pmap_dfr(
  list(minpatch_results_bp, as.list(boundary_penalties), as.list(minpatch_runtimes)),
  function(res, bp, rt) {
    overall <- compare_solutions(res)$overall

    tibble::tibble(
      scenario           = paste0("minpatch_bp", bp),
      boundary_penalty   = bp,
      selected_pu        = get_metric(overall, "MinPatch", "Selected Planning Units"),
      total_area         = get_metric(overall, "MinPatch", "Total Area"),
      n_patches          = get_metric(overall, "MinPatch", "Number of Patches"),
      valid_patches      = get_metric(overall, "MinPatch", "Valid Patches (>= min size)"),
      median_patch_size  = get_metric(overall, "MinPatch", "Median Patch Size"),
      planning_unit_cost = get_metric(overall, "MinPatch", "Planning Unit Cost"),
      boundary_cost      = get_metric(overall, "MinPatch", "Boundary Cost"),
      total_cost         = get_metric(overall, "MinPatch", "Total Cost"),
      runtime_sec        = rt
    )
  }
)

solution_summary_bp <- dplyr::bind_rows(baseline_row, minpatch_rows)
print(solution_summary_bp)
View(solution_summary_bp)
# ----------------------------
# 12. Individual summaries loop (optional)
# ----------------------------
for (i in seq_along(minpatch_results_bp)) {
  res <- minpatch_results_bp[[i]]
  bp  <- boundary_penalties[i]

  cat("\n\n====================================\n")
  cat("Scenario: boundary penalty =", format(bp, scientific = TRUE), "\n")
  cat("====================================\n")

  cat("\n-- MinPatch processing summary --\n")
  print_minpatch_summary(res)

  comparison <- compare_solutions(res)

  cat("\n-- Overall comparison --\n")
  print(comparison$overall)

  cat("\n-- Feature-level comparison --\n")
  print(comparison$features)

  cat("\n-- Feature change summary --\n")
  print(comparison$summary)
}

