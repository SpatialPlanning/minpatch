library(prioritizr)
library(prioritizrdata)
library(terra)
library(sf)
library(ggplot2)
library(dplyr)

#library(minpatch)

rm(list = c(
  "compare_solutions",
  "generate_minpatch_report",
  "plot_minpatch",
  "plot_prioritizr",
  "print_minpatch_summary",
  "run_minpatch"
))

devtools::load_all()
# Sourcing out the scripts locally
#source("R/utils-pipe.R")
#source("R/data_structures.R")
#source("R/cost_functions.R")
#source("R/patch_functions.R")
#source("R/whittling_functions.R")
#source("R/minpatch.R")
#source("R/output.R")


# load data
tas_pu <- get_tas_pu() %>%
  mutate(cost = cost*10000)

# At present minpatch works with sf objects. Here we convert the data to sf.
tas_features <- get_tas_features() %>%
  stars::st_as_stars() %>%
  sf::st_as_sf()

tas <- sf::st_interpolate_aw(tas_features, tas_pu, extensive = FALSE, keep_NA = FALSE, na.rm = FALSE) %>%
  st_join(tas_pu, join = st_equals)


features = tas %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-all_of(c("id", "cost", "locked_in", "locked_out"))) %>%
  names()

# Convert data to binary again
tas <- tas %>%
  mutate(across(all_of(features), ~ if_else(.x > 0, 1, 0)))

p <- problem(tas, features = features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.30) %>%  # 30% of each feature
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

s <- solve(p)

base<-plot_prioritizr(s) +
  labs(subtitle = "Base Solution")




# Calculate reasonable parameters based on planning unit characteristics
median_area <- median(st_area(tas))
min_patch_size <- median_area * 4
patch_radius <- sqrt(median_area * 10)



result <- run_minpatch(
  prioritizr_problem = p,
  prioritizr_solution = s,
  min_patch_size = min_patch_size,
  patch_radius = patch_radius,
  boundary_penalty = 0,
  remove_small_patches = TRUE,
  add_patches = TRUE,
  whittle_patches = TRUE,
  verbose = TRUE,
  debug_boundary = TRUE,
  debug_boundary_every = 50
)


result3 <- run_minpatch(
  prioritizr_problem = p,
  prioritizr_solution = s,
  min_patch_size = min_patch_size,
  patch_radius = patch_radius,
  boundary_penalty = 1,
  remove_small_patches = TRUE,
  add_patches = TRUE,
  whittle_patches = TRUE,
  verbose = TRUE,
  debug_boundary = TRUE,
  debug_boundary_every = 50
)

result4 <- run_minpatch(
  prioritizr_problem = p,
  prioritizr_solution = s,
  min_patch_size = min_patch_size,
  patch_radius = patch_radius,
  boundary_penalty = 10,
  remove_small_patches = TRUE,
  add_patches = TRUE,
  whittle_patches = TRUE,
  verbose = TRUE,
  debug_boundary = TRUE,
  debug_boundary_every = 50
)



patchwork::wrap_plots(plot_minpatch(result, title = "Boundary Penalty: 0"),
                      plot_minpatch(result3, title = "Boundary Penalty: 1"),
                      plot_minpatch(result4, title = "Boundary Penalty: 10"),
                      guides = "collect",
                      ncol = 3) &
  theme(legend.position = "bottom")


result_solution <- result$solution
result_solution <- result_solution |>
  dplyr::rename(solution_1 = minpatch)



result3_solution <- result3$solution
result3_solution <- result3_solution |>
  dplyr::rename(solution_1 = minpatch)


result4_solution <- result4$solution
result4_solution <- result4_solution |>
  dplyr::rename(solution_1 = minpatch)



result_plot <- plot_prioritizr(result_solution) +
  labs(subtitle = "MinPatch (4x): Boundary Penalty: 0")

result3_plot <- plot_prioritizr(result3_solution) +
  labs(subtitle = "MinPatch (4x): Boundary Penalty: 1")

result4_plot <- plot_prioritizr(result4_solution) +
  labs(subtitle = "MinPatch (4x): Boundary Penalty: 10")

library(patchwork)
(base | result_plot |
    result3_plot | result4_plot) +
  plot_layout(nrow = 2, guides = "collect") &
  theme(legend.position = "bottom")






# this is for result, boundary penalty = 0
# Looking at the results
print_minpatch_summary(result)

# Compare original vs MinPatch solutions
comparison <- compare_solutions(result)

# Print overall comparison
cat("=== Overall Solution Comparison ===\n")
print(comparison$overall)

# Print feature-level comparison
cat("\n=== Feature-Level Area Comparison ===\n")
print(comparison$features)

# Print summary statistics
cat("\n=== Feature Change Summary ===\n")
print(comparison$summary)



# this is for result, boundary penalty = 1
# Looking at the results
print_minpatch_summary(result3)

# Compare original vs MinPatch solutions
comparison <- compare_solutions(result3)

# Print overall comparison
cat("=== Overall Solution Comparison ===\n")
print(comparison$overall)

# Print feature-level comparison
cat("\n=== Feature-Level Area Comparison ===\n")
print(comparison$features)

# Print summary statistics
cat("\n=== Feature Change Summary ===\n")
print(comparison$summary)



# this is for result, boundary penalty = 10
# Looking at the results
print_minpatch_summary(result4)

# Compare original vs MinPatch solutions
comparison <- compare_solutions(result4)

# Print overall comparison
cat("=== Overall Solution Comparison ===\n")
print(comparison$overall)

# Print feature-level comparison
cat("\n=== Feature-Level Area Comparison ===\n")
print(comparison$features)

# Print summary statistics
cat("\n=== Feature Change Summary ===\n")
print(comparison$summary)





library(dplyr)
library(purrr)

# 1. Extract MinPatch column and rename
extract_minpatch <- function(comp, name){
  comp$overall %>%
    select(Metric, MinPatch) %>%
    rename(!!name := MinPatch)
}

# 2. Build summary table
summary_table <- reduce(
  list(
    extract_minpatch(compare_solutions(result),  "result"),
    extract_minpatch(compare_solutions(result3), "result3"),
    extract_minpatch(compare_solutions(result4), "result4")
  ),
  full_join,
  by = "Metric"
)

# 3. Print
print(summary_table)

