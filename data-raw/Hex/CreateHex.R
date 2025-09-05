# Some code to create a hex sticker
#
# Last updated: Monday 5th September 2025
#
# Jason D. Everett (UQ/CSIRO/UNSW)
#
# For installation instructions see here:
# https://github.com/GuangchuangYu/hexSticker

# devtools::install_github("GuangchuangYu/hexSticker")





library(minpatch)
library(prioritizr)
library(sf)
library(terra)

dat <- c(get_sim_pu_raster(), get_sim_features()) %>%
  as.polygons(dissolve = FALSE, values = TRUE) %>%
  sf::st_as_sf() %>%
  dplyr::rename(cost = layer)

features = colnames(dat) %>%
  stringr::str_subset("feature_")


p <- problem(dat, features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.17) %>%  # 17% of each feature
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

s <- solve(p)

median_area <- median(st_area(dat))

# Set minimum patch size to 5x median planning unit area
min_patch_size <- median_area * 5

# Set patch radius to encompass approximately 10 planning units
patch_radius <- sqrt(median_area * 10)

# Run MinPatch with automatic data extraction from prioritizr objects
result <- run_minpatch(
  prioritizr_problem = p,
  prioritizr_solution = s,
  min_patch_size = min_patch_size,
  patch_radius = patch_radius,
  boundary_penalty = 0.001,  # Small boundary penalty for connectivity
  remove_small_patches = TRUE,
  add_patches = TRUE,
  whittle_patches = TRUE,
  verbose = TRUE
)


# Extract original solution for comparison
original_solution <- s$solution_1

# Create a simple map showing the changes
dat$original_solution <- original_solution
dat$minpatch_solution <- result$solution

# Create change categories
dat$change <- "No Change"
dat$change[original_solution == 0 & result$solution == 1] <- "Added by MinPatch"
dat$change[original_solution == 1 & result$solution == 0] <- "Removed by MinPatch"
dat$change[original_solution == 1 & result$solution == 1] <- "Retained"

st_crs(dat) <- NA

library(ggplot2)
library(sf)

# Plot original solution
gg1 <- ggplot(data = dat) +
  geom_sf(aes(fill = as.logical(original_solution)), colour = NA) +
  scale_fill_manual(values = c("lightgray", "darkblue"),
                    name = "Original Solution") +
  theme_void() +
  theme(legend.position = "none") # This line turns off the legend

# Plot MinPatch solution
gg2 <- ggplot(data = dat) +
  geom_sf(aes(fill = as.logical(minpatch_solution)), colour = NA) +
  scale_fill_manual(values = c("lightgray", "darkgreen"),
                    name = "MinPatch Solution") +
  theme_void() +
  theme(legend.position = "none") # This line turns off the legend

arrow <- ggplot() +
  geom_segment(aes(x = 0.2, y = 0.5, xend = 0.8, yend = 0.5),
               arrow = arrow(length = unit(0.45, "cm"), type = "closed"), # Increased arrowhead length
               size = 1.2) + # Increased line thickness
  theme_void()

pw <- patchwork::wrap_plots(gg1, arrow, gg2) &
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Transparent plot background
    panel.background = element_rect(fill = "transparent", colour = NA) # Transparent panel background
  )



hexSticker::sticker(pw,
                    package = "minpatch",
                    p_y = 1.3,
                    p_color = "grey90",
                    p_size = 90,
                    s_x = 1.0,
                    s_y = 0.8,
                    s_width = 1.7,
                    s_height = 1.7,
                    h_fill = "#3B6E8F", #"#40B4ED",# "#54bceb", #
                    h_color = "#213f52", #"grey90",
                    # url = "planktonteam.github.io/planktonr",
                    u_color = "grey90",
                    # u_family = "sans",
                    u_size = 16.5,
                    u_x = 0.98,
                    u_y = 0.07,
                    dpi = 1000,
                    asp = 1,
                    filename = "data-raw/hex.png")
                    # filename = file.path("man", "figures", "planktonr.png"))

