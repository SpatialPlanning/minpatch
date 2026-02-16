# Some code to create a hex sticker
#
# Last updated: Monday 5th September 2025
#
# Jason D. Everett (UQ/CSIRO/UNSW)
#
# For installation instructions see here:
# https://github.com/GuangchuangYu/hexSticker

# devtools::install_github("GuangchuangYu/hexSticker")

library(magick)

# 1. Load your background image
img <- image_read("minpatch_balanced.png")
info <- image_info(img)
w <- info$width
h <- info$height

# 2. Open the magick drawing device
mask <- image_blank(w, h, color = "transparent")
img_mask <- image_draw(mask)

# 3. Scale the Hexagon down (80% of image height)
# This provides a 10% margin at the top and 10% at the bottom
hex_h <- h * 0.8
hex_w <- hex_h * (sqrt(3)/2)

# Calculate the start points to keep it perfectly centered
start_y <- (h - hex_h) / 2
start_x <- (w - hex_w) / 2

# Precise "Pointy-Topped" coordinates with space around them
x_coords <- c(w/2,            w - start_x,    w - start_x,    w/2,           start_x,        start_x)
y_coords <- c(start_y,        start_y + (hex_h * 0.25), start_y + (hex_h * 0.75), h - start_y, start_y + (hex_h * 0.75), start_y + (hex_h * 0.25))

# 4. Draw the polygon
polygon(x_coords, y_coords, col = "white", border = NA)
dev.off()

# 5. Clip and Save
clipped_img <- image_composite(img_mask, img, operator = "In")

# Trim the excess transparency so the hexSticker package
# doesn't have to deal with a lot of empty space
final_img <- image_trim(clipped_img)
image_write(final_img, "minpatch_centered_hex.png")


hexSticker::sticker(
  subplot = "minpatch_centered_hex.png",
  package = "minPatch",
  p_size = 86,
  p_y = 1.2,
  p_color = "#00441b",
  s_x = 1,
  s_y = 1,
  s_width = 0.8,
  s_height = 0.8,
  h_fill = "#FFFFFF",
  h_color = "#00441b",
  filename = "minpatch.png",
  dpi = 1000,
)

# First, load a nice font (requires sysfonts)
sysfonts::font_add_google("Montserrat", "montserrat")
showtext::showtext_auto()

hexSticker::sticker(
  subplot = "minpatch_centered_hex.png",
  package = "minPatch",
  p_size = 86,                    # Adjusted for showtext/standard scaling
  p_y = 1.15,                     # Lowered slightly to feel more centered
  p_color = "#00441b",
  p_family = "sans",              # Or "montserrat" if loaded
  p_fontface = "bold",            # Bold is key for the 'pop'
  s_x = 1,
  s_y = 1,
  s_width = 0.8,                  # Since we pre-cropped, let's fill the hex
  s_height = 0.8,
  h_fill = "#FFFFFF",
  h_color = "#00441b",            # Match border to text for a 'unified' look
  h_size = 1.5,                   # Slightly thicker border adds 'premium' feel
  spotlight = TRUE,               # Adds a subtle radial gradient to the background
  l_alpha = 0.3,                  # Softness of the spotlight
  filename = "data-raw/minpatch.png",
  dpi = 1000
)







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

