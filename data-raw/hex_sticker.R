# Hex sticker for ggfunction
# Run this script from the package root to generate man/figures/logo.png

library(ggplot2)
library(ggfunction)
library(hexSticker)

# Lissajous curve: a nice 1D-to-2D parametric function
lissajous <- function(t, A = 1, B = 1, a = 5, b = 4, delta = pi / 2) {
  c(A * sin(a * t + delta), B * sin(b * t))
}

# Generate the subplot
p <- ggplot() +
  geom_function_1d_2d(
    fun = lissajous, tlim = c(0, 2 * pi), dt = 0.005,
    arrow = NULL, linewidth = 0.8,
    args = list(A = 1, B = 1, a = 5, b = 4, delta = pi / 2)
  ) +
  scale_color_viridis_c(option = "C", begin = 0.15, end = 0.85) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none")

# Build the hex sticker
sticker(
  subplot = p,
  package = "ggfunction",
  p_size = 18,
  p_y = 1.45,
  p_color = "#FFFFFF",
  s_x = 1, s_y = 0.85,
  s_width = 1.4, s_height = 1.1,
  h_fill = "#2C3E50",
  h_color = "#E74C3C",
  h_size = 1.5,
  url = "github.com/dusty-turner/ggfunction",
  u_size = 3.5,
  u_color = "#CCCCCC",
  filename = "man/figures/logo.png",
  dpi = 300
)

message("Hex sticker saved to man/figures/logo.png")
