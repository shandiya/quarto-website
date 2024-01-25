# draws SB logo for website

library(dplyr)
library(ggplot2)
library(ggpattern)
library(purrr)

# 1. quarter circles --------
# function adapted from Bauhaus-inspired rtistry
# orientation refers to centre of circle
draw_quarter <- function(centre_x, centre_y, orientation,...){
  
  angle <- seq(0, pi/2, length.out = 25)
  
  x = case_when(
    orientation == "top_left" ~ centre_x + sin(angle),
    orientation == "bottom_left" ~ centre_x + sin(angle),
    orientation == "top_right" ~ centre_x - sin(angle),
    orientation == "bottom_right" ~ centre_x - sin(angle))
  
  y = case_when(
    orientation == "top_left" ~ centre_y - cos(angle),
    orientation == "bottom_left" ~ centre_y + cos(angle),
    orientation == "top_right" ~ centre_y - cos(angle),
    orientation == "bottom_right" ~ centre_y + cos(angle))
  
  centre <- c(centre_x, centre_y)
  df <- data.frame(x, y)
  df <- rbind(df, centre)
  return(df)
  
}

top_s <- draw_quarter(2, 1, "bottom_right")
bottom_s <- draw_quarter(0, 1, "top_left")
top_b <- draw_quarter(2.1, 2, "top_left")
bottom_b <- draw_quarter(2.1, 0, "bottom_left")

logo_quarter <- ggplot() +
  geom_polygon(data = top_s,
               aes(x, y),
               fill = "#ffaa00") +
  geom_polygon(data = bottom_s,
               aes(x, y),
               fill = "#ffaa00") +
  geom_polygon(data = top_b,
               aes(x, y),
               fill = "#ffaa00") +
  geom_polygon_pattern(data = bottom_b,
               aes(x, y),
               pattern = "stripe", 
               fill    = NA,
               colour  = NA,
               pattern_density = 0.2,
               pattern_colour = "#ffaa00",
               pattern_angle = 0,
               pattern_spacing = 0.02) +
  coord_equal() +
  theme_void()

ggsave("images/logo_quarter.png", logo_quarter, width = 6.2, height = 4, units = "in")

# 2. concentric curved lines --------

draw_halfcircle <- function(amplitude, direction, phase_x = 0, phase_y = 0) {
  
  theta <- seq(0, pi, length.out = 100)
  x <- amplitude * sin(theta) * direction + phase_x
  y <- amplitude * cos(theta) + phase_y
  df <- data.frame(x = x, y = y, group = amplitude * direction)
  return(df)

}

amplitude <- seq(1, 2, by = 0.2)

curve_left <- amplitude |> 
  map(draw_halfcircle, direction = -1) |> 
  list_rbind()  

curve_right <- amplitude |> 
  map(draw_halfcircle, direction = 1, phase_y = -3) |> 
  list_rbind()  

# option to draw some line in bold
draw_s <- curve_right |> 
  bind_rows(curve_left) |> 
  mutate(bold = if_else(abs(group) == 1, "bold", "reg")) |> 
  ggplot() +
  geom_path(aes(x, y, group = group)) +
  scale_size_manual(values = c(2, 1)) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")

ggsave("images/logo_wip.png", draw_s, width = 3, height = 3, units = "in")


