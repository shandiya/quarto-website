
library(arrow)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggpattern)
library(ggnewscale)

# logo ------
# modified from bauhaus.R in variations on a theme

draw_half_circle <- function(start, end, v = 0, h = 0) {
  
  theta <- seq(start, end, length.out = 100)
  x <- sin(theta) + h
  y <- cos(theta) + v
  df <- data.frame(x = x, y = y)
  return(df)
  
}

top_left_half <- draw_half_circle(-pi/2, pi/2)
top_right_half <- draw_half_circle(-pi/2, pi/2, h = 0.8)
bottom_left_half <- draw_half_circle(pi/2, 1.5 * pi)
bottom_right_half <- draw_half_circle(pi/2, 1.5 * pi, h = 0.8)

fill_col <- '#fec44f'
alt_fill_col <- '#fffbf7'

ggplot() +
  geom_polygon(data = top_left_half,
               aes(x, y),
               fill = fill_col) +
  geom_polygon_pattern(data = top_right_half,
                       aes(x, y),
                       pattern = "stripe",
                       fill    = alt_fill_col,
                       colour  = NA,
                       pattern_density = 0.5,
                       pattern_colour = fill_col,
                       pattern_fill = fill_col,
                       pattern_angle = 0,
                       pattern_spacing = 0.11) +
  geom_polygon_pattern(data = bottom_right_half,
                       aes(x, y),
                       pattern = "stripe",
                       fill    = alt_fill_col,
                       colour  = NA,
                       pattern_density = 0.5,
                       pattern_colour = fill_col,
                       pattern_fill = fill_col,
                       pattern_angle = 0,
                       pattern_spacing = 0.11) +
  geom_polygon(data = bottom_left_half,
               aes(x, y),
               fill = fill_col) +
  coord_equal() +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA))

ggsave("images/logo.png", width = 3, height = 4, units = "in")

# title -----------
# uses aggregated biodiversity dataset from EcoAssets

dir.create("data/by_year")

bd <- open_dataset("data/biodiversity.csv", format = "csv")

bd |> 
  select(year, capadStatus, epbcStatus, griisStatus, speciesName, occurrenceCount) |> 
  write_dataset(path = "data/by_year", partitioning = "year")

get_summaries <- function(fpath) {
  
  year <- str_extract(fpath, "(?<==)\\d{4}")
  
  # capad
  capad <- fpath |> 
    read_parquet() |> 
    select(capadStatus, speciesName, occurrenceCount) |> 
    group_by(capadStatus, speciesName) |> 
    summarise(counts = sum(occurrenceCount), .groups = "drop") |> 
    pivot_wider(names_from = capadStatus, values_from = counts, values_fill = 0) |> 
    mutate(protected_only = case_when(
      (IPA + PA > 0) & `not protected` == 0 ~ "protected",
      .default = "other"
    ))
  
  capad_prop <- nrow(filter(capad, protected_only == "protected"))/nrow(capad)*100
  
  # unique spp
  n_spp <- nrow(capad)
  
  # griis
  griis <- fpath |> 
    read_parquet() |> 
    select(griisStatus, speciesName, occurrenceCount) |> 
    group_by(griisStatus, speciesName) |> 
    summarise(counts = sum(occurrenceCount), .groups = "drop") 
  
  griis_prop <- nrow(filter(griis, griisStatus == "Native"))/nrow(griis)*100
  
  # epbc
  epbc <- fpath |> 
    read_parquet() |> 
    select(epbcStatus, speciesName, occurrenceCount) |> 
    group_by(epbcStatus, speciesName) |> 
    summarise(counts = sum(occurrenceCount), .groups = "drop") 
  
  epbc_prop <- nrow(filter(epbc, epbcStatus == "Not listed"))/nrow(epbc)*100
  
  df <- data.frame(type = c("capad", "n_spp", "griis", "epbc"),
                   value = c(capad_prop, n_spp, griis_prop, epbc_prop),
                   year = year)
  return(df)
  
}

files_yearly <- list.files("data/by_year", full.names = TRUE, recursive = TRUE)

summaries <- files_yearly |> 
  map(get_summaries) |> 
  list_rbind() |> 
  mutate(value = round(value, 0))

# TODO: functionalise facetted summaries
# grid (there HAS to be a better way to do this???)
# x starts at 1 and progresses in positive direction
# y starts at 0 and progresses in negatative direction
# space on top two rows for name and title

# colour palettes
griis_pal <- c("#89c2d9", "#468faf", "#2a6f97", "#013a63")
spp_pal <- c('#feebe2','#fbb4b9','#f768a1','#ae017e')
capad_pal <- c('#fee391','#fec44f','#fe9929','#d95f0e')
epbc_pal <- c("#4f772d", "#90a955")

griis <- summaries |> 
  filter(type == "griis") |> 
  mutate(value = as.factor(value),
         x = c(rep(seq(from = 1, to = 72, by = 6), times = 10),
               c(1, 7, 13)),
         y = c(rep(0:-9, each = 12), c(-10, -10, -10))) 

n_spp <- summaries |> 
  filter(type == "n_spp") |> 
  mutate(binned_vals = case_when(value < 10000 ~ "a",
                                 value < 20000 ~ "b",
                                 value < 30000 ~ "c",
                                 .default = "d"), 
         x = c(rep(seq(from = 2, to = 72, by = 6), times = 10),
               c(2, 8, 14)),
         y = c(rep(0:-9, each = 12), c(-10, -10, -10))) 
        
capad <- summaries |> 
  filter(type == "capad") |> 
  mutate(binned_vals = case_when(value < 15 ~ "1",
                                 value < 20 ~ "2",
                                 value < 25 ~ "3",
                                 .default = "4"), 
         x = c(rep(seq(from = 3, to = 72, by = 6), times = 10),
               c(3, 9, 15)),
         y = c(rep(0:-9, each = 12), c(-10, -10, -10))) 

epbc <- summaries |> 
  filter(type == "epbc") |> 
  mutate(value = as.factor(value),
         x = c(rep(seq(from = 4, to = 72, by = 6), times = 10),
               c(4, 10, 16)),
         y = c(rep(0:-9, each = 12), c(-10, -10, -10))) 

ggplot() +
  geom_point(data = griis,
             aes(x, y, colour = value),
             size = 4) +
  scale_color_manual(values = griis_pal) +
  new_scale_color() +
  geom_point(data = n_spp,
             aes(x, y, colour = binned_vals), 
             size = 4) +
  scale_colour_manual(values = spp_pal) +
  new_scale_color() +
  geom_point(data = capad,
             aes(x, y, colour = binned_vals), 
             size = 4) +
  scale_colour_manual(values = capad_pal) +
  new_scale_color() +
  geom_point(data = epbc,
             aes(x, y, colour = value),
             size = 4) +
  scale_color_manual(values = epbc_pal) +
  theme_void() +
  theme(legend.position = "none") +
  coord_fixed(ratio = 2.8)

ggsave("images/title.png")

