#big roads
#small roads
#water
#green spaces
# https://github.com/ropensci/osmextract
# https://joshuamccrain.com/tutorials/maps/streets_tutorial.html
library(osmdata)

big_streets <- getbb("Melbourne") |> 
  opq() |> 
  add_osm_feature(key = "highway", 
                  value = c("motorway", 
                            "primary", 
                            "motorway_link", 
                            "primary_link")) |> 
  osmdata_sf()

library(osmextract)
library(sf)
osm_lines <- oe_get("Melbourne", 
                    layer = "lines",
                    stringsAsFactors = FALSE, 
                    quiet = TRUE)
