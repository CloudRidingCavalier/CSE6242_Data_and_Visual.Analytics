library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)


fl <- subset(states, region == "florida")
counties <- map_data("county")
fl_county <- subset(counties, region == "florida")


fl_map <- ggplot(data = fl, mapping = aes(x = long, y = lat, group = group)) + coord_fixed(1.3) + geom_polygon(color = "black", fill = "gray")
fl_map + geom_polygon(data = fl_county, fill = NA, color = "white") + geom_polygon(color = "black", fill = NA)

fl_population <- read.csv("fl_county.csv")
fl_population$population <- as.numeric(fl_population$population)

fl_county_population <- inner_join(fl_county, fl_population, by = "subregion")

county_max_population <- unique(fl_county_population[fl_county_population$population == max(fl_county_population$population), ]$subregion)
county_max_population


fl_county_population_map <- fl_map + 
      geom_polygon(data = fl_county_population, aes(fill = population), color = "white") +
      geom_polygon(color = "black", fill = NA) +
      theme_bw()

fl_county_population_map

ggsave("fl_county_population_map.png", plot = fl_county_population_map, device = "png")
