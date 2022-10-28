## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# remotes::install_github("ovirahman/bangladesh")
library(bangladesh)

country <- get_map("country")
division <- get_map("division")
district <- get_map("district")
upazila <- get_map("upazila")
union <- get_map("union")

## ----plot , warning=FALSE, message=FALSE,fig.width = 5, fig.height = 5, fig.align = "center"----
bd_plot("country")
bd_plot("division")
bd_plot("district")

## ----interactive,include = FALSE, warning=FALSE, message=FALSE,fig.width = 5, fig.height = 5, fig.align = "center"----
bd_plot(level = "district", type = "interactive")

## ----choropleth_static, warning=FALSE, message=FALSE,fig.width = 5, fig.height = 5, fig.align = "center"----
library(tmap)
population <- bangladesh::pop_district_2011[, c("district", "population")]
district <- get_map("district")

map_data <- dplyr::left_join(district, population, by = c("District" = "district"))

map <- tm_shape(map_data) + 
  tm_polygons("population",id = "District",palette = "Reds", title = "Population") +
  tm_style("cobalt")+
  tm_layout(
    "Bangladesh District Wise Population Map\nSource: BBS",
    title.position = c("left", "bottom"),
    legend.position = c("right", "top")
    )

tmap::tmap_mode("plot")
map

## ----choropleth_interactive, include = FALSE, warning=FALSE, message=FALSE,fig.width = 5, fig.height = 5, fig.align = "center"----
tmap::tmap_mode("view")
map

## ----ggplot, warning=FALSE, message=FALSE,fig.width = 5, fig.height = 5, fig.align = "center"----
library(ggplot2)
ggplot(data = map_data) +
  geom_sf(aes(fill = population))+
  theme_void()+
  viridis::scale_fill_viridis(trans = "log", name="Population", labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  labs(
    title = "Bangladesh Population Map",
    subtitle = "Population & Housing Census 2011",
    caption = "Data Source: BBS"
  )
  

## ----centroids, warning=FALSE, message=FALSE,fig.width = 5, fig.height = 5, fig.align = "center"----
division_map <- get_map("division")
division_centroids <- bangladesh::get_coordinates(level = "division")
knitr::kable(division_centroids, format = "html")
ggplot(data = division_map) +
  geom_sf() +
  geom_sf_label(aes(label = Division)) +
  geom_point(data = division_centroids, x = division_centroids$lon, y = division_centroids$lat, col = "red", size = 3) +
  xlab("")+ ylab("")+
  theme_minimal()



## ----partial_map, warning=FALSE, message=FALSE,fig.width = 5, fig.height = 5, fig.align = "center"----

sylhet <- get_divisions(divisions = "Sylhet",level =  "upazila")
# single division
ggplot(data = sylhet) +
  geom_sf() +
  xlab("")+ ylab("")+
  theme_minimal()

#multiple division
sylhet_chittagong_dhaka <- get_divisions(divisions = c("Sylhet", "Chittagong", "Dhaka"),level =  "upazila")
ggplot(data = sylhet_chittagong_dhaka) +
  geom_sf() +
  xlab("")+ ylab("")+
  theme_minimal()



## ----search, warning=FALSE, message=FALSE,fig.width = 5, fig.height = 5, fig.align = "center"----
amtali <- bd_search("amtali", level = "union", as.is = TRUE, coordinates = TRUE)
knitr::kable(amtali, format = "html")
ggplot(bangladesh::map_union) +
  geom_sf() +
  geom_point(data = amtali, x = amtali$lon, y = amtali$lat, col = "red", size = 3) 



