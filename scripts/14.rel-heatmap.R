
##############################################################################
##################             CHART CHALLENGE            ####################
##################                COMPARISONS             ####################
##################                 DAY 14 - HEATMAP       ####################
##############################################################################

# Load packages
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggtext)
library(geobr)
library(readr)
library(leaflet)
library(sf)

##############################################################################
##################       USE ONLY FOR SAVING RDS FILE     ####################
##############################################################################

# Save map shapes
states_shapes <- geobr::read_state(code_state = "all")
readr::write_rds(states_shapes, "data/states_shapes.rds")

# Load Excel data
dt_map <- read_excel("data-raw/2024-cmig-gender-viol-parceiro.xlsx")

# Save RDS file
saveRDS(dt_map, "data-raw/2024-cmig-gender-viol-parceiro.RDS")

# Load data
dt_map <- readRDS("data-raw/2024-cmig-gender-viol-parceiro.RDS")

# Merging dt_map with states_shapes
dt_viol_map <- states_shapes %>% 
  inner_join(dt_map, c("name_state" = "name_state"))

str(dt_viol_map$taxa)
# dt_viol_map$taxa<-as.numeric(gsub(",", ".", gsub("\\.", "", dt_viol_map$taxa)))
# str(dt_viol_map$taxa)

saveRDS(dt_viol_map, "data/2024-cmig-gender-mapa-viol-parceiro.RDS")

##############################################################################

# Load data
dt_viol_map <- readRDS("data/2024-cmig-gender-mapa-viol-parceiro.RDS")

# Build elements of the map manually

## Labels
labels <- paste0(round(dt_viol_map$taxa, 1), "% of women reported 
                 that their partners 
                 perpetrated violence against them")

bins <- seq(0,9,1.5)
pal <- leaflet::colorBin("YlOrRd", domain = dt_viol_map$taxa, bins=bins)

## Text labels inside polygons
## Get latitude and longitude using sf::st_centroid
centers <- data.frame(st_centroid(dt_viol_map))
centers$state <- row.names(dt_viol_map)

# Map of violence
dt_viol_map %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    layerId = ~name_state,
    fillColor = ~pal(taxa), #adiciona a paleta de cores criada em pal
    fillOpacity = 0.9, #ajusta a opacidade das cores
    color = "white", #muda a cor das linhas de contorno
    weight = 1.5, #ajusta a espessura da linha
    opacity = 5,
    dashArray = "3",
    highlight = highlightOptions(weight = 3, #ajusta detalhes quando passa o mouse em cima do município
                                 color = "#972534",         #"#81C5FF",
                                 dashArray = "",
                                 fillOpacity = 0.7,
                                 bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(style = list("font-weight" = "light",
                                             padding = "3px 8px"),
                                textsize = "12px",
                                direction = "auto"                              )
  ) %>% 
  addLabelOnlyMarkers(
    lng = -62.8498,
    lat = -10.90912,
    label = c("RO"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  ) %>%
  addLabelOnlyMarkers(
    lng = -70.47916, lat = -9.214614,
    label = c("AC"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  ) %>%
  addLabelOnlyMarkers(
    -64.65028, -4.154738,
    label = c("AM"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
    )%>% 
  addLabelOnlyMarkers(
    -61.3989, 2.083398,
    label = c("RR"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -53.06323, -3.968641,
    label = c("PA"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -51.95587, 1.442957,
    label = c("AP"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -48.32811, -10.1411,
    label = c("TO"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -45.27622, -5.051132,
    label = c("MA"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -42.96165, -7.383154,
    label = c("PI"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -39.61671, -5.091165,
    label = c("CE"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -36.67349, -5.839909,
    label = c("RN"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -36.83243, -7.12186,
    label = c("PB"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -37.99708, -8.329186,
    label = c("PE"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -36.62246, -9.515768,
    label = c("AL"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -37.44388, -10.58405,
    label = c("SE"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -41.72184, -12.4686,
    label = c("BA"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -44.66228, -18.44653,
    label = c("MG"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -40.66513, -19.57121,
    label = c("ES"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -42.65795, -22.19708,
    label = c("RJ"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -48.74045, -22.26423,
    label = c("SP"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -51.61921, -24.63545,
    label = c("PR"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -50.47982, -27.2493,
    label = c("SC"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -53.31893, -29.69869,
    label = c("RS"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -54.84853, -20.31997,
    label = c("MS"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -55.91867, -12.94098,
    label = c("MT"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -49.61061, -16.03509,
    label = c("GO"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -47.79732, -15.78085,
    label = c("DF"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    lng = -62.8498,
    lat = -10.90912,
    label = c("RO"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  ) %>%
  addLabelOnlyMarkers(
    lng = -70.47916, lat = -9.214614,
    label = c("AC"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  ) %>%
  addLabelOnlyMarkers(
    -64.65028, -4.154738,
    label = c("AM"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -61.3989, 2.083398,
    label = c("RR"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -53.06323, -3.968641,
    label = c("PA"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -51.95587, 1.442957,
    label = c("AP"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -48.32811, -10.1411,
    label = c("TO"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -45.27622, -5.051132,
    label = c("MA"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -42.96165, -7.383154,
    label = c("PI"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -39.61671, -5.091165,
    label = c("CE"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -36.67349, -5.839909,
    label = c("RN"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -36.83243, -7.12186,
    label = c("PB"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -37.99708, -8.329186,
    label = c("PE"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -36.62246, -9.515768,
    label = c("AL"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -37.44388, -10.58405,
    label = c("SE"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -41.72184, -12.4686,
    label = c("BA"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -44.66228, -18.44653,
    label = c("MG"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -40.66513, -19.57121,
    label = c("ES"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -42.65795, -22.19708,
    label = c("RJ"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -48.74045, -22.26423,
    label = c("SP"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -51.61921, -24.63545,
    label = c("PR"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -50.47982, -27.2493,
    label = c("SC"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -53.31893, -29.69869,
    label = c("RS"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -54.84853, -20.31997,
    label = c("MS"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -55.91867, -12.94098,
    label = c("MT"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -49.61061, -16.03509,
    label = c("GO"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLabelOnlyMarkers(
    -47.79732, -15.78085,
    label = c("DF"),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
  )%>% 
  addLegend("bottomright",  #adiciona legenda no mapa 
            pal = pal, 
            values = ~taxa,
            title = "Percentage of violence<br/>
            against women perpetrated<br/> 
            by their partners",
            opacity=0.7)
