
##############################################################################
##################             CHART CHALLENGE            ####################
##################                COMPARISONS             ####################
##################            DAY 9 - MAJOR/MINOR         ####################
##############################################################################

# Load packages
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggtext)
library(prismatic)

##############################################################################
##################       USE ONLY FOR SAVING RDS FILE     ####################
##############################################################################

# Chart template and inspiration: https://r-graph-gallery.com/web-dumbell-chart.html

# Load Excel data
data_viol <- read_excel("data-raw/2024-cmig-gender-violence-at-home.xlsx")

# Save RDS file
saveRDS(data_viol, "data/2024-cmig-gender-viol-home.RDS")

################################### DATA #####################################

# Load data
data_viol <- readRDS("data/2024-cmig-gender-viol-home.RDS")

################################## PALETTE ##################################
pal_base <- c("#5BBCD4", "#CA3C25")
pal_dark <- clr_darken(pal_base, .25)

grey_base <- "grey50"
grey_dark <- "grey25"

################################### THEME ###################################
theme_set(theme_minimal(base_family = "sans", base_size = 22))

theme_update(
  axis.title = element_blank(),
  axis.text.y = element_text(hjust = 0, 
                             color = grey_dark),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.title = element_textbox_simple(
    size = rel(1.25), face = "plain", lineheight = 1.05, 
    fill = "transparent", width = unit(8, "inches"), 
    box.color = "transparent", 
    margin = margin(t=0, r=0, b=10, l=-100), 
    hjust = 0, halign = 0
  ),
  plot.caption = element_markdown(
    size = 7, color = "black", 
    hjust = 0, 
    margin = margin(t = 25, b = 0),
    family = 'sans'
  ),
  plot.subtitle = element_markdown(
    size = 12,
    face = "bold.italic",
    margin=margin(0,0,10,l=-100)
  ),
  plot.caption.position = "plot",
  plot.margin = margin(25, 25, 15, 25),
  plot.background = element_rect(fill = "white", color = "white"),
  legend.position = "none"
)

################################# ANNOTATIONS #################################
title <- paste0(
  "Physical violence at home"
)

subtitle <- paste0(
  "Percentage of <b style='color:",
  pal_dark[1], ";'>MEN</b> and <b style='color:", pal_dark[2], ";'>WOMEN</b> that reported to suffer physical<br> violence at home in the last 12 months in Brazil"
)

caption <- paste0(
  "Visualization by Ana Crispim. Data retrieved from Brazilian Gender data CMIG - 2024"
)


########################## VIOLENCE CHART - REGIONS ##########################

data_viol %>% 
  filter(level == "region") %>% 
  mutate(is_smaller = if_else(row_number() == 1, 0, 1), .by = region) %>% 
  ggplot(
       aes(x = perc, y = region)) +
  ## dumbbell segments
  stat_summary(
    geom = "linerange", 
    fun.min = "min", 
    fun.max = "max",
    linewidth = 1,
    color = grey_base
  ) +
  ## dumbbell points
  ## white point to overplot line endings
  geom_point(
    aes(x = perc), size = 6, shape = 21, stroke = 1, color = "white", fill = "white"
  ) +
  ## semi-transparent point fill
  geom_point(
    aes(x = perc, fill = gender), size = 6, shape = 21, stroke = 1, color = "white", alpha = .7
  ) +
  ## point outline
  geom_point(
    aes(x = perc), size = 6, shape = 21, stroke = 1, color = "white", fill = NA
  ) +
  geom_text(
    aes(label = paste0(round(perc,0), "%"),
        x = ifelse(is_smaller == 0, perc - 8, perc + 8),
        color = gender),
    fontface = "bold",
    family = "sans", size = 4
  ) +
  scale_x_continuous(limits = c(0, 100),
                     breaks = seq(0, 100,
                                  by = 20),
                     ) +
  scale_color_manual(values = pal_dark) +
  scale_fill_manual(values = pal_base) +
  labs(title = title, 
       subtitle = subtitle,
       caption = caption
       ) +
  theme(axis.text.y = element_text(
                                   size=14),
        axis.text.x = element_text(face = "bold",
                                   size = 12)
        
        
        )

# Save plot
ggsave("outputs/plot-day9-dist-majorminor-violenceathome-regions-using-r.png", 
       width = 6,
       height = 5.5)

########################## VIOLENCE CHART - STATES ##########################

data_viol %>% 
  filter(level == "state") %>% 
  dplyr::mutate(
    region = fct_reorder(region, 
                          desc(region))) %>% 
  mutate(is_smaller = if_else(row_number() == 1, 0, 1), .by = region) %>% 
  ggplot(
    aes(x = perc, y = region)) +
  ## dumbbell segments
  stat_summary(
    geom = "linerange", 
    fun.min = "min", 
    fun.max = "max",
    linewidth = 1,
    color = grey_base
  ) +
  ## dumbbell points
  ## white point to overplot line endings
  geom_point(
    aes(x = perc), size = 4, shape = 21, stroke = 1, color = "white", fill = "white"
  ) +
  ## semi-transparent point fill
  geom_point(
    aes(x = perc, fill = gender), size = 4, shape = 21, stroke = 1, color = "white", alpha = .7
  ) +
  ## point outline
  geom_point(
    aes(x = perc), size = 4, shape = 21, stroke = 1, color = "white", fill = NA
  ) +
  geom_text(
    aes(label = paste0(round(perc,0), "%"),
        x = ifelse(is_smaller == 0, perc - 7, perc + 7),
        color = gender),
    fontface = "bold",
    family = "sans", size = 2
  ) +
  scale_x_continuous(limits = c(0, 100),
                     breaks = seq(0, 100,
                                  by = 20),
  ) +
  scale_color_manual(values = pal_dark) +
  scale_fill_manual(values = pal_base) +
  labs(title = title, 
       subtitle = subtitle,
       caption = caption
  ) +
  theme(
    axis.text.y = element_text(size=9),
    axis.text.x = element_text(face = "bold", 
                               size = 8),
    plot.title = element_textbox_simple(
      size = rel(1), face = "plain", lineheight = 1.05, 
      fill = "transparent", width = unit(8, "inches"), 
      box.color = "transparent", 
      margin = margin(t=0, r=0, b=5, l=-90), 
      hjust = 0, halign = 0
    ),
    plot.caption = element_markdown(
      size = 7, color = "black", 
      hjust = 0, 
      margin = margin(t = 15, b = 0),
      family = 'sans'
    ),
    plot.subtitle = element_markdown(
      size = 10,
      face = "bold.italic",
      margin=margin(t=0,r=0,b=10,l=-90)
    ),
    #plot.caption.position = "plot",
    plot.margin = margin(10, 25, 15, 25),
    panel.grid.major.y = element_line(
      size = 0.40, 
      linetype = 'solid',
      colour = "lightgrey"
    ), # Horizontal major grid lines
  )


# Save plot
ggsave("outputs/plot-day9-dist-majorminor-violenceathome-states-using-r.png", 
       width = 5,
       height = 5.5)
