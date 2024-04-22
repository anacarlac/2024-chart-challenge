
##############################################################################
##################             CHART CHALLENGE            ####################
##################                COMPARISONS             ####################
##################            DAY 11 - MOBILE FRIENDLY    ####################
##############################################################################

# Load packages
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggtext)
library(ggrepel)
library(ggshadow)

##############################################################################
##################       USE ONLY FOR SAVING RDS FILE     ####################
##############################################################################

# Load Excel data
data_mobile <- read_excel("data-raw/2024-cmig-gender-mobile-use.xlsx")

# Save RDS file
saveRDS(data_mobile, "data/2024-cmig-mobile-use.RDS")

##############################################################################

# Load data
data_mobile <- readRDS("data/2024-cmig-mobile-use.RDS")

theme_set(theme_minimal(base_family = "sans", base_size = 22))

################################## PALETTE ##################################
palette <- c("#05838C", "#E7226A")

################################# ANNOTATIONS #################################
title <- paste0(
  "Is the world mobile-friendly to the youth?"
)

subtitle <- paste0(
  "Percentage of <b style='color:",
  palette[1], ";'>BOYS</b> and <b style='color:", palette[2], ";'>GIRLS</b> aged 10 to 14 years old that have a mobile in Brazil"
)

caption <- paste0(
  "Visualization by Ana Crispim. Data retrieved from Brazilian Gender data CMIG - 2024"
)

# Line chart
data_mobile %>% 
   #mutate(is_smaller = if_else(row_number() == 1, 0, 1), .by = year) %>% 
   ggplot(aes(y=frq_perc, x=year)) + 
      geom_line(size = 1, 
                alpha = 0.8,
                aes(color=(gender)),
                #color = "gray"
      ) +
     geom_point(color='#FFFDF9', 
                stroke = 1.5,
                shape=21, 
                size=5, 
                alpha = 0.95,
                aes(fill=factor(gender))) + 
     scale_fill_manual(values=c("#05838C", "#E7226A")) +
     scale_color_manual(values = c("lightgrey", "lightgrey")) +
     scale_x_continuous(limits = c(2016, 2022), breaks = seq(2016, 2022, by = 1)) +
     scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 20)) +
     geom_text(
       aes(label = paste0(round(frq_perc, 0), "%"), 
           vjust = ifelse(gender == 'men', 2.5, -2)),
       color = "#3A3B3B",
       fontface = "bold",
       family = "sans", size = 3.5
     ) +
     labs(
       title = title,
       subtitle = subtitle,
       caption = caption,
       x = " ", y = " ",
       color = " "
     ) +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(
        size = 0.60, 
        linetype = 'solid',
        colour = "#EBDBDC"
      ), # Horizontal major grid lines
      panel.background = element_rect(fill = '#FFFDF9', color = "#FFFDF9"),
      plot.margin = margin(t = 1, r = 2, b = 0.5, l = 0, "cm"),
      plot.title = element_text(
        size = 18,
        margin = margin(t=0, r=0, b=10, l=-25), 
        face = "bold"),
      plot.subtitle = element_markdown(
        size = 9,
        face = "bold.italic",
        margin=margin(0,0,10,-25)),
      plot.caption = element_text(size = 8,
                                  color = "black",
                                  hjust = 0,
                                  margin = margin(t=0, r=0, b=10, l=-20), 
                                  vjust = 0.5),
      axis.text.x = element_text(size = 12,
                                 face = "bold",
                                 vjust = -1),
      axis.text.y = element_text(size = 12),
      plot.background = element_rect(fill = "#FFFDF9", color = "#FFFDF9")
    )
    

# Save plot
ggsave("outputs/plot-day11-dist-mobilefriendly-using-r.png", width = 6, height = 5)
       



