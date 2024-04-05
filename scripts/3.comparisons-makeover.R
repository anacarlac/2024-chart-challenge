
##############################################################################
##################             CHART CHALLENGE            ####################
##################                COMPARISONS             ####################
##################            DAY 3 - MAKEOVER            ####################
##############################################################################

# Load packages
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggtext)

##############################################################################
##################       USE ONLY FOR SAVING RDS FILE     ####################
##############################################################################

# Load Excel data
#data_frq <- read_excel("data-raw/2024-cmig-gender-school-frq-15-17.xlsx")

# Save RDS file
#saveRDS(data_frq, "data/2024-cmig-gender-frq1517.RDS")

##############################################################################

# Load data
data_frq <- readRDS("data/2024-cmig-gender-frq1517.RDS")

# Build grouped bar chart
graph_frq <- data_frq %>% 
  dplyr::filter(age != "overall") %>% 
  dplyr::filter(ethnicity != "total") %>% 
  dplyr::filter(ethnicity != "overall") %>% 
  dplyr::mutate(
    gender = dplyr::case_when(
      gender == "men" ~ "Men",
      gender == "women" ~ "Women"
    )
  ) %>% 
  dplyr::mutate(
    ethnicity = dplyr::case_when(
      ethnicity == "black or brown" ~ "Black or Brown",
      ethnicity == "white" ~ "White"
    )     
  ) %>% 
  ggplot(aes(y=frq_perc, x=year, color=ethnicity)) + 
  geom_line()+
  geom_point() +
  scale_color_manual(values = c("#82C0CC", "#3D5A80")) +
  facet_grid(~gender) +
  scale_x_continuous(limits = c(2012, 2022), breaks = seq(2012, 2022, by = 1)) +
  scale_y_continuous(limits = c(70, 100), breaks = seq(70, 100, by = 10)) +
  labs(
    title = "Did everybody find their sit in class?",
    subtitle = "Raw school attendance rate of Brazilian students aged between 15 and 17 years old grouped by ethnicity and gender by year",
    caption = "Visualization by Ana Crispim. Data retrieved from Brazilian Gender data CMIG - 2024",
    x = " ", y = " ",
    color = "Ethnicity"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.box.spacing = margin(1),
    legend.key.width = unit(0.5, 'cm'),
    legend.key.height = unit(0.5, 'cm'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = 'white', color = "white"),
    strip.background = element_rect(colour="white", 
                                    fill="white",),
    plot.margin = margin(t = 1, r = 2, b = 0.5, l = 0, "cm"),
    plot.title = element_text(
      size = 14,
      face = "bold"),
    plot.subtitle = element_markdown(
      size = 8,
      face = "bold.italic",
      margin=margin(0,0,15,0)
      ),
    plot.caption = element_text(size = 8,
                                color = "black", 
                                hjust = 0,
                                vjust = 0.5),
    axis.text.x = element_text(size = 8,
                               vjust = -1),
    axis.title.x = element_text(vjust = -4)
    
  )

# Add ggrepel geom to position values around geom_line and geom_point
graph_frq + geom_text_repel(aes(label = paste0(round(frq_perc,1), "%")), 
                            size = 2.5,
                            box.padding = 0.3, 
                            max.overlaps = Inf,
                            force=1)

# Save plot
ggsave("outputs/plot-day3-comparisons-makeover-using-r.png", width = 9, height = 5)
       