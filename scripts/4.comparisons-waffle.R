
##############################################################################
##################             CHART CHALLENGE            ####################
##################                COMPARISONS             ####################
##################            DAY 4 - WAFFLE              ####################
##############################################################################

# Load packages
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggtext)
library(waffle)
library(fontawesome)
install_fa_fonts()

##############################################################################
##################       USE ONLY FOR SAVING RDS FILE     ####################
##############################################################################

# Load Excel data
# data_hs_complete <- read_excel("data-raw/2024-cmig-gender-school-highschool-rate.xlsx")
# 
# # Save RDS file
# saveRDS(data_hs_complete, "data/2024-cmig-gender-hs_complete.RDS")

##############################################################################

# Load data
data_hs_complete <- readRDS("data/2024-cmig-gender-hs_complete.RDS")

# Round numbers
data_hs_complete$frq_perc <- round(data_hs_complete$frq_perc, 0)

# Build waffle chart
data_hs_complete %>% 
  filter(gender == "total") %>% 
  dplyr::mutate(
    high_school = dplyr::case_when(
      high_school == "finished" ~ "Yes",
      high_school == "not finished" ~ "No"
    )     
  ) %>% 
  dplyr::mutate(
    ethnicity = dplyr::case_when(
      ethnicity == "total" ~ "Overall",
      ethnicity == "black or brown" ~ "Black or Brown",
      ethnicity == "white" ~ "White"
    )     
  ) %>% 
  dplyr::mutate(
    ethnicity=fct_relevel(ethnicity,
                          c("Overall",
                           "Black or Brown",
                           "White")
    )
  ) %>% 
  ggplot(aes(fill = high_school, 
             values = frq_perc,
             n_rows = 10
             )) + 
  geom_waffle() + 
  scale_fill_manual(values = c("#A93F55", "#82C0CC"),
                    guide = guide_legend(override.aes = list(fill = c("#A93F55", "#82C0CC")))) +
  facet_wrap(~ethnicity) + 
  labs(
    title = "Who is completing High School? \nExamining High School graduation numbers in Brazil",
    subtitle = "The percentage of Brazilians aged between 20 and 22 who have completed High School, categorized by overall statistics and ethnicity groups",
    caption = "Visualization by Ana Crispim. Data retrieved from Brazilian Gender data CMIG - 2024",
    x = " ", y = " ",
    fill = "Completed High School?"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.box.spacing = margin(1),
    #legend.key = element_rect(fill = 'transparent'),
    #legend.key = element_rect(fill = '#A93F55', color = '#A93F55'),
    legend.key.width = unit(0.5, 'cm'),
    legend.key.height = unit(0.5, 'cm'),
    #legend.box.background = element_rect(fill = '#A93F55'), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = '#E7E2DF'), #change plot background color
    panel.background = element_rect(fill = '#E7E2DF'), # change the panel background color (around title)
    strip.background = element_rect(fill='#E7E2DF'), # change strips background color
    legend.background = element_rect(fill = '#E7E2DF'), #change legend box background color
    plot.margin = margin(t = 1, r = 2, b = 0.5, l = 0, "cm"),
    plot.title = element_text(
      size = 19,
      face = "bold"),
    plot.subtitle = element_markdown(
      size = 9,
      face = "bold.italic",
      margin=margin(0,0,10,0)
    ),
    plot.caption = element_text(size = 10,
                                color = "black", 
                                hjust = 0,
                                vjust = 0.5),
    axis.text.x = element_text(size = 8,
                               vjust = -1),
    axis.title.x = element_text(vjust = -4)
    
  )

# Save plot
ggsave("outputs/plot-day4-comparisons-waffle-using-r.png", width = 8, height = 5)

