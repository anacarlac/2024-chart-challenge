
##############################################################################
##################             CHART CHALLENGE            ####################
##################                COMPARISONS             ####################
##################            DAY 5 - DIVERGING           ####################
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
data_college <- read_excel("data-raw/2024-cmig-gender-school-graduateschool.xlsx")

# Save RDS file
saveRDS(data_college, "data/2024-cmig-gender-graduateschool.RDS")

##############################################################################

# Load data
data_college <- readRDS("data/2024-cmig-gender-graduateschool.RDS")

# Build chart for enrolled students
data_college %>%
  dplyr::mutate(
    gender = dplyr::case_when(
      gender == "men" ~ "Men",
      gender == "women" ~ "Women"
    )
  ) %>% 
  dplyr::mutate(
    college = fct_reorder(college, 
                       desc(college))) %>% 
  dplyr::filter(college != "Core Programs") %>%
  ggplot(aes(x=college, y=enrolled)) +
  geom_segment(aes(x=college, xend=college, y=0, yend=enrolled), 
               color="lightgrey",
               size = 2,
               alpha = 0.5
               ) +
  geom_point(aes(
             color=gender), 
             alpha=0.7, 
             size=3) +
  geom_text(aes(label = paste0(round(enrolled,0), "%")), 
            position = position_dodge2(width = 0.9), 
            size = 3,
            #vjust=-2, 
            hjust=-0.4
            ) +
  scale_colour_manual(values = c("#82C0CC", "#F0803C")) +
  coord_flip() +
  facet_wrap(.~ gender)+
  xlab("") +
  ylab("") +
  labs(
    title = "Are career interests diverging? \nEnrollment rates for college degrees",
    subtitle = "Percentage of Brazilian men and women enrolled in on-campus college courses",
    caption = "Visualization by Ana Crispim. Data retrieved from Brazilian Gender data CMIG - 2024",
    x = " ", y = " ",
    color = "Gender"
  ) +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 
                                  by = 20)) +
  theme(
    legend.position = "none",
    # legend.title = element_text(size = 10),
    # legend.text = element_text(size = 10),
    # legend.box.spacing = margin(1),
    # legend.key.width = unit(0.5, 'cm'),
    # legend.key.height = unit(0.5, 'cm'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), # #E7E3E3
    plot.background = element_rect(fill = 'white'), #change plot background color
    panel.background = element_rect(fill = 'white'), # change the panel background color (around title)
    strip.background = element_rect(fill='white'), # change strips background color
    #legend.background = element_rect(fill = 'white'), #change legend box background color
    plot.margin = margin(t = 1, r = 2, b = 0.5, l = 0, "cm"),
    plot.title = element_text(
      size = 18,
      face = "bold"),
    plot.subtitle = element_markdown(
      size = 10,
      face = "bold.italic",
      margin=margin(0,0,10,0)
    ),
    plot.caption = element_text(size = 8,
                                color = "black", 
                                hjust = 0,
                                vjust = 0.5),
    axis.text.x = element_text(size = 7,
                               vjust = -1),
    axis.title.x = element_text(vjust = -4),
    axis.text.y = element_text(size = 8)
    
  )

# Save plot
ggsave("outputs/plot-day5-comparisons-diverging-enrolled-using-r.png", width = 8, height = 9)

# Build chart for graduated students
data_college %>%
  dplyr::mutate(
    gender = dplyr::case_when(
      gender == "men" ~ "Men",
      gender == "women" ~ "Women"
    )
  ) %>% 
  dplyr::mutate(
    college = fct_reorder(college, 
                          desc(college))) %>% 
  dplyr::filter(college != "Core Programs") %>%
  ggplot(aes(x=college, y=enrolled)) +
  geom_segment(aes(x=college, xend=college, y=0, yend=graduate), 
               color="lightgrey",
               size = 2,
               alpha = 0.5
  ) +
  geom_point(aes(
    color=gender), 
    alpha=0.7, 
    size=3) +
  geom_text(aes(label = paste0(round(graduate,0), "%")), 
            position = position_dodge2(width = 0.9), 
            size = 3,
            #vjust=-2, 
            hjust=-0.4
  ) +
  scale_colour_manual(values = c("#82C0CC", "#F0803C")) +
  coord_flip() +
  facet_wrap(.~ gender)+
  xlab("") +
  ylab("") +
  labs(
    title = "Are career interests diverging? \nGraduation rates for college degrees",
    subtitle = "Percentage of Brazilian men and women graduated in on-campus college courses",
    caption = "Visualization by Ana Crispim. Data retrieved from Brazilian Gender data CMIG - 2024",
    x = " ", y = " ",
    color = "Gender"
  ) +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 
                                  by = 20)) +
  theme(
    legend.position = "none",
    # legend.title = element_text(size = 10),
    # legend.text = element_text(size = 10),
    # legend.box.spacing = margin(1),
    # legend.key.width = unit(0.5, 'cm'),
    # legend.key.height = unit(0.5, 'cm'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), # #E7E3E3
    plot.background = element_rect(fill = 'white'), #change plot background color
    panel.background = element_rect(fill = 'white'), # change the panel background color (around title)
    strip.background = element_rect(fill='white'), # change strips background color
    #legend.background = element_rect(fill = 'white'), #change legend box background color
    plot.margin = margin(t = 1, r = 2, b = 0.5, l = 0, "cm"),
    plot.title = element_text(
      size = 18,
      face = "bold"),
    plot.subtitle = element_markdown(
      size = 10,
      face = "bold.italic",
      margin=margin(0,0,10,0)
    ),
    plot.caption = element_text(size = 8,
                                color = "black", 
                                hjust = 0,
                                vjust = 0.5),
    axis.text.x = element_text(size = 7,
                               vjust = -1),
    axis.title.x = element_text(vjust = -4),
    axis.text.y = element_text(size = 8)
    
  )

# Save plot
ggsave("outputs/plot-day5-comparisons-diverging-graduate-using-r.png", width = 8, height = 9)
