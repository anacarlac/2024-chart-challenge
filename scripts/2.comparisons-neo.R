
##############################################################################
##################             CHART CHALLENGE            ####################
##################                COMPARISONS             ####################
##################                 DAY 2 - NEO            ####################
##############################################################################

# Load packages
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggtext)

# Load data
data_literacy <- readRDS("data/2024-cmig-gender-literacy.RDS")

# Fix variable type from character to numeric
data_literacy$literacy  <- as.numeric(data_literacy$literacy)

# Build grouped bar chart
data_literacy %>% 
  dplyr::filter(age != "overall") %>% 
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
  dplyr::mutate(
    age = dplyr::case_when(
      age == "15 - 24" ~ "15 - 24 \nyears old",
      age == "25 - 64" ~ "25 - 64 \nyears old",
      age == "65 or more" ~ "over 65 \nyears old"
    )     
  ) %>% 
  dplyr::mutate(
    ages=fct_relevel(age,c("15 - 24 \nyears old",
                           "25 - 64 \nyears old",
                           "over 65 \nyears old")
                     )
    ) %>% 
  ggplot(aes(y=literacy, x=age, fill=ethnicity)) + 
  geom_bar(position="dodge", 
           stat="identity"
           )+
  geom_text(aes(label = paste0(literacy, "%")), 
            position = position_dodge2(width = 0.9, preserve = "single"), 
            size = 2.5,
            vjust=-1, 
            hjust=0.5) +
  scale_fill_manual(values = c("#82C0CC", "#3D5A80")) +
  facet_grid(~gender) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 110, by = 20)) +
  labs(
    title = "New policies, new insights? \nLiteracy levels for different generations in Brazil",
    subtitle = "Percentage of literate Brazilian people aged over 15 years old grouped by ethnicity, age and gender",
    caption = "Visualization by Ana Crispim. Data retrieved from Brazilian Gender data CMIG - 2024",
    x = " ", y = " ",
    fill = "Ethnicity"
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
        strip.background = element_rect(colour="white", fill="white"),
        plot.margin = margin(t = 1, r = 2, b = 0.5, l = 0, "cm"),
        plot.title = element_text(
          size = 14,
          face = "bold"),
        plot.subtitle = element_markdown(
          size = 8,
          face = "bold.italic"),
        plot.caption = element_text(size = 8,
                                    color = "black", 
                                    hjust = 0,
                                    vjust = 0.5),
        axis.text.x = element_text(size = 10,
                                   vjust = -1),
        axis.title.x = element_text(vjust = -4)
        
  )


# Save plot
ggsave("outputs/plot-day2-comparisons-neo-using-r.png", width = 6, height = 5)





