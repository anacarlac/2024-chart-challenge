
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
    title = "Neo(w) policies, neo(w) insights? \nLiteracy levels for different generations in Brazil",
    subtitle = "Percentage of literate Brazilian people aged over 15 years old grouped by ethnicity, age and gender",
    caption = "Visualization by Ana Crispim. Data retrieved from Brazilian Gender data CMIG - 2024",
    x = "Age groups", y = " ",
    fill = "Ethnicity"
  ) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', color = "white"),
        strip.background = element_rect(colour="white", fill="white"),
        plot.margin = margin(t = 1, r = 2, b = 1, l = 0, "cm"),
        plot.title = element_text(
          size = 14,
          face = "bold"),
        plot.subtitle = element_markdown(
          size = 8,
          face = "bold.italic"),
        plot.caption = element_text(size = 8,
                                    color = "black", 
                                    hjust = 0,
                                    vjust = -5),
        axis.text.x = element_text(size = 10,
                                   vjust = -1),
        axis.title.x = element_text(vjust = -3)
        
  )


# Save plot
ggsave("outputs/plot-day2-comparisons-neo-using-r.png", width = 6, height = 5)





