
##############################################################################
##################             CHART CHALLENGE            ####################
##################                COMPARISONS             ####################
##################            DAY 1 - PART TO WHOLE       ####################
##############################################################################

# Note: the donut graph function was adapted from: https://r-graph-gallery.com/128-ring-or-donut-plot.html
# Load packages
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggtext)

# Load data
data_literacy <- readRDS("data/2024-cmig-gender-literacy.RDS")

# Fix variable type from character to numeric
data_literacy$literacy  <- as.numeric(data_literacy$literacy)

# Create another row for the "Not literate" option because rows have observations about percentage of literate people only
literate_no <- c("country", "2022", "Brasil", "total", "total", "overall", "5.6")

# Create a data.frame that contains overall data about literacy
data_literacy_total <- data_literacy 

# Merge data.frame with new row of observations for "Not literate"
data_literacy_total <- rbind(data_literacy_total, literate_no)

# Adjust data.frame to contain values of percentages in decimals and add cumulative percentage column

data_literacy_total <- data_literacy_total %>% 
  filter(gender == "total") %>% 
  mutate(
    literate = c("Literate", "Not literate"),
    literacy_num = literacy/100
  ) %>% 
  mutate(
    ymax = cumsum(literacy_num),
    ymin = c(0, head(data_literacy_total$ymax, n=-1))
  )

# Compute label position
data_literacy_total$labelpos <- (data_literacy_total$ymax + data_literacy_total$ymin) / 2

# Compute a good label for the categories in the donut chart
data_literacy_total$label <- paste0(data_literacy_total$literate, "\n", 
                                    data_literacy_total$literacy, "%")

# Build donut plot
data_literacy_total %>% 
  
  #gg geom
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=literate)) +
  geom_rect() +
  coord_polar(theta="y") +  
  
  # Labels and title
  geom_text(x=4.4, aes(y=labelpos, 
                       label=label, 
                       color=literate), 
            size = 5) +  # x here controls label position (inner / outer)
  labs(
    title = "What is the percentage of literate people in Brazil?",
    subtitle = "Percentage of literate Brazilian people aged over 15 years old",
    caption = "Visualization by Ana Crispim. Data retrieved from Brazilian Gender data CMIG - 2024") +
  
  # Axis and title info
  xlim(c(1, 4)) + 
  
  # Theme and colors
  scale_fill_manual(values = c("#82C0CC", "#F7F4F3"))+
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(
          size = 20,
          face = "bold"),
        plot.subtitle = element_markdown(
          size = 15,
          face = "bold.italic"),
        plot.caption = element_text(size = 12,
                                    color = "black", 
                                    hjust = -0.5)
  ) +
  scale_colour_manual(values=c("#000000", "#000000")) 

# Save plot
ggsave("outputs/plot-day1-comparisons-partoftowhole-using-r.png", width = 8, height = 8)



