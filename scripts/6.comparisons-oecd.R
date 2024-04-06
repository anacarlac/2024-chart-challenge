
##############################################################################
##################             CHART CHALLENGE            ####################
##################                COMPARISONS             ####################
##################            DAY 6 - OECD                ####################
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
data_govspend <- read_excel("data-raw/2024-oecd-educ-public-spending-total-expend-gov.xlsx")

# Save RDS file
saveRDS(data_govspend, "data/2024-oecd-gov-spending.RDS")

##############################################################################

# Load data
data_govspend <- readRDS("data/2024-oecd-gov-spending.RDS")

# Build grouped bar chart
graph_govspend <- data_govspend %>% 
  dplyr::filter(year != "2008") %>% 
  dplyr::filter(year != "2009") %>% 
  dplyr::filter(year != "2010") %>% 
  ggplot(aes(y=gov_spending, x=year, color=coutries)) + 
  geom_line(size = 1, alpha = 0.8) +
  # geom_line(size = 3, alpha = 0.05) +
  # geom_line(size = 2, alpha = 0.1) +
  # geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.8) +
  # geom_point(size = 3, alpha = 0.05) +
  # geom_point(size = 2, alpha = 0.1) +
  # geom_point(size = 1, alpha = 0.8) +
  scale_color_manual(values = c("#F0803C", "#82C0CC")) + #F08800 #3D5A80
  facet_grid(~edu_level) +
  scale_x_continuous(limits = c(2011, 2020.5), breaks = seq(2011, 2020.5, by = 1)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +
  labs(
    title = "How much are we investing in education?",
    subtitle = "Total public expenditure on education as a percentage of total government expenditure in Brazil and OECD countries",
    caption = "Visualization by Ana Crispim. \nSource: OECD (2024), Education at a glance: Educational finance indicators, OECD Education Statistics (database), https://doi.org/10.1787/c4e1b551-en (accessed on 06 April 2024).",
    x = " ", y = " ",
    color = " "
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
      margin=margin(0,0,10,0)
      ),
    plot.caption = element_text(size = 6,
                                color = "black", 
                                hjust = 0,
                                vjust = 0.5),
    strip.text = element_text(
      size = 9,
      face = "bold"
      ),
    axis.text.x = element_text(size = 8,
                               vjust = -1),
    axis.title.x = element_text(vjust = -4)
    
  )


# Add ggrepel geom to position values around geom_line and geom_point
graph_govspend + geom_text_repel(aes(label = paste0(round(gov_spending,1), "%")), 
                            size = 2.5,
                            box.padding = 0.4, 
                            max.overlaps = Inf,
                            force=1)

# Save plot
ggsave("outputs/plot-day6-comparisons-oecd-using-r.png", width = 9, height = 5)
       


# Test 
# Build grouped bar chart
data_govspend %>% 
  dplyr::filter(year != "2008") %>% 
  dplyr::filter(year != "2009") %>% 
  dplyr::filter(year != "2010") %>% 
  ggplot(aes(y=gov_spending, x=year, color=coutries)) + 
  geom_line() +
  geom_point() +
  geom_line(size = 1, alpha = 0) +
  geom_line(size = 3, alpha = 0.1) +
  geom_line(size = 2, alpha = 0.2) +
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 1, alpha = 0) +
  geom_point(size = 3, alpha = 0.1) +
  geom_point(size = 2, alpha = 0.2) +
  geom_point(size = 1, alpha = 0.5) +
  scale_color_manual(values = c("#1EECFF", "#FF9E49")) +
  facet_grid(~edu_level) +
  scale_x_continuous(limits = c(2011, 2020), breaks = seq(2011, 2020, by = 1)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 10)) +
  labs(
    title = "How much are we investing in education?",
    subtitle = "Total public expenditure on education as a percentage of total government expenditure in Brazil and OECD countries",
    caption = "Visualization by Ana Crispim. \nSource: OECD (2024), Education at a glance: Educational finance indicators, OECD Education Statistics (database), https://doi.org/10.1787/c4e1b551-en (accessed on 06 April 2024).",
    x = " ", y = " ",
    color = " "
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.box.spacing = margin(1),
    legend.key.width = unit(0.5, 'cm'),
    legend.key.height = unit(0.5, 'cm'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = 'white'), #change plot background color
    panel.background = element_rect(fill = 'white'), # change the panel background color (around title)
    strip.background = element_rect(fill='white'), # change strips background color
    #legend.background = element_rect(fill = 'white'), #change legend box background color
    plot.margin = margin(t = 1, r = 2, b = 0.5, l = 0, "cm"),
    plot.title = element_text(
      size = 14,
      face = "bold"),
    plot.subtitle = element_markdown(
      size = 8,
      face = "bold.italic",
      margin=margin(0,0,15,0)
    ),
    plot.caption = element_text(size = 6,
                                color = "black", 
                                hjust = 0,
                                vjust = 0.5),
    axis.text.x = element_text(size = 8,
                               vjust = -1),
    axis.title.x = element_text(vjust = -4)
    
  )

# Save plot
ggsave("outputs/plot-day6-comparisons-oecd-using-r.png", width = 9, height = 5)


