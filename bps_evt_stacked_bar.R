
# Author and data ----
# Randy Swaty
# January 14, 1971

# Notes ----

## More notes ----
# make stacked bar chart, where each bar is a BpS, and the sections of each bar representing ag, urban, natural veg and other clearings

# input data is a combine of BpS and EVT data for the AOI

# Dependencies ----

library(scales)
library(tidyverse)
library(plotly)


# Load data
data <- read.csv("data/bps_evt_stacked_bar.csv")

# Remove geographic modifiers
geographies <- c(
  "Boreal ",
  "Central Interior and Appalachian ",
  "Great Lakes ",
  "Laurentian ",
  "Laurentian-Acadian ",
  "North-Central Interior ",
  "Northeastern Interior ",
  "Appalachian ",
  "Acadian-Appalachian ",
  "Central Appalachian ",
  "Acadian "
)

data$BPS_NAME <- gsub(paste(geographies, collapse = "|"), "", data$BPS_NAME)


# Set the levels for EVT_PHYS
data$EVT_PHYS <- factor(data$EVT_PHYS, levels = c(
  "Natural Vegetation",
  "Agricultural",
  "Developed"
))


# group for "floodplain" and other shortened names that have multiples
grouped_data <- data %>%
  drop_na() %>%  
  group_by(BPS_NAME, EVT_PHYS) %>%
  summarize(HECTARES = sum(HECTARES)) 

grouped_data_ordered <- grouped_data %>%
  group_by(BPS_NAME) %>%
  mutate(total_hectares = sum(HECTARES)) %>%
  ungroup() %>%
  arrange(desc(total_hectares)) 


# Reorder BPS_NAME factor levels based on the summarized data
grouped_data_ordered$BPS_NAME <- factor(grouped_data_ordered$BPS_NAME, levels = unique(grouped_data_ordered$BPS_NAME))




# Reverse the levels for EVT_PHYS
### data$EVT_PHYS <- factor(data$EVT_PHYS, levels = rev(levels(data$EVT_PHYS)))



plot <-
  ggplot(grouped_data_ordered, aes(fill = EVT_PHYS, y = HECTARES, x = BPS_NAME)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  labs(
    title = "Conversion status",
    caption = "Data from landfire.gov.",
    x = "",
    y = "Hectares",
    fill = "Status") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = comma) + 
  theme_bw() + 
  scale_fill_manual(values = c("#74a36f", # orange
                               "#532a66", # purple
                               "#827c75", # grey
                               "#f5eb2f", # yellow
                               "#74a36f" # green-natural veg
  )) +
  theme(plot.caption = element_text(hjust = 0, face = "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") 

plot

plotly <- ggplotly(plot, height = 1500, width = 900)  


plotly
