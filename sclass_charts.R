
### Notes -----

# Succession Class charts for the Great Lakes Basin
# Will try different styles for one main BpS then get feedback
# Need to have meaningful labels


## Dependencies -----

library(tidyverse)

raw_data <- read.csv('data/final_df_full.csv')


### Wrangle for one BpS; clean up  -----

northern_hardwoods_hemlock <- raw_data %>%
  filter(bps_name %in% 'Laurentian-Acadian Northern Hardwoods Forest - Hemlock') %>%
  mutate(label = coalesce(state_class_id, ref_label)) %>% #replace NAs with values from ref_label %>%
  dplyr::select(c("bps_name", "label",  "cur_percent", "ref_percent")) %>%
  pivot_longer(
    cols = c(`ref_percent`, `cur_percent`), 
    names_to = "ref_cur", 
    values_to = "percent"
  )

# note this one is 'easy' with only one BPS_MODEL.  No grouping necessary


## Grouped Bar -----

# order classes
northern_hardwoods_hemlock$label <- factor(northern_hardwoods_hemlock$label, levels= c(
  "Developed",
  "Agriculture",
  "UE",
  "UN",
  "Late1:CLS",
  "Mid2:CLS",
  "Mid1:CLS",
  "Early2:ALL",
  "Early1:ALL"))

sclass_bar <-
  ggplot(northern_hardwoods_hemlock, aes(fill = factor(ref_cur), y = percent, x = label)) + 
  geom_col(width = 0.8, position = position_dodge()) +
  coord_flip() +
  scale_x_discrete(limits = (levels(northern_hardwoods_hemlock$label))) +
  labs(
    title = "Succession Classes past and present",
    subtitle = "Northern Hardwoods-Hemlock",
    caption = "\nData from landfire.gov.",
    x = "",
    y = "Percent") +
  theme_minimal(base_size = 18) +
  theme(plot.caption = element_text(hjust = 0, face = "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") +
  scale_fill_manual(values = c("#3d4740", "#32a852" ), # present (grey), historical (green)
                    name = " ", 
                    labels = c("Present",
                               "Past")) 

sclass_bar

## Arrow chart -----

northern_hardwoods_hemlock_wide <- raw_data %>%
  filter(bps_name %in% 'Laurentian-Acadian Northern Hardwoods Forest - Hemlock') %>%
  mutate(label = coalesce(state_class_id, ref_label)) %>% #replace NAs with values from ref_label %>%
  mutate(cur_percent = replace_na(cur_percent, 0)) %>%
  dplyr::select(c("bps_name", "label",  "cur_percent", "ref_percent")) %>%
  na.omit(label)   %>% 
  mutate(change = cur_percent - ref_percent,
         sign_change = (change > 0)) %>%
  filter(label != "Water")

northern_hardwoods_hemlock_wide$label <- factor(northern_hardwoods_hemlock_wide$label, 
                                                levels = c(
  "Urban",
  "Agriculture",
  "UE",
  "UN",
  "Late1:CLS",
  "Mid2:CLS",
  "Mid1:CLS",
  "Early2:ALL",
  "Early1:ALL"))
  

arrow_plot <- northern_hardwoods_hemlock_wide %>%
  ggplot(aes(
    x = ref_percent, xend = cur_percent, 
    y = label, yend = label,
    color = sign_change)) +
  geom_segment(
    arrow = arrow(angle = 30, length = unit(0.3, 'cm')),
    size = 2.5) +
  labs(
    x = 'Percent Change', 
    y = element_blank(),
    title = 'Changes in Succession Class Percentages,  \nHistorical to ~2022',
    subtitle = ''
  ) +
  scale_color_manual(
    values = c("#fcba03", "#10692c")) +
  theme_bw(base_size = 18) + 
  theme(legend.position = "none")

arrow_plot

## diverging bar -----

# Color based on value
color <- ifelse(northern_hardwoods_hemlock_wide$change < 0, "#fcba03", "#10692c")

diverging_bar <- 
ggplot(northern_hardwoods_hemlock_wide, aes(x = reorder(label, change), y = change)) +
  geom_bar(stat = "identity",
           show.legend = FALSE,
           fill = color) +
  coord_flip()  +
  labs(
    title = "Change in succession class percentages from past to present",
    subtitle = "Northern Hardwoods-Hemlock",
    caption = "\nData from landfire.gov.",
    x = "",
    y = "Percent") +
  theme_minimal(base_size = 18) +
  theme(plot.caption = element_text(hjust = 0, face = "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot")


diverging_bar

