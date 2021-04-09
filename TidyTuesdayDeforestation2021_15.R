# TidyTuesday Challenge 2021 week 15 : Deforestation


# LIBRARIES ---------------------------------------------------------------


library(tidyverse)
library(waffle)

# DATA --------------------------------------------------------------------

# get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 15)

brazil_loss <- tuesdata$brazil_loss

#re-naming causes and scaling variables
brazil_loss <- brazil_loss %>% pivot_longer(commercial_crops:small_scale_clearing, values_to = "loss", names_to = "cause") %>% 
  group_by(cause) %>% 
  summarize(sum_loss = sum(loss)/50000) %>% 
  mutate(cause = case_when(cause == "commercial_crops" ~ "Commercial Crops",
                           cause == "mining" ~ "Mining",
                           cause == "pasture" ~ "Pasture",
                           cause == "small_scale_clearing" ~ "Small Scale Clearing",
                           cause == "fire" ~ "Fire",
                           cause == "roads" ~ "Roads",
                           cause == "natural_disturbances" ~ "Natural Disturbances",
                           cause == "selective_logging" ~ "Selective Logging",
                           cause == "flooding_due_to_dams" ~ "Flooding due to Dams",
                           cause == "other_infrastructure" ~ "Other Infrastructure",
                           cause == "tree_plantations_including_palm" ~ "Tree Plantations",
                           cause == "fire" ~ "Fire"))

reordered_df <- brazil_loss %>% arrange(desc(sum_loss))
brazil_loss$cause <- factor(brazil_loss$cause,      # Reordering factor levels
                         levels = reordered_df$cause)


# PLOT --------------------------------------------------------------------

theme_set(theme_void(base_family = "Roboto Condensed"))

p <- brazil_loss %>%
  ggplot(aes(fill = "black",values = sum_loss)) +
  geom_waffle(color = "#e7e8c4", size = 0.5, n_rows = 4) + 
  facet_wrap(cause~., nrow = 11, strip.position = "left")+
  scale_y_discrete() +
  scale_x_continuous(labels = function(x) x * 4, 
                     expand = c(0,1)) +
  coord_equal() +
  labs(
    y = "What \nis \ndriving \nthe \ndeforestation \nin \nBrazil?",
    subtitle = "1 square = 50'000 hectares",
    caption = "Data from ourworldindata.org | TidyTuesday | @a_bagaini",
    title = "Total surface of forest lost between 2001 and 2013"
  ) +
  theme(legend.position = "none",
        plot.title = element_text(size = 24, face = "bold.italic",hjust = 0, margin = margin(t = 20, r = 0, b = 5, l = 0), lineheight = 0.1),
        plot.subtitle = element_text(size = 18, face = "italic",hjust = 0, margin = margin(t = 0, r = 0, b = 10, l = 0), lineheight = 0.1),
        text = element_text(colour = "#41240c"),
        plot.caption = element_text(face = "italic", hjust = 0.97, size = 9,  margin = margin(t = 0, r = 0, b = 5,l = 0)),
        axis.title.y = element_text(face = "bold", size = 50, angle = 0, vjust = 1, hjust = 1,margin = margin(t = 0, r = 60, b = 20, l = 20)),
        panel.background = element_rect(fill = "#e7e8c4", colour = "#e7e8c4"),
        plot.background = element_rect(fill = "#e7e8c4", colour = "#e7e8c4"),
        panel.spacing.y = unit(0.1, "lines"),
        strip.text.y.left = element_text(hjust = 0.95, vjust = 0.2, face = "bold.italic", size = 14, angle = 0, margin = margin(t = 0, r = 05, b = 0, l = 0))) +
  scale_fill_manual(values = "#174a2c")

# save
ggsave(p, file="plot.png", device="png",dpi = 300, width = 40, height = 40, units = "cm") # can adjust resolution





 




