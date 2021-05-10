

# TidyTuesday 2021 w19: Water Point Exchange

# PACKAGES ----------------------------------------------------------------


library(tidyverse)
library(ggstream)
library(data.table)
library(patchwork)


# DATA --------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 19)

water <- tuesdata$water
unique(water$country_name)


# installations per year
water_year <- water %>% filter(install_year %in% c(1970:2015) & !is.na(country_name)& !country_name %in% c("Dominican Republic", "Egypt","Peru", "Timor-Leste")) %>% 
  mutate(water_source = case_when(water_source %like% "Shallow Well" ~ "Shallow Well",
                                  water_source %like% "Spring" ~ "Spring",
                                  water_source %like% "Borehole" ~ "Borehole",
                                  water_source %like% "Surface Water" ~ "Surface Water",
                                  is.na(water_source) ~ "Not Specified",
                                  TRUE ~ "Other")) %>% 
  group_by(install_year, water_source) %>%
  summarise(sum_source = n()) %>% ungroup()

# order factors
water_year$water_source <- factor(water_year$water_source, levels=c("Not Specified", "Shallow Well", "Borehole", "Spring", "Surface Water", "Other"))

# sum of installed water points
sum(water_year$sum_source)


# sum of installed water points per type
water_year %>% group_by(water_source) %>% summarise(total_source = sum(sum_source)) %>% 
  mutate(total_percentage = 100*total_source/sum(total_source))



# installation in countries - percentages
water_country <- water %>% filter(install_year %in% c(1970:2015) & !is.na(country_name)& !country_name %in% c("Dominican Republic", "Egypt","Peru", "Timor-Leste")) %>% 
  group_by(country_name) %>%
  summarise(sum_source = n()) %>% ungroup() %>% 
  mutate(perct_source = 100*sum_source/sum(sum_source),
         percent_categ = case_when(perct_source < 1 ~ "< 1%",
                                   perct_source > 1 & perct_source < 5 ~ "1% - 5%",
                                   perct_source > 5 & perct_source < 10 ~ "5% - 10%",
                                   perct_source > 10 & perct_source < 20 ~ "10% - 20%",
                                   perct_source > 20 ~ "> 20%")) %>%
  group_by(percent_categ) %>% 
  summarise(sum_percent = sum(perct_source)) %>%
  mutate(plot_label = sprintf("%g%%", round(sum_percent)))

# order factors
water_country$percent_categ <- factor(water_country$percent_categ , levels=c("< 1%", "1% - 5%", "5% - 10%", "10% - 20%","> 20%"))


# # installation in countries - list of countries for labels
country_percentage <- water %>% 
  filter(install_year %in% c(1970:2015) & !is.na(country_name)& !country_name %in% c("Dominican Republic", "Egypt","Peru", "Timor-Leste")) %>% 
  group_by(country_name) %>%
  summarise(sum_source = n()) %>% ungroup() %>% 
  mutate(perct_source = 100*sum_source/sum(sum_source),
         percent_categ = case_when(perct_source < 1 ~ "< 1%",
                                   perct_source > 1 & perct_source < 5 ~ "1% - 5%",
                                   perct_source > 5 & perct_source < 10 ~ "5% - 10%",
                                   perct_source > 10 & perct_source < 20 ~ "10% - 20%",
                                   perct_source > 20 ~ "> 20%"))



# PLOT --------------------------------------------------------------------

#drop of water
a <- ggplot(water_year, aes(install_year, sum_source, fill = water_source)) + 
  geom_stream(sorting = "none", size = 0.5, bw = 1, extra_span = 0.05,color = "light grey")  +
  theme_void()+
  scale_fill_manual(values = c("#012a4a","#468faf", "#8ecae6", "#90e0ef", "#caf0f8", "white"))+
  coord_flip(ylim = c(-15000, 40000), xlim = c(2015, 1960),clip = "off") + scale_x_reverse() +
  labs(title =
         "INSTALLATION OF WATER
POINTS IN AFRICAN
COUNTRIES",
       subtitle = "Using entries from the Water Point Data Exchange dataset
with non-missing installation years (1970-2015)") +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = "#4cabcd",color = "#4cabcd"),
        plot.title = element_text(size = 70, family = "Montserrat ExtraBold", color = "white", margin = margin(t = 30), lineheight = 0.8),
        plot.subtitle = element_text(size = 25, family = "Montserrat Italic", color = "white",  margin = margin(t = 0, b = 20)),
        axis.text.y = element_text(size = 25, family = "Montserrat Bold", color = "white"))  +
  scale_x_continuous(breaks = seq(1970,2010,10))


# annotations
a <- a +
  # titles
  annotate("text", y = -15000, x = 1960, size = 12, hjust = "left",vjust = "top", family = "Montserrat Bold",
           label = 
             "BETWEEN 1970 AND 2015
AT LEAST 321'498* WATER
POINTS WERE INSTALLED
IN 26 AFRICAN COUNTRIES", color = "white", lineheight = 0.8) +
  geom_vline(xintercept = 2015, size = 2, color = "white") +
  annotate("text", y = 16500, x = 1960, size = 7.7, hjust = "left",vjust = "top", family = "Montserrat Medium",
           label = 
             "About 50% of these water points
were installed between 2000 and
2010. From 1970 to 2010 the number
of water points installed increased.
Since 2010 there is a slow decrease
in the number of new installations.
Importantly, the type of installations
vary. Boreholes were the most
frequently installed, followed by
shallow wells.", color = "white", lineheight = 0.8)+
  
  # borehole
  annotate("segment", y = 0, x = 1985, yend = 14500, xend = 1985, size = 0.7, linetype = "dashed", color = "white") +
  annotate("text", y = 15000, x = 1985, hjust = "left",vjust = "bottom", family = "Montserrat Bold",label = "Borehole
(40%)", size = 23, color = "white", lineheight = 0.7) +
  
  
  # shallow well
  annotate("segment", y = 2000, x = 1995, yend = 14500, xend = 1995, size = 0.7, linetype = "dashed", color = "white") +
  annotate("text", y = 15000, x = 1995, size = 16, hjust = "left",vjust = "bottom", family = "Montserrat Bold",
           label = "Shallow
Well (25%)", color = "white", lineheight = 0.7) +
  
  # spring
  annotate("segment", y = -3000, x = 2000, yend = 14500, xend = 2000, size = 0.7, linetype = "dashed", color = "white") +
  annotate("text", y = 15000, x = 2000, hjust = "left",vjust = "bottom", family = "Montserrat Bold",label = "Spring (13%)", size = 10, color = "white", lineheight = 0.7) +
  
  
  # not specified
  annotate("segment", y = 5000, x = 2014, yend = 14500, xend = 2014, size = 0.7, linetype = "dashed", color = "white") +
  annotate("text", y = 15000, x = 2014, size = 9.5, hjust = "left",vjust = "bottom", family = "Montserrat Bold",
           label = "Not
Specified (11%)", color = "white", lineheight = 0.7) +
  
  # other
  annotate("segment", y = -8000, x = 2010, yend = 14500, xend = 2010, size = 0.7, linetype = "dashed", color = "white") +
  annotate("text", y = 15000, x = 2010, hjust = "left",vjust = "bottom", family = "Montserrat Bold",label = "Other (6%)", size = 6.5, color = "white", lineheight = 0.7) +     
  annotate("text", y = 21000, x = 2010, size = 4, hjust = "left",vjust = "bottom", family = "Montserrat Italic",
           label = 
             "Incl. Rainwater Harvesting, Delivered Water,
Sand or Sub-surface Dam, Piped or Packaged Water", color = "white", lineheight = 0.9) +   
  
  
  # surface water
  annotate("segment", y = -6500, x = 2006, yend = 14500, xend = 2006, size = 0.7, linetype = "dashed", color = "white") +
  annotate("text", y = 15000, x = 2006, hjust = "left",vjust = "bottom", family = "Montserrat Bold",label = "Surface
Water (5%)", size = 6.5, color = "white", lineheight = 0.7) 




# bar chart
b <- ggplot(water_country, aes(x=sum_percent, y=percent_categ, label = plot_label)) + 
  geom_bar(stat="identity", fill = "#4cabcd", color = "white", size = 1.5, width = 0.75) +
  geom_text(size = 8, position = position_stack(vjust = 0.5), family = "Montserrat Bold", color = "white") +
  theme_void() +
  labs(title = "WHERE WERE THESE WATER POINTS INSTALLED?",
       subtitle = 
         "Some significant disparities can be noticed as to where water points were installed between 1970 and 2015.
Close to 60% of water points were installed in three countries: Uganda, Nigeria and Sierra Leone.
For 15 countries altogether, the number of water points installed made up less than 2% of the total.",
       caption = "*based on water points with non-missing installation years | Data: Water Point Data Exchange | #TidyTuesday | @a_bagaini") +
  theme(legend.position = "none",
        plot.title = element_text(size = 38, family = "Montserrat Bold", 
                                  color = "white", margin = margin(t = 1), lineheight = 0.8),
        plot.caption = element_text(size = 16, color = "white", hjust = 0.95, margin = margin(b = 5, t = 5), family = "Montserrat Medium", lineheight = 0.7),
        plot.subtitle = element_text(size = 18, family = "Montserrat Medium",
                                     color = "white",  margin = margin(t = 10, b = 10)),
        plot.background = element_rect(fill = "#4cabcd", color = "#4cabcd")) +
  annotate("text", x = 32, y = 5, size = 12, hjust = "left",vjust = "center", family = "Montserrat Bold",
           label = "Uganda", color = "white", lineheight = 0.9) +
  annotate("text", x = 29.5, y = 4, size = 9.5, hjust = "left",vjust = "center", family = "Montserrat Bold",
           label = 
             "Nigeria, Sierra Leone", color = "white", lineheight = 0.7) +
  annotate("text", x = 27.75, y = 3, size = 8, hjust = "left",vjust = "center", family = "Montserrat Bold",
           label = "Swaziland, Tanzania,
Ghana, Ethiopia", color = "white", lineheight = 0.7) +
  annotate("text", x = 11.5, y = 2, size = 6, hjust = "left",vjust = "center", family = "Montserrat Bold",
           label = "Zimbabwe, Liberia, Zambia,
Kenya", color = "white", lineheight = 0.7) +
  annotate("text", x = 2.5, y = 1, size = 5, hjust = "left",vjust = "center", family = "Montserrat Bold",
           label = 
             "Algeria, Burkina Faso, Burundi, Central African Republic, Chad, Republic of Congo, Gambia, Madagascar, Mali, Mozambique,
Namibia, Niger, Rwanda,South Sudan, Sudan", color = "white", lineheight = 0.7) +
  scale_x_continuous(breaks = seq(0,35,5), limits = c(0,40),expand = expansion(mult = c(0, .1)))



# merging
c <- a/b + plot_layout(heights = c(3, 0.75))



# saving
ggsave(c, file="TidyTuesdayWater2021_w19.png", device="png",dpi = 600, width = 40, height = 57, units = "cm") # can adjust resolution


