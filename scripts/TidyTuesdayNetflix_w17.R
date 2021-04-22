library(tidyverse)
library(waffle)


netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')
names <- paste(str_to_lower(netflix_titles$title), collapse = "")
character <- strsplit(names, "")

dt <- as.data.frame(table(character)) %>% filter(!character %in% c(" ", "\n")) # ignore spacings
dt <- dt[-c(1,6), ] 

dt_netflix <- dt %>% filter(character  %in% c("n", "e", "t", "f", "l", "i", "x"))
dt_netflix <- dt_netflix %>% mutate(sum_char = sum(dt$Freq), perc = (Freq/sum_char)*100, font_size = round(9*perc)) # define font size based on %

sum(dt_netflix$Freq)
sum(dt$Freq)



# recreate logo (not that efficient, do not know another way to go about this)
#use dt_netflix to specify the font sizes and manually place the letters/text

p <- ggplot() + theme_void() + theme(plot.background = element_rect(fill = "grey20"), plot.caption = element_text(size = 12, color = "#E50914", hjust = 0.98, family = "Bebas Neue"))+
  annotate(geom = "text", label = "N", size = 58, x = 10, y = 8, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "#e50914") +
  annotate(geom = "text", label = "N: 7'745 (6.40%)", size = 7, x = 9.7, y = 16, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "grey50") +
 
  annotate(geom = "text", 
           label = 
             "Font size represents how often
           each letter appears in the titles
           relative to the whole catalogue", size = 4, lineheight = 0.7, x = 9.93, y = 9, vjust = "bottom", hjust = "right", family = "Montserrat Regular", color = "grey50") +
 
  annotate(geom = "text", 
           label = 
             "X is super tiny", size = 4, lineheight = 0.7, x = 10.185, y = 8, vjust = "bottom", hjust = "left", family = "Montserrat Regular", color = "grey50") +
  
  annotate(geom = "text", 
           label = 
             "# of occurences\n(as a % of the whole catalogue)", size = 4, lineheight = 0.7, x = 9.7, y = 15.7, vjust = "top", hjust = "right", family = "Montserrat Regular", color = "grey50") +
  
   annotate(geom = "text", label = "E", size = 99, x = 10.075, y = 8, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "#E50914") + 
  annotate(geom = "text", label = "E: 13'372 (11.05%)", size = 7, x = 9.85, y = 16, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "grey50") +
  annotate(geom = "text", label = "T", size = 64, x = 10.118, y = 8,vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "#E50914") + 
  annotate(geom = "text", label = "T: 8'657 (7.15%)", size = 7, x = 9.98, y = 16, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "grey50") + 
  annotate(geom = "text", label = "F", size = 16, x = 10.117, y = 8, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "#E50914") + 
  annotate(geom = "text", label = "F: 2'202 (1.82%)", size = 7, x = 10.11, y = 16, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "grey50") + 
  annotate(geom = "text", label = "L", size = 42, x = 10.15, y = 8, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "#E50914") + 
  annotate(geom = "text", label = "L: 5'650 (4.67%)", size = 7, x = 10.23, y = 16, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "grey50") + 
  annotate(geom = "text", label = "I", size = 62, x = 10.175, y = 8, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "#E50914") +  
  annotate(geom = "text", label = "I: 8'274 (6.84%)", size = 7, x = 10.35, y = 16, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "grey50") + 
  annotate(geom = "text", label = "X", size = 2, x = 10.173, y = 8, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "#E50914") + 
  annotate(geom = "text", label = "X: 248 (0.20%)", size = 7, x = 10.47, y = 16, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "grey50") + 
  xlim(9.5,10.6) + ylim(0,20) + 
  labs(caption = "Title of Movies & Series in the Netflix catalogue | Data: kaggle | #TidyTuesday | @a_bagaini") +
  annotate(geom = "text", label = "7'787 titles | 121'038 characters/Numbers/Symbols ", size = 8, x = 10.06, y = 3, vjust = "bottom", hjust = "center", family = "Bebas Neue", color = "#E50914")

p


# Characters, symbols and numbers with at least 100 occurances (scaling to 100)
dt_s_100 <- dt %>% mutate(Freq_s = round(Freq/100), constant = "a") %>% filter(Freq_s > 0)

p2 <- ggplot(dt_s_100, aes(values = Freq_s, fill = constant)) +
  theme_void()+
  geom_waffle(color = "grey20", size = .35, n_rows = 10, flip = TRUE) +
  facet_wrap(~character, ncol = 22, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_fill_manual( values = c("#E50914")) +
  coord_equal() +
  labs(title = "The Characters, numbers & symbols with at least 100 occurrences",
       caption = "Title of Movies & Series in the Netflix catalogue | Data: kaggle | #TidyTuesday | @a_bagaini",
       subtitle = "1 square = 100 occurrences") +
  theme_minimal(base_family = "Bebas Neue") +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "grey20"),
        legend.position = "none",
        axis.title.y  = element_blank(), 
        axis.text.y  = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = 32, color = "#E50914"),
        plot.caption = element_text(size = 4, color = "#E50914", hjust = 0.98),
        plot.subtitle = element_text(size = 20, color = "#E50914", margin = margin(b = 30)),
        strip.text.x  = element_text(size = 15, color = "#E50914", hjust = 0.05)) 
p2



# save
ggsave(p, file="tidytue_w17_1.png", device="png",dpi = 600, width = 50, height = 25, units = "cm") # can adjust resolution
ggsave(p2, file="tidytue_w17_2.png", device="png",dpi = 600, width = 30, height = 17, units = "cm") # can adjust resolution
