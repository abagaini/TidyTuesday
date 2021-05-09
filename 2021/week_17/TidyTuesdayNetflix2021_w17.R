
# LIBRARIES ---------------------------------------------------------------



library(tidyverse)



# DATA --------------------------------------------------------------------


netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')
names <- paste(str_to_lower(netflix_titles$title), collapse = "")
character <- strsplit(names, "") # split characters

dt <- as.data.frame(table(character)) %>% filter(!character %in% c(" ", "\n")) # include all characters and ignore spacings
dt <- dt[-c(1,6), ] # include all characters and ignore spacings/blanks

# select n e t f l i x
dt_netflix <- dt %>% filter(character  %in% c("n", "e", "t", "f", "l", "i", "x"))
dt_netflix <- dt_netflix %>% mutate(sum_char = sum(dt$Freq), perc = (Freq/sum_char)*100, font_size = round(9*perc))

sum(dt_netflix$Freq)
sum(dt$Freq)


# mean centering occurence of all font "readable" characters 
dt_c <- dt %>% mutate(mean_freq = round(mean(Freq)), freq_c = Freq - mean_freq) 
dt_c$character <- iconv(dt_c$character, "latin1", "ASCII", sub="") # cannot read all characters
dt_c <- dt_c %>% filter(character != "") 





# PLOTS -------------------------------------------------------------------

# netflix logo
p <- ggplot() + theme_void() + theme(plot.background = element_rect(fill = "grey20"), 
                                     plot.caption = element_text(size = 12, color = "#E50914", hjust = 0.98, family = "Bebas Neue", margin = margin(b = 5, t = 10)))+
  annotate(geom = "text", label = "N", size = 58, x = 10, y = 8, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "#e50914") +
  annotate(geom = "text", label = "N: 7'745 (6.40%)", size = 7, x = 9.7, y = 17, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "grey50") +
  
  annotate(geom = "text", 
           label = 
             "Font size represents how often
           that letter appears in the titles
           relative to the whole catalogue", size = 4, lineheight = 0.7, x = 9.93, y = 8, vjust = "bottom", hjust = "right", family = "Montserrat Regular", color = "grey50") +
  
  annotate(geom = "text", 
           label = 
             "X is super tiny", size = 4, lineheight = 0.7, x = 10.185, y = 8, vjust = "bottom", hjust = "left", family = "Montserrat Regular", color = "grey50") +
  
  annotate(geom = "text", 
           label = 
             "# of occurences\n(as a % of the whole catalogue)", size = 4, lineheight = 0.7, x = 9.7, y = 16.7, vjust = "top", hjust = "right", family = "Montserrat Regular", color = "grey50") +
  
  annotate(geom = "text", label = "E", size = 99, x = 10.075, y = 8, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "#E50914") + 
  annotate(geom = "text", label = "E: 13'372 (11.05%)", size = 7, x = 9.85, y = 17, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "grey50") +
  annotate(geom = "text", label = "T", size = 64, x = 10.118, y = 8,vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "#E50914") + 
  annotate(geom = "text", label = "T: 8'657 (7.15%)", size = 7, x = 9.98, y = 17, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "grey50") + 
  annotate(geom = "text", label = "F", size = 16, x = 10.113, y = 8, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "#E50914") + 
  annotate(geom = "text", label = "F: 2'202 (1.82%)", size = 7, x = 10.11, y = 17, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "grey50") + 
  annotate(geom = "text", label = "L", size = 42, x = 10.146, y = 8, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "#E50914") + 
  annotate(geom = "text", label = "L: 5'650 (4.67%)", size = 7, x = 10.23, y = 17, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "grey50") + 
  annotate(geom = "text", label = "I", size = 62, x = 10.171, y = 8, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "#E50914") +  
  annotate(geom = "text", label = "I: 8'274 (6.84%)", size = 7, x = 10.35, y = 17, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "grey50") + 
  annotate(geom = "text", label = "X", size = 2, x = 10.168, y = 8, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "#E50914") + 
  annotate(geom = "text", label = "X: 248 (0.20%)", size = 7, x = 10.47, y = 17, vjust = "bottom", hjust = "right", family = "Bebas Neue", color = "grey50") + 
  xlim(9.5,10.6) + ylim(0,20) + 
  labs(caption = "*includes letters, numbers, common punctuation,letters with accents, arabic letters, as well Chinese, Japanese and Korean characters\nTitle of Movies & Series in the Netflix catalogue | Data: kaggle | #TidyTuesday | @a_bagaini") +
  annotate(geom = "text", label = "7'787 titles  | 121'038 characters*", size = 8, x = 10.06, y = 3, vjust = "bottom", hjust = "center", family = "Bebas Neue", color = "#E50914")

p



# character occurence


theme_set(theme_void())
p2 <- dt_c  %>% ggplot(aes(x = reorder(character, freq_c), y = freq_c, label = character)) +
  geom_bar(stat = "identity", fill = "grey40",colour = "grey60", size = 0.5, width = 0.85) + 
  labs(y = "Mean centered number of occurences",
       caption = "*only listing letters, numbers, common punctuation,not listing certain special characters such as letters with accents, arabic letters, as well Chinese, Japanese and Korean characters\nTitle of Movies & Series in the Netflix catalogue | Data: kaggle | #TidyTuesday | @a_bagaini") +
  theme(plot.background = element_rect(fill = "grey20"),
        plot.margin = margin(l = 10, r = 20),
        axis.text.y = element_text(size = 12, colour = "#E50914", family = "Bebas Neue"),
        panel.grid.major.y = element_line(colour = "grey40", size = 0.1),
        plot.caption = element_text(size = 12, color = "#E50914", hjust = 0.98, margin = margin(b = 5, t = 10), family = "Bebas Neue", lineheight = 0.7),
        axis.title.y = element_text(size = 15, colour = "#E50914", angle = 90, margin = margin(r = 10, l = 10), family = "Bebas Neue"))+
  scale_y_continuous(breaks = c(-1000,0, seq(2000, 14000, 4000))) +
  geom_hline(aes(yintercept = 0), colour = "grey60", size = 0.8) +
  annotate(geom = "text",label = 
             "Occurrences of characters*\nin the Netflix catalogue", lineheight = 0.7,
           size = 20, x = 2, y = 12500, vjust = "top", hjust = "left", family = "Bebas Neue", color = "#E50914" )+
  annotate(geom = "text",label = 
             "Relative to the mean", lineheight = 0.7,
           size = 15, x = 2, y = 10400, vjust = "top", hjust = "left", family = "Bebas Neue", color = "#E50914" )+
  annotate(geom = "text",label = "Mean occurrence: 756", size = 5, x = 2, y = 9700, vjust = "top", hjust = "left", family = "Montserrat Regular", color = "grey60" )+
  annotate(geom = "text",label = "Median occurrence: 2", size = 5, x = 10, y = 9700, vjust = "top", hjust = "left", family = "Montserrat Regular", color = "grey60" )+
  geom_text(position = position_stack(vjust = .5), color = "grey80", family = "Bebas Neue", size = 11) 
p2

# save
ggsave(p, file="tidytue_w17_1.png", device="png",dpi = 400, width = 50, height = 25, units = "cm") # can adjust resolution
ggsave(p2, file="tidytue_w17_2.png", device="png",dpi = 400, width = 50, height = 25, units = "cm") # can adjust resolution
