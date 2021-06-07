
# mix of TidyTuesday and RecreationThursday
# Alexandra Bagaini
# PACKAGES ----------------------------------------------------------------

library(tidyverse)

# DATA --------------------------------------------------------------------
# reading data
tuesdata <- tidytuesdayR::tt_load('2021-05-18')
data <- tuesdata$survey


# selecting relevant vars
data <- data %>%
  select(timestamp, how_old_are_you,country, industry,annual_salary,currency,years_of_experience_in_field,
         highest_level_of_education_completed, gender, race)

# LINK TO THE ORIGINAL SURVEY https://www.askamanager.org/2021/04/how-much-money-do-you-make-4.html

#____________WRANGLING & DATA QUALITY CHECKS____________#
# CURRENCY
# keep only USD because we'll focus on USA
data <- data %>%filter(currency == "USD") 

# COUNTRY: CLEAN FREE TEXT ENTRIES
# inspect the list of countries people entered and get an idea of how differently the USA was written
data <- data %>% mutate(country = tolower(country)) # all lower case
unique(data$country)
# focus on usa
keep_if = c("united s", "u.s", "u. s", "us") #trying to capture as many ways to spell USA
data <- subset(data, grepl(paste(keep_if, collapse="|"), country))
# after this there were some unwanted rows
remove_if = c("aust", "rus", "argent", "base", "gov", "comp", "bonus") #trying to remove non-relevant rows
data <- subset(data, !grepl(paste(remove_if, collapse="|"), country))
# just have ONE way to spell USA
data <- data %>% mutate(country = "U.S.A")

# INDUSTRY: CLEAN CATEGORIES (some inconsistencies with upper/lower case and the OTHER option)
data <- data %>% mutate(industry = tolower(industry)) # all lower case
unique(data$industry) # ~800 "different" industries....
# although we are probably losing some valuable details in the info and some industries could be reclassified, we'll just keep entries as they are
#  if the name of the industry is consistent with the one in the list of industries that are
#  in the orginal Google form otherwise it will be labeled as "Other".
google_form_industries <- tolower(c("Accounting, Banking & Finance",
                                    "Agriculture or Forestry",
                                    "Art & Design",
                                    "Business or Consulting",
                                    "Computing or Tech",
                                    "Education (Primary/Secondary)",
                                    "Education (Higher Education)",
                                    "Engineering or Manufacturing",
                                    "Entertainment",
                                    "Government and Public Administration",
                                    "Health care",
                                    "Hospitality & Events",
                                    "Insurance",
                                    "Law",
                                    "Law Enforcement & Security",
                                    "Leisure, Sport & Tourism",
                                    "Marketing, Advertising & PR",
                                    "Media & Digital",
                                    "Nonprofits",
                                    "Property or Construction",
                                    "Recruitment or HR",
                                    "Retail",
                                    "Sales",
                                    "Social Work",
                                    "Transport or Logistics",
                                    "Utilities & Telecommunications")) # thank you datapasta addin! ;)

data_other <- data %>% filter(!is.na(industry) & !industry %in% google_form_industries) %>% 
  mutate(industry = "other") # industries NOT in google form list (quite a lot to do with libraries + research like pharma & biotech)
# join data_other with the entries that appear in the google list
data <- data %>% filter(industry %in% google_form_industries) %>% bind_rows(data_other)

unique(data$industry) # now 27 different industries; better ! :)


# GENDER: Grouping "Non-binary" and "Other" and "Prefer not to say" together (samples are low)
table(data$gender)
data <- data %>% mutate(gender = if_else(!gender %in% c("Woman", "Man"), "Non-Binary & Other", gender))

# HIGHEST LEVEL OF EDUCATION --> OK 

# EXPERIENCE IN FIELD + CONSITENCY WITH AGE GROUP 
table(data$years_of_experience_in_field, data$how_old_are_you) # ... some issues here (years of experience > age) let's fix it & remove odd cases
#1) Convert categories into numeric ages and year of experience
data <- data %>% mutate(up_age = case_when(how_old_are_you == "under 18" ~ 17,
                                           how_old_are_you == "18-24" ~ 24,
                                           how_old_are_you == "25-34" ~ 34,
                                           how_old_are_you == "35-44" ~ 44,
                                           how_old_are_you == "45-54" ~ 54,
                                           how_old_are_you == "55-64" ~ 64,
                                           how_old_are_you == "65 or over" ~ 66),
                        low_field_exp = case_when(years_of_experience_in_field == "1 year or less" ~ 0.5,
                                                  years_of_experience_in_field == "2 - 4 years" ~ 2,
                                                  years_of_experience_in_field == "5-7 years" ~ 5,
                                                  years_of_experience_in_field == "8 - 10 years" ~ 8,
                                                  years_of_experience_in_field == "11 - 20 years" ~ 11,
                                                  years_of_experience_in_field == "21 - 30 years" ~ 21,
                                                  years_of_experience_in_field == "31 - 40 years" ~ 31,
                                                  years_of_experience_in_field == "41 years or more" ~ 42)) %>% 
  filter(up_age > low_field_exp + 15) # only select rows where age > experience in the field + 15 (arbitrary chosen min working age...)
table(data$years_of_experience_in_field, data$how_old_are_you) # a bit better!


# SALARY: range of values
data %>% ggplot(aes(x = annual_salary)) + geom_histogram(bins = 100)
summary(data$annual_salary)
# there are some very high and low values; will need to filter some of these out later
#let's round these to the closest 100 dollars
data <- data %>% mutate(annual_salary = round(annual_salary, -2))

# FILTER OUT ROWS WITH CERTAIN ANSWERS
data <- data %>% filter(
  !is.na(annual_salary) & annual_salary %in% c(1000:300000) & # keep responses with salaries between 1'000 & 300'000 USD
    !is.na(how_old_are_you) & how_old_are_you != "under 18" & # keep only over 18
    !is.na(highest_level_of_education_completed) & # keep only entries with educa. information
    !is.na(gender) & # keep only entries with gender information
    !is.na(race) & # keep only entries with race information
    !is.na(industry)) # keep only entries with industry information


#____________SUMARIZING DATA FOR THE PLOT____________#

# had a look at the salaries for different groups of respondents and based on different characteristics
#  I decided to focus on industry and education, such that values would mimic most of the lines in  Alfredo Hlito's Curves and Straight Lines (1948).

edu_sal_l <- data %>% group_by(highest_level_of_education_completed) %>% summarise(median_salary = median(annual_salary)) 

edu_sal_w <- edu_sal_l %>% 
  filter(highest_level_of_education_completed %in% c("Master's degree", "High School")) %>% 
  pivot_wider(names_from = highest_level_of_education_completed, values_from = median_salary)


indust_sal_l <- data %>% 
  group_by(industry) %>% summarise(median_salary = median(annual_salary)) 

indust_sal_w <- indust_sal_l %>% 
  filter(industry %in% c("retail","agriculture or forestry", "art & design", 	"recruitment or hr" ,
                         "marketing, advertising & pr", "insurance", "business or consulting", 
                         "engineering or manufacturing", "law")) %>% pivot_wider(names_from = industry, values_from = median_salary)



# PLOTS -------------------------------------------------------------------
# adapted from Maya Gans's script https://github.com/MayaGans/recreating_art/blob/main/code/curves_and_straights.R

#____________CREATING DF FOR PLOTS ____________#


data_indust <- data.frame(
  colors = c("#DA381D", "black", "#064E9F", "#DA381D", "#75A621",  "#DA381D", "#75A621","#064E9F", "black"), 
  x1 = c(c(2,2,2,2,2), c(13,13,13,13)),
  y1 = c(c(indust_sal_w$retail, indust_sal_w$`agriculture or forestry`, indust_sal_w$`art & design`,
           indust_sal_w$`recruitment or hr`, indust_sal_w$`marketing, advertising & pr`), 
         c(indust_sal_w$insurance, indust_sal_w$`business or consulting`, 
           indust_sal_w$`engineering or manufacturing`, indust_sal_w$law)),
  xend = c(c(4,4,4,4,4), c(15,15,15,15)),
  yend  = c(c(indust_sal_w$retail, indust_sal_w$`agriculture or forestry`, indust_sal_w$`art & design`,
              indust_sal_w$`recruitment or hr`, indust_sal_w$`marketing, advertising & pr`), 
            c(indust_sal_w$insurance, indust_sal_w$`business or consulting`, 
              indust_sal_w$`engineering or manufacturing`, indust_sal_w$law)))

data_edu <- data.frame(y = c(edu_sal_w$`Master's degree`,edu_sal_w$`High School`),
                       x = c(6,13),
                       labels = c("a", "b"))

num_labels = data.frame(labels = as.character(c(1:9)),
                        x = c(c(1.8,1.8,1.8,1.8,1.8), c(15.2,15.2,15.2,15.2)),
                        y = c(c(indust_sal_w$retail, indust_sal_w$`agriculture or forestry`, indust_sal_w$`art & design`,
                                indust_sal_w$`recruitment or hr`, indust_sal_w$`marketing, advertising & pr`), 
                              c(indust_sal_w$insurance, indust_sal_w$`business or consulting`, 
                                indust_sal_w$`engineering or manufacturing`, indust_sal_w$law)))

let_labels = data.frame(labels = c("a", "b"),
                        x = c(5.8,13.2),
                        y = c(edu_sal_w$`Master's degree`,edu_sal_w$`High School`))

#____________PLOTTING____________#

p <- ggplot() +
  
  # main elements
  geom_segment(data = data_indust, aes(x = x1, y = y1, xend = xend, yend = yend),size = 1.5, color = data_indust$colors) +
  geom_curve(aes(x = 8, y = 73500, xend = 8.5 , yend = 40000),size = 1.8,color = "#064E9F", linetype = "solid", arrow = arrow(length = unit(0.2, "inches"))) +
  geom_curve(aes(x = 7, y = 121200, xend = 3.7 , yend = 58000),size = 1.7,  curvature = -0.4, color = "#C5B854",  linetype = "solid") +  
  geom_line(data = data_edu, aes(x = x, y = y), size = 1.8, color = "#010101") + 
  theme_void() +
  labs(y = "Median Annual Salary (USD)", 
       title = 
         "An Abstract Look at
the Annual Salaries
Reported in Ask A
Manager's Survey",
       subtitle = "A Focus on Industry & Education",
       caption = 
         "Data: Ask A Manager (only including respondents working in the U.S.A) |
Adaptation of Alfredo Hlito, Curves and Straight Lines (1948) | #RecreationThursday | #TidyTuesday | @a_bagaini") +
  theme(
    plot.caption = element_text(size = 8, color = "black", hjust = 0.95,lineheight = 0.9, margin = margin(b = 5, t = 5), family = "Roboto"),
    plot.background = element_rect(fill = "#E6DED1"), 
    plot.title = element_text(family = "Roboto Black", color = "black", size = 28, lineheight = 0.8, hjust = 0),
    plot.subtitle = element_text(family = "Roboto Italic", color = "black", size = 16, lineheight = 0.7,
                                 margin = margin(b = 40, t = 5)),
    axis.text.y = element_text(angle = 90, family = "Roboto", color = "black", size = 10),
    axis.title.y = element_text(angle = 90, family = "Roboto Medium", color = "black",size = 12,hjust = 0.7,
                                margin = margin(l = 15, r = 10))) +
  scale_y_continuous(limits = c(20000,125000), breaks = seq(50000,125000,25000)) +
  scale_x_continuous(limits = c(-1.5,19)) +
  
  # some labels
  annotate(geom = "text", label = 
             "Hospitality", x = 3.5 , y = 58000, hjust = 1, lineheight = 0.9,  family = "Roboto", size = 3.25) + # data from indust_sal_l
  
  annotate(geom = "text", label = 
             "Computing & Tech", x = 6.6 , y = 121200, hjust = 1, lineheight = 0.9,  family = "Roboto", size = 3.25) + # data from indust_sal_l
  
  annotate(geom = "text", label =
             "Median annual salary
comparison: holding a
Master's degree (a) vs.
a High School diploma (b)", x = 8.75 , y = 40000, hjust = 0, lineheight = 0.9,  family = "Roboto",  size = 3.25)+
  
  annotate(geom = "text", label = num_labels$labels, x = num_labels$x, y = num_labels$y, family = "Roboto", size = 3) +
  
  annotate(geom = "text", label = let_labels$labels, x = let_labels$x, y = let_labels$y, family = "Roboto", size = 3) +
  
  #legend
  annotate(geom = "text", x = 13, y = 120000, hjust = 0, lineheight = 0.9,size = 3, family = "Roboto", label = 
             "1. Retail
2. Agriculture
3. Art & Design
4. HR
5. Marketing
6. Insurance
7. Consulting
8. Engineering
9. Law")



#____________SAVING____________#
ggsave(p, file="TidyTuesdaySalaries2021_w21.png", device="png",dpi = 600, width = 19, height = 28, units = "cm") # can adjust resolution



# ALT TEXT  ---------------------------------------------------
# suggestion for the ORIGINAL WORK from https://github.com/sharlagelfand/RecreationThursday/blob/main/2021-06-03/alt_text.md
# Alfredo Hlito, Curves and Straight Lines (1948).
# An oil painting of colorful geometric lines on a cream background. Starting at the top of the painting in the middle there is a dark yellow curve. Intersecting it in the center of the painting is a black line at a 45 degree angle and a dark blue curve. In the top right of the center area there are short horizontal black blue green and red lines. In the top left of the center area are horizontal green red blue and black lines. In the bottom left of the center there is a short horizontal red line.
# To us, the placement of these lines conveys a sense of balance and free movement. This in the spirit of Hlito's placement in art history, as one who rejected figurativism and celebrated abstraction and geometry.


# suggestion for the PLOT
# An Abstract Look at the Annual Salaries Reported in Ask A Manager's Survey. A focus on Industry & Education
# A plot imitating Alfredo Hlito's Curves and Straight Lines (1948) that contains information on salaries.
# Geometric lines all appear on a cream background and the Y axis represents the median annual salary in USD.
# Starting at the top of the plot in the middle there is a yellow curve. Intersecting it in the center is a black line at an approximately 45 degree angle and a dark blue curve indicating that this line shows the differences in salary between individuals with a Master's degree versus a High School certificate. 
# In the top right of the center area there are short horizontal black blue green and red lines. In the top left of the center area are horizontal green red blue and black lines. 
# In the bottom left of the center there is a short horizontal red line. These short lines represent the median salaries for 9 industries.

