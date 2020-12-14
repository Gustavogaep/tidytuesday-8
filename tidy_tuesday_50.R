### My fist contribution to #TidyTuesday!
### project 50 (2020): Women of 2020
### Theresa Hügel, Dec 11th, 2020

### load packages
#install.packages("tidytuesdayR")

library(rvest)
library(tidyverse)
library(ggalluvial)
library(extrafont)
loadfonts(device = "win")

# get data
women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

# clean and prepare data
# Load web page
bbc_women <- html("https://www.bbc.co.uk/news/world-55042935")

# Save even and odd indices for data extraction later
odd_index <- seq(1,200,2)
even_index <- seq(2,200,2)

# Extract name
name <- bbc_women %>% 
  html_nodes("article h4") %>% 
  html_text()

# Extract image
img <- bbc_women %>% 
  html_nodes(".card__header") %>% 
  html_nodes("img") %>% 
  html_attr("src")

img <- img[odd_index]

# Extract category
category <- bbc_women %>% 
  html_nodes("article .card") %>% 
  str_extract("card category--[A-Z][a-z]+") %>% 
  str_remove_all("card category--")

# Extract country & role
country_role <- bbc_women %>% 
  html_nodes(".card__header__strapline__location") %>% 
  html_text()

country <- country_role[odd_index]
role <- country_role[even_index]

# Extract description
description <- bbc_women %>% 
  html_nodes(".first_paragraph") %>% 
  html_text()

# Finalise data frame
df <- data.frame(
  name,
  img,
  category,
  country,
  role,
  description
)


### further data preperation
# for illustration purpose delete "unsung hero"
df = df[-1,]

# adding "frequency" to data frame
df$freq <- rep(1,dim(df)[1])

# spread data
df_alt <- gather(df, "yaxis", "value", 3:5)
df_alt$yaxis <- as.factor(df_alt$yaxis)
df_alt$yaxis <- factor(df_alt$yaxis, levels= c( "category", "country","role" ))
df_alt$value <- as.factor(df_alt$value)
df_alt$subject <- rep(1:99,3) 
df_alt$fill_cat <- rep(df_alt$value[1:99],3)

### clean data 
# delete space after "India "
df_alt$value[which(df_alt$value=="India ")] <-"India"

# delete space after "UK "
df_alt$value[which(df_alt$value=="UK ")] <-"UK"

# delete "-" in "human-rights activist"
df_alt$value[which(df_alt$value=="Human-rights activist")] <-"Human rights activist"

# delete "-" in "Social-justice activist"
df_alt$value[which(df_alt$value=="Social-justice activist")] <-"Social justice activist"

# shortening "Exiled Uighur from Ghulja (in Chinese, Yining)"
df_alt$value <- as.character(df_alt$value)
df_alt$value[which(df_alt$value=="Exiled Uighur from Ghulja (in Chinese, Yining)")] <- "Uyghur"
df_alt$value <- as.factor(df_alt$value)

# shortening "Minister for Advanced Technologies"
df_alt$value <- as.character(df_alt$value)
df_alt$value[which(df_alt$value=="Minister for Advanced Technologies")] <- "Minister for Adv. Technologies"
df_alt$value <- as.factor(df_alt$value)

# shortening "Parliamentary Under Secretary of State"
df_alt$value <- as.character(df_alt$value)
df_alt$value[which(df_alt$value=="Parliamentary Under Secretary of State")] <- "Parl. Under Secretary of State"
df_alt$value <- as.factor(df_alt$value)

# shortening "UN Women ambassador/model"
df_alt$value <- as.character(df_alt$value)
df_alt$value[which(df_alt$value=="UN Women ambassador/model")] <- "UN Women amb./model"
df_alt$value <- as.factor(df_alt$value)



## Alluvial plot
# prepare vectors plot design
cat_len<- length(unique(df_alt$value[which(df_alt$yaxis == "category")]))
cou_len<- length(unique(df_alt$value[which(df_alt$yaxis == "country")]))
rol_len<- length(unique(df_alt$value[which(df_alt$yaxis == "role")]))

# angle of text
angle_vec <- c(rep(90,cat_len), rep(0, cou_len), rep(0, rol_len))

# alpha for strata
alpha_vec <- c( rep(0.7,cat_len), rep(0.7, cou_len), rep(0.7, rol_len))

# colours for strata
fill_vec <- c("#EFD9CE", "#07BEB8","#98DFEA", "#AF46A3", rep("#464B72", cou_len), rep("#464B72", rol_len))

# colours for text in strata
colour_vec <- c(rep("#000000",cat_len), rep("#A6AAC9", cou_len), rep("#A6AAC9", rol_len))

# size for text in strata
size_vec <- c( rep(5,cat_len), rep(2.8, cou_len), rep(2.8, rol_len))

# widths of strata
width_vec <- c(rep(0.25,cat_len), rep(0.4, cou_len), rep(0.55, rol_len))

# widths of flows
width_vec2 <- c(rep(0.25,82),rep(0.4,82), rep(0.4, 96), rep(0.55, 96))


ggplot(df_alt,
       aes(x=yaxis, stratum=value, alluvium = subject , y=freq, label = value,
           fill = fill_cat)) +
  geom_flow(width = width_vec2, alpha=0.6) +
  geom_stratum(alpha = alpha_vec, width = width_vec, fill=fill_vec) +
  geom_text(stat = "stratum", angle=angle_vec, size = size_vec, colour=colour_vec) +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", color = "#E6E7EF", size = 10),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", size = 24, color = "#E6E7EF"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin = margin(0.7, 0.4, 0.4, 0.4, "cm"),
        plot.background = element_rect(fill = "#25283D")) +
  scale_y_continuous(expand = c(0.005, 0)) +
  scale_x_discrete(expand = c(0, 0), labels = c("CATEGORY","COUNTRY", "ROLE")) +
  labs(title="The BBC's 100 women of 2020",
       caption = "@diggingthedata | source: BBC") +
  scale_fill_manual(values = c("#AF46A3", "#98DFEA", "#07BEB8", "#EFD9CE")) +
  ggsave(file= "week50.png", 
         units = "cm",
         dpi = 300,
         width=22, height=28) 

