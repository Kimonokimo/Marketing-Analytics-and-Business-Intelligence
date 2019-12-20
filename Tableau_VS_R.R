# bus240viz2
# data intake and wrangling for assignment #2
library(tidyverse)
library(stringr)
library(lubridate)
library(scales)
library(grid)

### read the WDI data from disk--modify next lines for your R installation.
# set YOUR working directory
setwd("~/Desktop")
# change if needed to your directory structure
wi <- read_csv("World Indicators.csv")
#######
wi2 <- select(wi, c("BirthRate", "LifeExpectancy", "InfantMortalityRate", "Region", "Year"))
wi2 <- filter(wi2, Year > "12/1/2003" & Year < "12/1/2011")
glimpse(wi2)

#####  Need to strip out '%' from 2 of the columns
wi2$birth <- str_sub(wi2$`BirthRate`, 1, str_length(wi2$`BirthRate`)-1)
wi2$birth <- as.numeric(wi2$birth)
wi2$im <- str_sub(wi2$`InfantMortalityRate`, 1, str_length(wi2$`InfantMortalityRate`)-1)
wi2$im <- as.numeric(wi2$im)
wi2$life <- as.numeric(wi2$`LifeExpectancy`)

##### Convert Year to numeric year
wi2$Yr <- year(as.Date.character(wi2$Year, "%m/%d/%Y"))
wi2$Region <- as.factor(wi2$Region)

###############  Now summarize average rates by region
wi3 <- wi2 %>%
  group_by(Region, Yr) %>%
  summarize(birthrate = mean(birth, na.rm = T),
            life_exp = mean(life, na.rm = T), 
            infant = mean(im, na.rm = T))

###########  NOW make your plots

# Divide bithrate and infant by 100 to show them in percentage format
wi3 <- wi3 %>% mutate(birthrate = birthrate/100, infant = infant/100)

# Convert wi3 into a data frame
wi3 <- as.data.frame(wi3)

# Create two new color palettes
red <- colorRampPalette(c("#FFBEB2", "#AE123A"))
blue <- colorRampPalette(c("#B9DDF1", "#2A5783"))

p1 <- wi3 %>% ggplot(aes(x = Yr, y = birthrate, fill = factor(birthrate))) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.6) +
  facet_wrap(.~Region, nrow = 1) +
  theme_minimal(base_family = "Times New Roman") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
  labs(x = NULL, y = "Birth Rate") +
  ggtitle("Trends in Birth Rate, Life Expectancy, Infant Mortality Rate Among Six Regions from 2004 to 2010") +
  scale_y_continuous(limits = c(0, 0.038), labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = red(41)) +
  scale_x_continuous(breaks = NULL) +
  theme(panel.grid.minor.y = element_blank())

p2 <- wi3 %>% ggplot(aes(x = Yr, y = infant, fill = factor(infant))) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.6) +
  facet_wrap(.~Region, nrow = 1, strip.position = NULL) +
  theme_minimal(base_family = "Times New Roman") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none", strip.text.x = element_blank()) +
  labs(x = NULL, y = "Infant Mortality Rate") +
  scale_y_continuous(limits = c(0, 0.075), labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = blue(42)) +
  scale_x_continuous(breaks = NULL) +
  theme(panel.grid.minor.y = element_blank())

p3 <- wi3 %>% ggplot(aes(x = Yr, y = life_exp)) +
  geom_step(group = 1, color = "#686868") +
  facet_wrap(.~Region, nrow = 1, strip.position = NULL) +
  theme_minimal(base_family = "Times New Roman") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none", strip.text.x = element_blank()) +
  labs(x = NULL, y = "Life Expectancy") +
  scale_y_continuous(limits = c(50, 80)) +
  scale_x_continuous(breaks = c(2004, 2005, 2006, 2007, 2008, 2009, 2010)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}  
grid.newpage()  #create a new grid for the plots
pushViewport(viewport(layout = grid.layout(3,1))) #change the composition of the grid
print(p1, vp = vplayout(1,1))
print(p2, vp = vplayout(2,1))
print(p3, vp = vplayout(3,1))