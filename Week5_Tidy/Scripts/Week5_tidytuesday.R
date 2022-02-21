################################################################
### TidyTuesday for Week5 
### Created by: Joynaline Hart
### Created on: 2022-02-22
############################################################### 

### Load libraries #####
# Always done first, and always be in the line to run the function
library(tidyverse) 
library(here) 


### Load data #####
starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')
glimpse (starbucks) #looking at what kind of data we have to analyze

### GGPLOT LINE #####
starbucks %>% #calling the dataframe
  #filtering the things we do not want from the size column
  filter (size != "1 scoop", 
          size != "1 shot", 
          size != "doppio",
          size != "solo",
          size != "trenta",
          size != "quad", 
          size != "triple") %>% 
  #data should be greater than 1, any 0's will be excluded
  filter (total_fat_g>1, 
          total_carbs_g>1) %>% 
  mutate (size = factor (size, levels = c ("short", "tall", "grande", "venti"))) %>% #rearrange the column on how I want it to look like
  ggplot (aes (x = total_fat_g, #stating what my x-axis will be composed of
               y = total_carbs_g, #stating what my y-axis will be composed of
               size = size)) + #setting size (lower value means small in size, higher value means bigger in size)
  geom_jitter(alpha=0.2, #setting the transparency of the jitterplot
              color = "darkgreen") + #setting the color of the jitterplot
  geom_smooth(size = 2,   #setting the size of the smooth line
              method ="lm", #setting the method of the smooth line
              color = "darkred") + #setting the color of the smooth line
  theme_bw() + #theme is black and white
  theme (legend.position = "none") + #excluding the legend
  theme(strip.text = element_text(color = "darkred", #manipulating how the strip text's color 
                                  hjust = 0.5, #adjusting the strip text's distance -- setting it to center
                                  size = 15),  #setting the size 
        text = element_text (family = "serif",
                             face = "bold",
                             size = 14),
        plot.caption = element_text(color = "darkgray", #setting the caption's color to darkgray 
                                    size = 11, #setting the size to 11 
                                    face = "italic", #setting the plot caption to italic to be different from others
                                    hjust = 0.5)) + #setting the arrangement to the middle
  labs(title = "Starbucks: Positive relationship regardless of the drink's size",  #telling what the title is and what the plot is all about
       caption = "Source: TidyTuesday from GitHub (https://github.com/rfordatascience/tidytuesday#datasets)") + #adding captions so that people can access them
  xlab ("Total Fat (g)") + #what the x axis is all about
  scale_x_continuous(breaks = c (5, 10, 15, 20, 25)) + #setting the specific outline on what goes to the x-axis
  ylab ("Total Carbohydrate (g)") + #what the y axis is all about
  facet_grid (. ~ size) #help to visualize properly in term's of the starbuck's sizes
  

ggsave(here("Week5_Tidy", "Output", "Week5_tidytuesday_Starbucks.png"), #saving the plot
       width=10, height=5) #setting the size of my plot
