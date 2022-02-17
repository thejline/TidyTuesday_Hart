################################################################
### TidyTuesday for Week4 -- the codes were learned from Week3
### Created by: Joynaline Hart
### Created on: 2022-02-13
############################################################### 

### Load libraries #####
# Always done first, and always be in the line to run the function
library(tidyverse) 
library(here) 


### Load data #####
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')
glimpse (stressor)

### GGPLOT LINE #####
 ggplot(data = stressor, #calling out the dataframe
       mapping = aes(x = stress_pct,  #using the aesthetic function, setting the x
                     y = stressor)) +    #setting what goes to the y
  geom_boxplot(fill = "white", colour = "darkred") +  #creating a boxplot by changing the fill to white 
  theme_bw() +  #setting the theme to black and white
  theme(text = element_text(size=14,        #changing the text's size; this applies to everything
                            face = "bold"), #changing the text's font to bold; this applies to everything
        axis.title = element_text(size = 15, #changing the axis title to a different size
                                  color = "darkblue"), #changing the axis title's color to darkblue
        axis.text = element_text(size = 11, #setting the axis text (both x and y) to a certain size
                                 color = "black"), #setting the axis text to a certain color
        axis.line = element_line(color = "darkblue", #setting the axis line to darkblue in color -- give it a highlight
                                 size = 1, #setting the axis line to one which is thicker than the default
                                 linetype = "solid"), #setting the axis line to solid for clarification
        plot.title = element_text(color = "darkred", #plot title's color is changed
                                  hjust = 0.5), #plot title's arrangement is adjusted to the center (0.5)
        plot.subtitle = element_text(color="black", #plot subtitle's color is changed
                                     size = 11, #size is change to 11 which is lower than the plot title 
                                     hjust = 0.5), #setting the plot's subtitle to 0.5 which is center
        plot.caption = element_text(color = "darkgray", #setting the caption's color to darkgray 
                                    size = 11, #setting the size to 11 
                                    face = "italic", #setting the plot caption to italic to be different from others
                                    hjust = 0.5)) + #setting the arrangement to the middle
  labs(title = "Bee Colony",  #telling what the title is and what the plot is all about
       subtitle = "Studying stressors quarterly", #adding some subtitle to further explain the plot
       x = "colonies affected by stressors (%)",  #adding what the x-axis is all about
       y = "Stressors", #adding what the y-axis is all about
       caption = "Source: TidyTuesday from GitHub (https://github.com/rfordatascience/tidytuesday#datasets)") + #adding captions so that people can access them
  guides (fill=FALSE) +  #removing the legend on the side
  scale_x_continuous(limits = c(0,100), #limiting what I can see on the x-axis 
                     breaks = c(0, 20, 40, 60, 80, 100)) + #manually changing how I want the range to look like
  facet_wrap(~months, ncol=1) #wrapping the column to only 1



ggsave(here("tidytuesday","Week4_tidytuesday_Bee_colony.png"), #saving the plot
       width=10, height=10) #setting the size of my plot
