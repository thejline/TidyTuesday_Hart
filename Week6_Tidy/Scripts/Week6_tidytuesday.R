################################################################
### TidyTuesday for Week6 
### Created by: Joynaline Hart
### Created on: 2022-03-01
############################################################### 

### Load libraries #####
# Always done first, and always be in the line to run the function
library(tidyverse) 
library(here) 
library(lubridate)

### Load data ######
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv') %>% #reading the data
  select(Grad_date=graduation_date, Grad_rank=rank_at_graduation, Grad_from=graduated_from, pilot_type) %>% #selecting specific columns to be analyzed
  mutate (datetime = ymd(Grad_date), #setting the date specification
          Year=year(Grad_date), #extracting the year from datetime
          Month=month(Grad_date, label=TRUE, abbr=FALSE)) %>% #extracting month; spell it out than being abbreviated
  #summarise (Dateavg = mean(Grad_date, na.rm=TRUE)) %>%  #taking the date average
  write_csv(here("Week6_Tidy", "Output", "Week6_Tidytuesday_CleanData.csv")) #saving the result in a csv file

View(airmen) #viewing how the data is now

airmen %>% #calling the dataframe
  filter(Grad_from == "TAAF") %>% #only show the TAAF column/data
  mutate(pilot_type=fct_relevel(pilot_type, "Single engine", "Twin engine", "Liaison pilot", "Liason pilot"), #rearranging the pilot type on how it would look like
         highlight_this=ifelse(Year>1945, T, F)) %>% #show highlight for year after 1945
  filter (complete.cases(.)) %>% #filter the NA's row
  ggplot (aes(x=Year, #setting what goes to the x-axis
              y=Month)) + #setting what goes to the y-axis
  geom_jitter(size=3.5,shape=10, aes(color=highlight_this)) + #specifying the specific size, shape and color for the jitter plot
  scale_color_manual(values = c("azure4", "deeppink4")) + #this manipulates the highlight (higher than 1945 are TRUE)
  theme_bw() + #setting the theme for a black and white
  theme(text = element_text(size=14,        #changing the text's size; this applies to everything
                            family="serif"), 
        axis.title = element_text(size = 15), #changing the axis title to a different size
        axis.text = element_text(size = 11, #setting the axis text (both x and y) to a certain size
                                 color = "black"), #setting the axis text to a certain color
        plot.title = element_text(color = "deeppink4", #plot title's color is changed
                                  hjust = 0.5), #plot title's arrangement is adjusted to the center (0.5)
        plot.subtitle = element_text(color="black", #plot subtitle's color is changed
                                     size = 12, #size is change to 11 which is lower than the plot title 
                                     hjust = 0.5), #setting the plot's subtitle to 0.5 which is center
        plot.caption = element_text(color = "azure4", #setting the caption's color to darkgray 
                                    size = 11, #setting the size to 11 
                                    hjust = 0.001), #setting the arrangement to the middle
        strip.text = element_text(color = "cyan4", #manipulating how the strip text's color 
                                  hjust = 0.5, #adjusting the strip text's distance -- setting it to center
                                  size = 15),  #setting the size
        panel.background=element_rect(fill="aliceblue"), #changing the panel background to a specific color
        legend.position="none") + #removing the legend color
  labs(title="The Army Air Force: Airmen graduated by pilot type", #giving a title to the plot
       subtitle = "Highlighting the airmen after WWII (1945)", #adding some subtitle to further explain the plot
       x="Year (YYYY)", #giving a definition for the x-axis
       y = "Month", #giving a definition for the y-axis
       caption = "Source: TidyTuesday(https://github.com/rfordatascience/tidytuesday#datasets) | Designed by: Jline") + #adding captions so that people can access them
  scale_x_continuous(limits = c(1942,1946), #limiting what I can see on the x-axis 
                     breaks = c(1942, 1943, 1944, 1945, 1946)) + #manually changing how I want the range to look like
  facet_grid(pilot_type~Grad_from) #facetting by grid

ggsave(here("Week6_Tidy", "Output", "Week6_tidytuesday_TAAF.png"), #saving the plot
       width=10, height=10) #setting the size of my plot
